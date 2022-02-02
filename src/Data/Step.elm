module Data.Step exposing
    ( Label(..)
    , Step
    , airTransportRatioToString
    , computeTransports
    , create
    , displayLabel
    , dyeingWeightingToString
    , encode
    , getCountryElectricityProcess
    , getStepGitbookPath
    , initMass
    , labelToString
    , qualityToString
    , updateFromInputs
    , updateWaste
    )

import Data.Country as Country exposing (Country)
import Data.Db exposing (Db)
import Data.Formula as Formula
import Data.Gitbook as Gitbook exposing (Path(..))
import Data.Impact as Impact exposing (Impacts)
import Data.Inputs exposing (Inputs)
import Data.Process as Process exposing (Process)
import Data.Transport as Transport exposing (Transport)
import Data.Unit as Unit
import Energy exposing (Energy)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), frenchLocale)
import Json.Encode as Encode
import Mass exposing (Mass)
import Quantity


type alias Step =
    { label : Label
    , country : Country
    , editable : Bool
    , inputMass : Mass
    , outputMass : Mass
    , waste : Mass
    , transport : Transport
    , impacts : Impacts
    , heat : Energy
    , kwh : Energy
    , processInfo : ProcessInfo
    , dyeingWeighting : Unit.Ratio -- FIXME: why not Maybe?
    , airTransportRatio : Unit.Ratio -- FIXME: why not Maybe?
    , customCountryMix : Maybe Unit.Impact
    , quality : Unit.Quality
    }


type alias ProcessInfo =
    { countryElec : Maybe String
    , countryHeat : Maybe String
    , dyeingWeighting : Maybe String
    , airTransportRatio : Maybe String
    , airTransport : Maybe String
    , seaTransport : Maybe String
    , roadTransport : Maybe String
    , useIroning : Maybe String
    , useNonIroning : Maybe String
    , passengerCar : Maybe String
    , endOfLife : Maybe String
    , knittingWeaving : Maybe String
    , distribution : Maybe String
    }


type Label
    = MaterialAndSpinning -- Matière & Filature
    | WeavingKnitting -- Tissage & Tricotage
    | Ennoblement -- Ennoblement
    | Making -- Confection
    | Distribution -- Distribution
    | Use -- Utilisation
    | EndOfLife -- Fin de vie


create : { db : Db, label : Label, editable : Bool, country : Country } -> Step
create { db, label, editable, country } =
    let
        defaultImpacts =
            Impact.impactsFromDefinitons db.impacts
    in
    { label = label
    , country = country
    , editable = editable
    , inputMass = Quantity.zero
    , outputMass = Quantity.zero
    , waste = Quantity.zero
    , transport = Transport.default defaultImpacts
    , impacts = defaultImpacts
    , heat = Quantity.zero
    , kwh = Quantity.zero
    , processInfo = defaultProcessInfo
    , dyeingWeighting = country.dyeingWeighting
    , airTransportRatio = Unit.ratio 0 -- Note: this depends on next step country, so we can't set an accurate default value initially
    , customCountryMix = Nothing
    , quality = Unit.standardQuality
    }


defaultProcessInfo : ProcessInfo
defaultProcessInfo =
    { countryElec = Nothing
    , countryHeat = Nothing
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , airTransport = Nothing
    , seaTransport = Nothing
    , roadTransport = Nothing
    , useIroning = Nothing
    , useNonIroning = Nothing
    , passengerCar = Nothing
    , endOfLife = Nothing
    , knittingWeaving = Nothing
    , distribution = Nothing
    }


displayLabel : { knitted : Bool } -> Label -> String
displayLabel { knitted } label =
    case ( label, knitted ) of
        ( WeavingKnitting, True ) ->
            "Tricotage"

        ( WeavingKnitting, False ) ->
            "Tissage"

        _ ->
            labelToString label


getCountryElectricityProcess : Step -> Process
getCountryElectricityProcess { country, customCountryMix } =
    let
        { electricityProcess } =
            country
    in
    case customCountryMix of
        Just mix ->
            electricityProcess
                |> Process.updateImpact (Impact.trg "cch") mix

        Nothing ->
            electricityProcess


countryMixToString : Unit.Impact -> String
countryMixToString =
    Unit.impactToFloat
        >> FormatNumber.format { frenchLocale | decimals = Exact 3 }
        >> (\kgCo2e -> "Mix électrique personnalisé: " ++ kgCo2e ++ "\u{202F}kgCO₂e/kWh")


{-| Computes step transport distances and impact regarding next step.

Docs: <https://fabrique-numerique.gitbook.io/wikicarbone/methodologie/transport>

-}
computeTransports : Db -> Step -> Step -> Result String Step
computeTransports db next ({ processInfo } as current) =
    db.processes
        |> Process.loadWellKnown
        |> Result.map
            (\wellKnown ->
                let
                    transport =
                        db.transports
                            |> Transport.getTransportBetween
                                current.transport.impacts
                                current.country.code
                                next.country.code

                    stepSummary =
                        computeTransportSummary current transport

                    roadTransportProcess =
                        getRoadTransportProcess wellKnown current
                in
                { current
                    | processInfo =
                        { processInfo
                            | roadTransport = Just roadTransportProcess.name
                            , seaTransport = Just wellKnown.seaTransport.name
                            , airTransport = Just wellKnown.airTransport.name
                        }
                    , transport =
                        stepSummary
                            |> computeTransportImpacts db
                                current.transport.impacts
                                wellKnown
                                roadTransportProcess
                                next.inputMass
                }
            )


computeTransportImpacts : Db -> Impacts -> Process.WellKnown -> Process -> Mass -> Transport -> Transport
computeTransportImpacts db impacts { seaTransport, airTransport } roadProcess mass { road, sea, air } =
    { road = road
    , sea = sea
    , air = air
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    let
                        ( roadImpact, seaImpact, airImpact ) =
                            ( mass |> Unit.forKgAndDistance (Process.getImpact trigram roadProcess) road
                            , mass |> Unit.forKgAndDistance (Process.getImpact trigram seaTransport) sea
                            , mass |> Unit.forKgAndDistance (Process.getImpact trigram airTransport) air
                            )
                    in
                    Quantity.sum [ roadImpact, seaImpact, airImpact ]
                )
            |> Impact.updatePefImpact db.impacts
    }


computeTransportSummary : Step -> Transport -> Transport
computeTransportSummary step transport =
    let
        ( noTransports, defaultInland ) =
            ( Transport.default step.transport.impacts
            , Transport.defaultInland step.transport.impacts
            )
    in
    case step.label of
        Ennoblement ->
            transport
                -- Note: no air transport ratio at the Ennoblement step
                |> Formula.transportRatio (Unit.ratio 0)
                -- Added intermediary inland transport distances to materialize
                -- "processing" + "dyeing" steps (see Excel)
                -- Also ensure we don't add unnecessary air transport
                |> Transport.add { defaultInland | air = Quantity.zero }

        Making ->
            -- Air transport only applies between the Making and the Distribution steps
            transport
                |> Formula.transportRatio step.airTransportRatio

        Use ->
            -- Product Use leverages no transports
            noTransports

        EndOfLife ->
            -- End of life leverages no transports
            noTransports

        _ ->
            -- All other steps don't use air transport, force a 0 ratio
            transport
                |> Formula.transportRatio (Unit.ratio 0)


getRoadTransportProcess : Process.WellKnown -> Step -> Process
getRoadTransportProcess wellKnown { label } =
    case label of
        Making ->
            wellKnown.roadTransportPostMaking

        Distribution ->
            wellKnown.distribution

        _ ->
            wellKnown.roadTransportPreMaking


updateFromInputs : Db -> Inputs -> Step -> Step
updateFromInputs { processes } inputs ({ label, country } as step) =
    let
        { dyeingWeighting, airTransportRatio, customCountryMixes, quality } =
            inputs

        countryElecInfo =
            Maybe.map countryMixToString
                >> Maybe.withDefault country.electricityProcess.name
                >> Just
    in
    -- Note: only WeavingKnitting, Ennoblement, Making and Use steps render detailed processes info.
    case label of
        WeavingKnitting ->
            { step
                | customCountryMix = customCountryMixes.fabric
                , processInfo =
                    { defaultProcessInfo
                        | countryElec = countryElecInfo customCountryMixes.fabric
                        , knittingWeaving = Just inputs.product.fabricProcess.name
                    }
            }

        Ennoblement ->
            { step
                | customCountryMix = customCountryMixes.dyeing
                , dyeingWeighting =
                    dyeingWeighting |> Maybe.withDefault country.dyeingWeighting
                , processInfo =
                    { defaultProcessInfo
                        | countryHeat = Just country.heatProcess.name
                        , countryElec = countryElecInfo customCountryMixes.dyeing
                        , dyeingWeighting = Just (dyeingWeightingToString country.dyeingWeighting)
                    }
            }

        Making ->
            { step
                | customCountryMix = customCountryMixes.making
                , airTransportRatio =
                    airTransportRatio |> Maybe.withDefault country.airTransportRatio
                , processInfo =
                    { defaultProcessInfo
                        | countryElec = countryElecInfo customCountryMixes.making
                        , airTransportRatio =
                            country.airTransportRatio
                                |> airTransportRatioToString
                                |> Just
                    }
            }

        Distribution ->
            processes
                |> Process.loadWellKnown
                |> Result.map
                    (\{ distribution } ->
                        { step
                            | processInfo =
                                { defaultProcessInfo | distribution = Just distribution.name }
                        }
                    )
                |> Result.withDefault step

        Use ->
            { step
                | quality =
                    quality |> Maybe.withDefault Unit.standardQuality
                , processInfo =
                    { defaultProcessInfo
                        | countryElec = Just country.electricityProcess.name
                        , useIroning = Just inputs.product.useIroningProcess.name
                        , useNonIroning = Just inputs.product.useNonIroningProcess.name
                    }
            }

        EndOfLife ->
            let
                newProcessInfo =
                    { defaultProcessInfo
                        | countryElec = Just country.electricityProcess.name
                        , countryHeat = Just country.heatProcess.name
                    }
            in
            processes
                |> Process.loadWellKnown
                |> Result.map
                    (\{ endOfLife } ->
                        { step
                            | processInfo =
                                { newProcessInfo
                                    | passengerCar = Just "Transport en voiture vers point de collecte (1km)"
                                    , endOfLife = Just endOfLife.name
                                }
                        }
                    )
                |> Result.withDefault { step | processInfo = newProcessInfo }

        _ ->
            step


initMass : Mass -> Step -> Step
initMass mass step =
    { step
        | inputMass = mass
        , outputMass = mass
    }


updateWaste : Mass -> Mass -> Step -> Step
updateWaste waste mass step =
    { step
        | waste = waste
        , inputMass = mass
        , outputMass = Quantity.difference mass waste
    }


airTransportRatioToString : Unit.Ratio -> String
airTransportRatioToString (Unit.Ratio airTransportRatio) =
    case round (airTransportRatio * 100) of
        0 ->
            "Aucun transport aérien"

        p ->
            String.fromInt p ++ "% de transport aérien"


dyeingWeightingToString : Unit.Ratio -> String
dyeingWeightingToString (Unit.Ratio dyeingWeighting) =
    case round (dyeingWeighting * 100) of
        0 ->
            "Procédé représentatif"

        p ->
            "Procédé " ++ String.fromInt p ++ "% majorant"


qualityToString : Unit.Quality -> String
qualityToString (Unit.Quality float) =
    "Qualité intrinsèque\u{00A0}: " ++ String.fromFloat float


encode : Step -> Encode.Value
encode v =
    Encode.object
        [ ( "label", Encode.string (labelToString v.label) )
        , ( "country", Country.encode v.country )
        , ( "editable", Encode.bool v.editable )
        , ( "inputMass", Encode.float (Mass.inKilograms v.inputMass) )
        , ( "outputMass", Encode.float (Mass.inKilograms v.outputMass) )
        , ( "waste", Encode.float (Mass.inKilograms v.waste) )
        , ( "transport", Transport.encode v.transport )
        , ( "impacts", Impact.encodeImpacts v.impacts )
        , ( "heat", Encode.float (Energy.inMegajoules v.heat) )
        , ( "kwh", Encode.float (Energy.inKilowattHours v.kwh) )
        , ( "processInfo", encodeProcessInfo v.processInfo )
        , ( "dyeingWeighting", Unit.encodeRatio v.dyeingWeighting )
        , ( "airTransportRatio", Unit.encodeRatio v.airTransportRatio )
        , ( "customCountryMix", v.customCountryMix |> Maybe.map Unit.encodeImpact |> Maybe.withDefault Encode.null )
        , ( "quality", Unit.encodeQuality v.quality )
        ]


encodeProcessInfo : ProcessInfo -> Encode.Value
encodeProcessInfo v =
    let
        encodeMaybeString =
            Maybe.map Encode.string >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "countryElec", encodeMaybeString v.countryElec )
        , ( "countryHeat", encodeMaybeString v.countryHeat )
        , ( "dyeingWeighting", encodeMaybeString v.dyeingWeighting )
        , ( "airTransportRatio", encodeMaybeString v.airTransportRatio )
        , ( "airTransport", encodeMaybeString v.airTransport )
        , ( "seaTransport", encodeMaybeString v.seaTransport )
        , ( "roadTransport", encodeMaybeString v.roadTransport )
        , ( "useIroning", encodeMaybeString v.useIroning )
        , ( "useNonIroning", encodeMaybeString v.useNonIroning )
        , ( "passengerCar", encodeMaybeString v.passengerCar )
        , ( "endOfLife", encodeMaybeString v.endOfLife )
        , ( "knittingWeaving", encodeMaybeString v.knittingWeaving )
        , ( "distribution", encodeMaybeString v.distribution )
        ]


labelToString : Label -> String
labelToString label =
    case label of
        MaterialAndSpinning ->
            "Matière & Filature"

        WeavingKnitting ->
            "Tissage & Tricotage"

        Making ->
            "Confection"

        Ennoblement ->
            "Teinture"

        Distribution ->
            "Distribution"

        Use ->
            "Utilisation"

        EndOfLife ->
            "Fin de vie"


getStepGitbookPath : Label -> Gitbook.Path
getStepGitbookPath label =
    case label of
        MaterialAndSpinning ->
            Gitbook.MaterialAndSpinning

        WeavingKnitting ->
            Gitbook.WeavingKnitting

        Ennoblement ->
            Gitbook.Dyeing

        Making ->
            Gitbook.Making

        Distribution ->
            Gitbook.Distribution

        Use ->
            Gitbook.Use

        EndOfLife ->
            Gitbook.EndOfLife

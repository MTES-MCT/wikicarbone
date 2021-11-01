module Data.Step exposing (..)

import Data.Co2 as Co2 exposing (Co2e)
import Data.Country as Country exposing (Country)
import Data.Db exposing (Db)
import Data.Formula as Formula
import Data.Gitbook as Gitbook exposing (Path(..))
import Data.Inputs exposing (Inputs)
import Data.Process as Process exposing (Process)
import Data.Transport as Transport exposing (Transport, defaultInland, defaultSummary)
import Energy exposing (Energy)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Json.Encode as Encode
import Mass exposing (Mass)
import Quantity


type alias Step =
    { label : Label
    , country : Country
    , editable : Bool
    , mass : Mass
    , waste : Mass
    , transport : Transport.Summary
    , co2 : Co2e
    , heat : Energy
    , kwh : Energy
    , processInfo : ProcessInfo
    , dyeingWeighting : Float
    , airTransportRatio : Float
    }


type alias ProcessInfo =
    { electricity : Maybe String
    , heat : Maybe String
    , dyeingWeighting : Maybe String
    , airTransportRatio : Maybe String
    }


type Label
    = MaterialAndSpinning -- Matière & Filature
    | WeavingKnitting -- Tissage & Tricotage
    | Ennoblement -- Ennoblement
    | Making -- Confection
    | Distribution -- Distribution


create : Label -> Bool -> Country -> Step
create label editable country =
    { label = label
    , country = country
    , editable = editable
    , mass = Mass.kilograms 0
    , waste = Mass.kilograms 0
    , transport = defaultSummary
    , co2 = Quantity.zero
    , heat = Energy.megajoules 0
    , kwh = Energy.kilowattHours 0
    , processInfo = defaultProcessInfo
    , dyeingWeighting = country.dyeingWeighting
    , airTransportRatio = 0 -- Note: this depends on next step country, so we can't set an accurate default value initially
    }


defaultProcessInfo : ProcessInfo
defaultProcessInfo =
    { electricity = Nothing
    , heat = Nothing
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    }


processCountryInfo : Label -> Country -> ProcessInfo
processCountryInfo label country =
    case label of
        WeavingKnitting ->
            { defaultProcessInfo | electricity = Just country.electricity.name }

        Ennoblement ->
            { defaultProcessInfo
                | heat = Just country.heat.name
                , electricity = Just country.electricity.name
                , dyeingWeighting = Just (dyeingWeightingToString country.dyeingWeighting)
            }

        Making ->
            { defaultProcessInfo
                | electricity = Just country.electricity.name
                , airTransportRatio =
                    country.airTransportRatio
                        |> airTransportRatioToString
                        |> Just
            }

        _ ->
            defaultProcessInfo


{-| Computes step transport distances and co2 scores regarding next step.

Docs: <https://fabrique-numerique.gitbook.io/wikicarbone/methodologie/transport>

-}
computeTransports : Db -> Step -> Step -> Result String Step
computeTransports db next current =
    db.processes
        |> Process.loadWellKnown
        |> Result.map
            (\wellKnown ->
                let
                    transport =
                        db.transports
                            |> Transport.getTransportBetween current.country.code next.country.code

                    stepSummary =
                        computeTransportSummary current transport
                            |> Formula.transportRatio current.airTransportRatio

                    roadTransportProcess =
                        getRoadTransportProcess wellKnown current
                in
                { current
                    | transport =
                        stepSummary
                            |> computeTransportCo2 wellKnown roadTransportProcess next.mass
                            |> Transport.addSummary (initialTransportSummary wellKnown current)
                }
            )


computeTransportCo2 : Process.WellKnown -> Process -> Mass -> Transport -> Transport.Summary
computeTransportCo2 { seaTransport, airTransport } roadProcess mass { road, sea, air } =
    let
        ( roadCo2, seaCo2, airCo2 ) =
            ( mass |> Co2.co2eForMassAndDistance roadProcess.climateChange road
            , mass |> Co2.co2eForMassAndDistance seaTransport.climateChange sea
            , mass |> Co2.co2eForMassAndDistance airTransport.climateChange air
            )
    in
    { road = road
    , sea = sea
    , air = air
    , co2 = Quantity.sum [ roadCo2, seaCo2, airCo2 ]
    }


initialTransportSummary : Process.WellKnown -> Step -> Transport.Summary
initialTransportSummary wellKnown { label, mass } =
    case label of
        MaterialAndSpinning ->
            -- Apply initial Material to Spinning step transport data (see Excel)
            Transport.materialToSpinningTransport
                |> computeTransportCo2 wellKnown wellKnown.roadTransportPreMaking mass

        _ ->
            defaultSummary


computeTransportSummary : Step -> Transport -> Transport.Summary
computeTransportSummary step transport =
    case step.label of
        Ennoblement ->
            -- Added intermediary defaultInland transport step to materialize
            -- Processing + Dyeing steps (see Excel)
            { defaultSummary
                | road = transport.road |> Quantity.plus defaultInland.road
                , sea = transport.sea |> Quantity.plus defaultInland.sea
            }

        Making ->
            -- Air transport only applies between the Making and the Distribution steps
            { defaultSummary
                | road = transport.road
                , sea = transport.sea
                , air = transport.air
            }

        _ ->
            -- All other steps don't use air transport at all
            { defaultSummary
                | road = transport.road
                , sea = transport.sea
            }


getRoadTransportProcess : Process.WellKnown -> Step -> Process
getRoadTransportProcess wellKnown { label } =
    case label of
        Making ->
            wellKnown.roadTransportPostMaking

        Distribution ->
            wellKnown.distribution

        _ ->
            wellKnown.roadTransportPreMaking


update : Inputs -> Maybe Step -> Step -> Step
update { dyeingWeighting, airTransportRatio } _ step =
    { step
        | processInfo =
            processCountryInfo step.label step.country
        , dyeingWeighting =
            if step.label == Ennoblement then
                dyeingWeighting |> Maybe.withDefault step.country.dyeingWeighting

            else
                step.dyeingWeighting
        , airTransportRatio =
            if step.label == Making then
                airTransportRatio |> Maybe.withDefault step.country.airTransportRatio

            else
                step.airTransportRatio
    }


airTransportRatioToString : Float -> String
airTransportRatioToString airTransportRatio =
    case round (airTransportRatio * 100) of
        0 ->
            "Aucun transport aérien"

        p ->
            String.fromInt p ++ "% de transport aérien"


dyeingWeightingToString : Float -> String
dyeingWeightingToString dyeingWeighting =
    case round (dyeingWeighting * 100) of
        0 ->
            "Procédé représentatif"

        p ->
            "Procédé " ++ String.fromInt p ++ "% majorant"


decodeLabel : Decoder Label
decodeLabel =
    Decode.string
        |> Decode.andThen
            (\label ->
                case labelFromString label of
                    Just decoded ->
                        Decode.succeed decoded

                    Nothing ->
                        Decode.fail ("Invalid step : " ++ label)
            )


encode : Step -> Encode.Value
encode v =
    Encode.object
        [ ( "label", Encode.string (labelToString v.label) )
        , ( "country", Country.encode v.country )
        , ( "editable", Encode.bool v.editable )
        , ( "mass", Encode.float (Mass.inKilograms v.mass) )
        , ( "waste", Encode.float (Mass.inKilograms v.waste) )
        , ( "transport", Transport.encodeSummary v.transport )
        , ( "co2", Co2.encodeKgCo2e v.co2 )
        , ( "heat", Encode.float (Energy.inMegajoules v.heat) )
        , ( "kwh", Encode.float (Energy.inKilowattHours v.kwh) )
        , ( "processInfo", encodeProcessInfo v.processInfo )
        , ( "dyeingWeighting", Encode.float v.dyeingWeighting )
        , ( "airTransportRatio", Encode.float v.airTransportRatio )
        ]


decodeProcessInfo : Decoder ProcessInfo
decodeProcessInfo =
    Decode.succeed ProcessInfo
        |> Pipe.required "electricity" (Decode.maybe Decode.string)
        |> Pipe.required "heat" (Decode.maybe Decode.string)
        |> Pipe.required "dyeingWeighting" (Decode.maybe Decode.string)
        |> Pipe.required "airTransportRatio" (Decode.maybe Decode.string)


encodeProcessInfo : ProcessInfo -> Encode.Value
encodeProcessInfo v =
    Encode.object
        [ ( "electricity", Maybe.map Encode.string v.electricity |> Maybe.withDefault Encode.null )
        , ( "heat", Maybe.map Encode.string v.heat |> Maybe.withDefault Encode.null )
        , ( "dyeing", Maybe.map Encode.string v.dyeingWeighting |> Maybe.withDefault Encode.null )
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


labelFromString : String -> Maybe Label
labelFromString label =
    case label of
        "Matière & Filature" ->
            Just MaterialAndSpinning

        "Tissage & Tricotage" ->
            Just WeavingKnitting

        "Confection" ->
            Just Making

        "Teinture" ->
            Just Ennoblement

        "Distribution" ->
            Just Distribution

        _ ->
            Nothing


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

module Data.Textile.Step exposing
    ( PreTreatments
    , Step
    , airTransportDisabled
    , airTransportRatioToString
    , computeMaterialTransportAndImpact
    , computePreTreatments
    , computeTransports
    , create
    , encode
    , getInputSurface
    , getOutputSurface
    , getTotalImpactsWithoutComplements
    , getTransportedMass
    , initMass
    , makingDeadStockToString
    , makingWasteToString
    , surfaceMassToString
    , updateDeadStock
    , updateFromInputs
    , updateWasteAndMasses
    , yarnSizeToString
    )

import Area exposing (Area)
import Data.Country as Country exposing (Country)
import Data.Impact as Impact exposing (Impacts)
import Data.Process as Process exposing (Process)
import Data.Split as Split exposing (Split)
import Data.Textile.Db as Textile
import Data.Textile.Dyeing exposing (ProcessType)
import Data.Textile.Fabric as Fabric
import Data.Textile.Formula as Formula
import Data.Textile.Inputs as Inputs exposing (Inputs)
import Data.Textile.MakingComplexity exposing (MakingComplexity)
import Data.Textile.Printing exposing (Printing)
import Data.Textile.Step.Label as Label exposing (Label)
import Data.Textile.WellKnown as WellKnown exposing (WellKnown)
import Data.Transport as Transport exposing (Transport)
import Data.Unit as Unit
import Energy exposing (Energy)
import Json.Encode as Encode
import Length
import List.Extra as LE
import Mass exposing (Mass)
import Quantity
import Static.Db exposing (Db)
import Views.Format as Format


type alias Step =
    { airTransportRatio : Split
    , complementsImpacts : Impact.ComplementsImpacts
    , country : Country
    , deadstock : Mass
    , durability : Unit.NonPhysicalDurability
    , dyeingProcessType : Maybe ProcessType
    , editable : Bool
    , enabled : Bool
    , heat : Energy
    , impacts : Impacts
    , inputMass : Mass
    , kwh : Energy
    , label : Label
    , makingComplexity : Maybe MakingComplexity
    , makingDeadStock : Maybe Split
    , makingWaste : Maybe Split
    , outputMass : Mass
    , picking : Maybe Unit.PickPerMeter
    , preTreatments : PreTreatments
    , printing : Maybe Printing
    , processInfo : ProcessInfo
    , surfaceMass : Maybe Unit.SurfaceMass
    , threadDensity : Maybe Unit.ThreadDensity
    , transport : Transport
    , waste : Mass
    , yarnSize : Maybe Unit.YarnSize
    }


type alias PreTreatments =
    { heat : Energy
    , impacts : Impacts
    , kwh : Energy
    , operations : List Process
    }


type alias ProcessInfo =
    { airTransport : Maybe String
    , airTransportRatio : Maybe String
    , countryElec : Maybe String
    , countryHeat : Maybe String
    , distribution : Maybe String
    , dyeing : Maybe String
    , endOfLife : Maybe String
    , fabric : Maybe String
    , fading : Maybe String
    , making : Maybe String
    , passengerCar : Maybe String
    , printing : Maybe String
    , roadTransport : Maybe String
    , seaTransport : Maybe String
    , useIroning : Maybe String
    , useNonIroning : Maybe String
    }


create : { country : Country, editable : Bool, enabled : Bool, label : Label } -> Step
create { country, editable, enabled, label } =
    let
        defaultImpacts =
            Impact.empty
    in
    { airTransportRatio = Split.zero -- Note: this depends on next step country, so we can't set an accurate default value initially
    , complementsImpacts = Impact.noComplementsImpacts
    , country = country
    , deadstock = Quantity.zero
    , durability = Unit.standardDurability Unit.NonPhysicalDurability
    , dyeingProcessType = Nothing
    , editable = editable
    , enabled = enabled
    , heat = Quantity.zero
    , impacts = defaultImpacts
    , inputMass = Quantity.zero
    , kwh = Quantity.zero
    , label = label
    , makingComplexity = Nothing
    , makingDeadStock = Nothing
    , makingWaste = Nothing
    , outputMass = Quantity.zero
    , picking = Nothing
    , preTreatments = emptyPreTreatments
    , printing = Nothing
    , processInfo = defaultProcessInfo
    , surfaceMass = Nothing
    , threadDensity = Nothing
    , transport = Transport.default defaultImpacts
    , waste = Quantity.zero
    , yarnSize = Nothing
    }


emptyPreTreatments : PreTreatments
emptyPreTreatments =
    { heat = Quantity.zero
    , impacts = Impact.empty
    , kwh = Quantity.zero
    , operations = []
    }


defaultProcessInfo : ProcessInfo
defaultProcessInfo =
    { airTransport = Nothing
    , airTransportRatio = Nothing
    , countryElec = Nothing
    , countryHeat = Nothing
    , distribution = Nothing
    , dyeing = Nothing
    , endOfLife = Nothing
    , fabric = Nothing
    , fading = Nothing
    , making = Nothing
    , passengerCar = Nothing
    , printing = Nothing
    , roadTransport = Nothing
    , seaTransport = Nothing
    , useIroning = Nothing
    , useNonIroning = Nothing
    }


computeMaterialTransportAndImpact : Db -> Country -> Mass -> Inputs.MaterialInput -> Transport
computeMaterialTransportAndImpact { distances, textile } country outputMass materialInput =
    let
        materialMass =
            materialInput.share
                |> Split.applyToQuantity outputMass
    in
    materialInput
        |> Inputs.computeMaterialTransport distances country.code
        |> Formula.transportRatio Split.zero
        |> computeTransportImpacts Impact.empty textile.wellKnown textile.wellKnown.roadTransport materialMass


computePreTreatment : Country -> Mass -> Process -> PreTreatments
computePreTreatment country mass process =
    let
        massInKg =
            Mass.inKilograms mass

        ( consumedElec, consumedHeat ) =
            ( process.elec
                |> Quantity.multiplyBy massInKg
            , process.heat
                |> Quantity.multiplyBy massInKg
            )
    in
    { heat = consumedHeat
    , impacts =
        Impact.sumImpacts
            [ -- NOTE: don't take pre-treatment process own impacts as they've
              --       already been added at the global ennobling step
              -- FIXME: this should be refactored to do everything at once
              -- process.impacts
              --  |> Impact.multiplyBy massInKg
              country.electricityProcess.impacts
                |> Impact.multiplyBy (Energy.inKilowattHours consumedElec)
            , country.heatProcess.impacts
                |> Impact.multiplyBy (Energy.inMegajoules consumedHeat)
            ]
    , kwh = consumedElec
    , operations = List.singleton process
    }


computePreTreatments : WellKnown -> List Inputs.MaterialInput -> Step -> PreTreatments
computePreTreatments wellKnown materials { country, inputMass } =
    materials
        |> List.concatMap
            (\{ material, share } ->
                wellKnown
                    |> WellKnown.getEnnoblingPreTreatments material.origin
                    |> List.map
                        (share
                            |> Split.applyToQuantity inputMass
                            |> computePreTreatment country
                        )
            )
        |> List.foldl
            (\{ heat, impacts, kwh, operations } acc ->
                { acc
                    | heat = acc.heat |> Quantity.plus heat
                    , impacts = Impact.sumImpacts [ acc.impacts, impacts ]
                    , kwh = acc.kwh |> Quantity.plus kwh
                    , operations = LE.unique <| acc.operations ++ operations
                }
            )
            emptyPreTreatments


{-| Computes step transport distances and impact regarding next step.

Docs: <https://fabrique-numerique.gitbook.io/ecobalyse/methodologie/transport>

-}
computeTransports : Db -> Inputs -> Step -> Step -> Step
computeTransports db inputs next ({ processInfo } as current) =
    let
        transport =
            if current.label == Label.Material then
                inputs.materials
                    |> List.map (computeMaterialTransportAndImpact db next.country current.outputMass)
                    |> Transport.sum

            else
                db.distances
                    |> Transport.getTransportBetween current.transport.impacts current.country.code next.country.code
                    |> computeTransportSummary current
                    |> computeTransportImpacts current.transport.impacts
                        db.textile.wellKnown
                        db.textile.wellKnown.roadTransport
                        (getTransportedMass inputs current)
    in
    { current
        | processInfo =
            { processInfo
                | airTransport = Just db.textile.wellKnown.airTransport.name
                , roadTransport = Just db.textile.wellKnown.roadTransport.name
                , seaTransport = Just db.textile.wellKnown.seaTransport.name
            }
        , transport = transport
    }


computeTransportImpacts : Impacts -> WellKnown -> Process -> Mass -> Transport -> Transport
computeTransportImpacts impacts { airTransport, seaTransport } roadProcess mass { air, road, sea } =
    { air = air
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
    , road = road
    , roadCooled = Quantity.zero
    , sea = sea
    , seaCooled = Quantity.zero
    }


computeTransportSummary : Step -> Transport -> Transport
computeTransportSummary step transport =
    let
        noTransports =
            Transport.default step.transport.impacts
    in
    case step.label of
        Label.Distribution ->
            -- Add default road transport to materialize transport to/from a warehouse
            noTransports
                |> Transport.add { noTransports | road = Length.kilometers 500 }

        Label.EndOfLife ->
            -- End of life leverages no transports
            noTransports

        Label.Making ->
            -- Air transport only applies between the Making and the Distribution steps
            transport
                |> Formula.transportRatio step.airTransportRatio

        Label.Use ->
            -- Product Use leverages no transports
            noTransports

        _ ->
            -- All other steps don't use air transport, force a 0 split
            transport
                |> Formula.transportRatio Split.zero


getInputSurface : Inputs -> Step -> Area
getInputSurface { product, surfaceMass } { inputMass } =
    let
        surfaceMassWithDefault =
            Maybe.withDefault product.surfaceMass surfaceMass
    in
    Unit.surfaceMassToSurface surfaceMassWithDefault inputMass


getOutputSurface : Inputs -> Step -> Area
getOutputSurface { product, surfaceMass } { outputMass } =
    Unit.surfaceMassToSurface (Maybe.withDefault product.surfaceMass surfaceMass) outputMass


getTransportedMass : Inputs -> Step -> Mass
getTransportedMass inputs { label, outputMass } =
    -- Transports from the Making step shouldn't include waste, only the final product.
    if label == Label.Making then
        inputs.mass

    else
        outputMass


updateFromInputs : Textile.Db -> Inputs -> Step -> Step
updateFromInputs { wellKnown } inputs ({ label, country, complementsImpacts } as step) =
    let
        { dyeingProcessType, makingComplexity, makingDeadStock, makingWaste, printing, surfaceMass, yarnSize } =
            inputs
    in
    case label of
        Label.Distribution ->
            { step
                | processInfo =
                    { defaultProcessInfo | distribution = Just wellKnown.distribution.name }
            }

        Label.EndOfLife ->
            { step
                | complementsImpacts =
                    { complementsImpacts
                        | outOfEuropeEOL = Inputs.getOutOfEuropeEOLComplement inputs
                    }
                , processInfo =
                    { defaultProcessInfo
                        | countryElec = Just country.electricityProcess.name
                        , countryHeat = Just country.heatProcess.name
                        , endOfLife = Just wellKnown.endOfLife.name
                        , passengerCar = Just "Transport en voiture vers point de collecte (1km)"
                    }
            }

        Label.Ennobling ->
            { step
                | dyeingProcessType = dyeingProcessType
                , printing = printing
                , processInfo =
                    { defaultProcessInfo
                        | countryElec = Just country.electricityProcess.name
                        , countryHeat = Just country.heatProcess.name
                        , dyeing =
                            -- FIXME
                            -- wellKnown
                            --     |> WellKnown.getDyeingProcess
                            --         (dyeingProcessType
                            --             |> Maybe.withDefault inputs.product.dyeing.defaultMedium
                            --         )
                            --     |> .name
                            --     |> Just
                            Nothing
                        , printing =
                            printing
                                |> Maybe.map
                                    (\{ kind } ->
                                        WellKnown.getPrintingProcess kind wellKnown |> .printingProcess |> .name
                                    )
                    }
            }

        Label.Fabric ->
            { step
                | processInfo =
                    { defaultProcessInfo
                        | countryElec = Just country.electricityProcess.name
                        , fabric =
                            inputs.product.fabric
                                |> Fabric.getProcess wellKnown
                                |> .name
                                |> Just
                    }
                , surfaceMass = surfaceMass
                , yarnSize = yarnSize
            }

        Label.Making ->
            { step
                | makingComplexity = makingComplexity
                , makingDeadStock = makingDeadStock
                , makingWaste = makingWaste
                , processInfo =
                    { defaultProcessInfo
                        | countryElec = Just country.electricityProcess.name
                        , fading = Just wellKnown.fading.name
                    }
            }

        Label.Material ->
            { step
                | complementsImpacts =
                    { complementsImpacts
                      -- Note: no other steps than the Material one generate microfibers pollution
                        | microfibers = Inputs.getTotalMicrofibersComplement inputs
                    }
            }

        Label.Spinning ->
            { step
                | processInfo = { defaultProcessInfo | countryElec = Just country.electricityProcess.name }
                , yarnSize = yarnSize
            }

        Label.Use ->
            { step
                | processInfo =
                    { defaultProcessInfo
                        | countryElec = Just country.electricityProcess.name
                        , useIroning =
                            -- Note: Much better expressing electricity consumption in kWh than in MJ
                            inputs.product.use.ironingElec
                                |> Energy.inKilowattHours
                                |> Format.formatFloat 4
                                |> (\x -> "Repassage\u{00A0}: " ++ x ++ "\u{00A0}kWh")
                                |> Just
                        , useNonIroning = Just inputs.product.use.nonIroningProcess.name
                    }
            }


initMass : Mass -> Step -> Step
initMass mass step =
    { step
        | inputMass = mass
        , outputMass = mass
    }


updateWasteAndMasses : Mass -> Mass -> Step -> Step
updateWasteAndMasses waste mass step =
    { step
        | inputMass = mass
        , outputMass = Quantity.difference mass waste
        , waste = waste
    }


updateDeadStock : Mass -> Mass -> Step -> Step
updateDeadStock deadstock mass step =
    { step
        | deadstock = deadstock
        , inputMass = mass
        , outputMass = Quantity.difference mass deadstock
    }


airTransportDisabled : Step -> Bool
airTransportDisabled { country, enabled, label } =
    not enabled
        || -- Note: disallow air transport from France to France at the Making step
           (label == Label.Making && country.code == Country.codeFromString "FR")


airTransportRatioToString : Split -> String
airTransportRatioToString percentage =
    case Split.toPercent percentage |> round of
        0 ->
            "Aucun transport aérien"

        _ ->
            Split.toPercentString 0 percentage ++ "% de transport aérien"


surfaceMassToString : Unit.SurfaceMass -> String
surfaceMassToString surfaceMass =
    "Grammage\u{00A0}: " ++ String.fromInt (Unit.surfaceMassInGramsPerSquareMeters surfaceMass) ++ "\u{202F}g/m²"


makingWasteToString : Split -> String
makingWasteToString makingWaste =
    if makingWaste == Split.zero then
        "Aucune perte en confection"

    else
        Split.toPercentString 0 makingWaste ++ "% de pertes"


makingDeadStockToString : Split -> String
makingDeadStockToString makingDeadStock =
    if makingDeadStock == Split.zero then
        "Aucun stock dormant"

    else
        Split.toPercentString 0 makingDeadStock ++ "% de stocks dormants"


yarnSizeToString : Unit.YarnSize -> String
yarnSizeToString yarnSize =
    "Titrage\u{00A0}: " ++ String.fromFloat (Unit.yarnSizeInKilometers yarnSize) ++ "\u{202F}Nm (" ++ yarnSizeToDtexString yarnSize ++ ")"


yarnSizeToDtexString : Unit.YarnSize -> String
yarnSizeToDtexString yarnSize =
    Format.formatFloat 2 (Unit.yarnSizeInGrams yarnSize) ++ "\u{202F}Dtex"


encode : Step -> Encode.Value
encode v =
    Encode.object
        [ ( "airTransportRatio", Split.encodeFloat v.airTransportRatio )
        , ( "complementsImpacts", Impact.encodeComplementsImpacts v.complementsImpacts )
        , ( "country", Country.encode v.country )
        , ( "deadstock", Encode.float (Mass.inKilograms v.deadstock) )
        , ( "durability", Unit.encodeNonPhysicalDurability v.durability )
        , ( "editable", Encode.bool v.editable )
        , ( "elec_kWh", Encode.float (Energy.inKilowattHours v.kwh) )
        , ( "enabled", Encode.bool v.enabled )
        , ( "heat_MJ", Encode.float (Energy.inMegajoules v.heat) )
        , ( "impacts", v.impacts |> Impact.applyComplements (Impact.getTotalComplementsImpacts v.complementsImpacts) |> Impact.encode )
        , ( "inputMass", Encode.float (Mass.inKilograms v.inputMass) )
        , ( "label", Encode.string (Label.toString v.label) )
        , ( "makingDeadStock", v.makingDeadStock |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "makingWaste", v.makingWaste |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "outputMass", Encode.float (Mass.inKilograms v.outputMass) )
        , ( "picking", v.picking |> Maybe.map Unit.encodePickPerMeter |> Maybe.withDefault Encode.null )
        , ( "preTreatments", encodePreTreatments v.preTreatments )
        , ( "processInfo", encodeProcessInfo v.processInfo )
        , ( "surfaceMass", v.surfaceMass |> Maybe.map Unit.encodeSurfaceMass |> Maybe.withDefault Encode.null )
        , ( "threadDensity", v.threadDensity |> Maybe.map Unit.encodeThreadDensity |> Maybe.withDefault Encode.null )
        , ( "transport", Transport.encode v.transport )
        , ( "waste", Encode.float (Mass.inKilograms v.waste) )
        , ( "yarnSize", v.yarnSize |> Maybe.map Unit.encodeYarnSize |> Maybe.withDefault Encode.null )
        ]


encodePreTreatments : PreTreatments -> Encode.Value
encodePreTreatments v =
    Encode.object
        [ ( "elec_kWh", Encode.float (Energy.inKilowattHours v.kwh) )
        , ( "heat_MJ", Encode.float (Energy.inMegajoules v.heat) )
        , ( "impacts", Impact.encode v.impacts )
        , ( "operations", v.operations |> List.map Process.getDisplayName |> Encode.list Encode.string )
        ]


encodeProcessInfo : ProcessInfo -> Encode.Value
encodeProcessInfo v =
    let
        encodeMaybeString =
            Maybe.map Encode.string >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "airTransport", encodeMaybeString v.airTransport )
        , ( "airTransportRatio", encodeMaybeString v.airTransportRatio )
        , ( "countryElec", encodeMaybeString v.countryElec )
        , ( "countryHeat", encodeMaybeString v.countryHeat )
        , ( "distribution", encodeMaybeString v.distribution )
        , ( "endOfLife", encodeMaybeString v.endOfLife )
        , ( "fabric", encodeMaybeString v.fabric )
        , ( "fading", encodeMaybeString v.fading )
        , ( "making", encodeMaybeString v.making )
        , ( "passengerCar", encodeMaybeString v.passengerCar )
        , ( "roadTransport", encodeMaybeString v.roadTransport )
        , ( "seaTransport", encodeMaybeString v.seaTransport )
        , ( "useIroning", encodeMaybeString v.useIroning )
        , ( "useNonIroning", encodeMaybeString v.useNonIroning )
        ]


getTotalImpactsWithoutComplements : Step -> Impacts
getTotalImpactsWithoutComplements { impacts, transport } =
    Impact.sumImpacts [ impacts, transport.impacts ]

module Data.Textile.Simulator exposing
    ( Simulator
    , compute
    , encode
    , getTotalImpactsWithoutComplements
    , getTotalImpactsWithoutDurability
    , stepMaterialImpacts
    , toStepsImpacts
    )

import Array
import Data.Component as Component
import Data.Country as Country
import Data.Env as Env
import Data.Impact as Impact exposing (Impacts)
import Data.Impact.Definition as Definition
import Data.Split as Split
import Data.Textile.Dyeing as Dyeing
import Data.Textile.Economics as Economics
import Data.Textile.Fabric as Fabric
import Data.Textile.Formula as Formula
import Data.Textile.Inputs as Inputs exposing (Inputs)
import Data.Textile.LifeCycle as LifeCycle exposing (LifeCycle)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Material.Origin as Origin
import Data.Textile.Material.Spinning as Spinning exposing (Spinning)
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Query as Query exposing (Query)
import Data.Textile.Step as Step exposing (Step)
import Data.Textile.Step.Label as Label exposing (Label)
import Data.Textile.WellKnown as WellKnown
import Data.Transport as Transport exposing (Transport)
import Data.Unit as Unit
import Duration exposing (Duration)
import Energy exposing (Energy)
import Json.Encode as Encode
import Mass
import Quantity
import Static.Db exposing (Db)


type alias Simulator =
    { complementsImpacts : Impact.ComplementsImpacts
    , daysOfWear : Duration
    , durability : Unit.HolisticDurability
    , impacts : Impacts
    , inputs : Inputs
    , lifeCycle : LifeCycle
    , transport : Transport
    , trimsImpacts : Impacts
    , useNbCycles : Int
    }


encode : Simulator -> Encode.Value
encode v =
    Encode.object
        [ ( "complementsImpacts", Impact.encodeComplementsImpacts v.complementsImpacts )
        , ( "daysOfWear", v.daysOfWear |> Duration.inDays |> round |> Encode.int )
        , ( "durability", v.durability |> Unit.floatDurabilityFromHolistic |> Encode.float )
        , ( "impacts", Impact.encode v.impacts )
        , ( "impactsWithoutDurability", Impact.encode (getTotalImpactsWithoutDurability v) )
        , ( "inputs", Inputs.encode v.inputs )
        , ( "lifeCycle", LifeCycle.encode v.lifeCycle )
        , ( "transport", Transport.encode v.transport )
        , ( "useNbCycles", Encode.int v.useNbCycles )
        ]


init : Db -> Query -> Result String Simulator
init db =
    Query.handleUpcycling
        >> Inputs.fromQuery db
        >> Result.map
            (\({ product } as inputs) ->
                inputs
                    |> LifeCycle.init db
                    |> (\lifeCycle ->
                            { complementsImpacts = Impact.noComplementsImpacts
                            , daysOfWear = inputs.product.use.daysOfWear
                            , durability =
                                { nonPhysical = Unit.standardDurability Unit.NonPhysicalDurability
                                , physical = inputs.physicalDurability |> Maybe.withDefault (Unit.maxDurability Unit.PhysicalDurability)
                                }
                            , impacts = Impact.empty
                            , inputs = inputs
                            , lifeCycle = lifeCycle
                            , transport = Transport.default Impact.empty
                            , trimsImpacts = Impact.empty
                            , useNbCycles = Product.customDaysOfWear product.use
                            }
                       )
            )


{-| Computes simulation impacts.
-}
compute : Db -> Query -> Result String Simulator
compute db query =
    let
        next fn =
            Result.map fn

        andNext fn =
            Result.andThen fn

        nextWithDb fn =
            next (fn db)

        andNextWithDb fn =
            andNext (fn db)

        nextIf label fn =
            if not (List.member label query.disabledSteps) then
                next fn

            else
                identity

        nextWithDbIf label fn =
            if not (List.member label query.disabledSteps) then
                nextWithDb fn

            else
                identity
    in
    init db query
        -- Ensure end product mass is first applied to the final Distribution step
        |> next initializeFinalMass
        --
        -- WASTE: compute the initial required material mass
        --
        -- Compute Making mass waste - Confection
        |> nextIf Label.Making computeMakingStepWaste
        -- Compute Making dead stock - Confection
        |> nextIf Label.Making computeMakingStepDeadStock
        -- Compute Knitting/Weawing waste - Tissage/Tricotage
        |> nextWithDbIf Label.Fabric computeFabricStepWaste
        -- Compute Spinning waste - Filature
        |> nextIf Label.Spinning computeSpinningStepWaste
        -- Compute Material waste - Matière
        -- We always need to compute the Material's step waste otherwise the input mass
        -- for the next step (spinning) would never be computed.
        |> next computeMaterialStepWaste
        --
        -- DURABILITY
        --
        |> next computeDurability
        -- Compute Making air transport ratio (depends on durability) - Confection
        |> nextIf Label.Making computeMakingAirTransportRatio
        --
        -- LIFECYCLE STEP IMPACTS
        --
        -- Compute Material step impacts
        |> nextIf Label.Material (computeMaterialImpacts db)
        -- Compute Spinning step impacts
        |> nextIf Label.Spinning computeSpinningImpacts
        -- Compute Weaving & Knitting step impacts
        |> nextWithDbIf Label.Fabric computeFabricImpacts
        -- Compute Ennobling step Dyeing impacts
        |> nextWithDbIf Label.Ennobling computeDyeingImpacts
        -- Compute Ennobling step Printing impacts
        |> nextWithDbIf Label.Ennobling computePrintingImpacts
        -- Compute Ennobling step Finishing impacts
        |> nextWithDbIf Label.Ennobling computeFinishingImpacts
        -- Compute Ennobling step bleaching impacts
        |> nextWithDbIf Label.Ennobling computeBleachingImpacts
        -- Compute Making step impacts
        |> nextWithDbIf Label.Making computeMakingImpacts
        -- Compute product Use impacts
        |> nextIf Label.Use computeUseImpacts
        -- Compute product Use impacts
        |> nextWithDbIf Label.EndOfLife computeEndOfLifeImpacts
        --
        -- TRANSPORTS
        --
        -- Compute step transport
        |> nextWithDb computeStepsTransport
        -- Compute transport summary
        |> next computeTotalTransportImpacts
        --
        -- TRIMS
        --
        -- Compute trims
        |> andNextWithDb computeTrims
        --
        -- Final impacts
        --
        |> next computeFinalImpacts


initializeFinalMass : Simulator -> Simulator
initializeFinalMass ({ inputs } as simulator) =
    simulator
        |> updateLifeCycleSteps Label.all (Step.initMass inputs.mass)


computeDurability : Simulator -> Simulator
computeDurability ({ inputs } as simulator) =
    let
        nonPhysicalDurability =
            Economics.computeNonPhysicalDurabilityIndex
                { business =
                    inputs.business
                        |> Maybe.withDefault inputs.product.economics.business
                , numberOfReferences =
                    inputs.numberOfReferences
                        |> Maybe.withDefault inputs.product.economics.numberOfReferences
                , price =
                    inputs.price
                        |> Maybe.withDefault inputs.product.economics.price
                , repairCost = inputs.product.economics.repairCost
                , traceability =
                    inputs.traceability
                        |> Maybe.withDefault inputs.product.economics.traceability
                }

        newDurability =
            { nonPhysical = nonPhysicalDurability
            , physical = simulator.durability.physical
            }
    in
    { simulator
        | daysOfWear =
            simulator.daysOfWear
                |> Quantity.multiplyBy (Unit.floatDurabilityFromHolistic newDurability)
        , durability = newDurability
        , useNbCycles =
            round (toFloat simulator.useNbCycles * Unit.floatDurabilityFromHolistic newDurability)
    }


computeMakingAirTransportRatio : Simulator -> Simulator
computeMakingAirTransportRatio ({ durability, inputs } as simulator) =
    simulator
        |> updateLifeCycleStep Label.Making
            (\({ country } as step) ->
                { step
                    | airTransportRatio =
                        case inputs.airTransportRatio of
                            Just airTransportRatio ->
                                -- User-provided value always takes precedence
                                airTransportRatio

                            Nothing ->
                                if Country.isEuropeOrTurkey country then
                                    -- If Making country is Europe or Turkey, airTransportRatio is always 0
                                    Split.zero

                                else if Unit.floatDurabilityFromHolistic durability >= 1 then
                                    -- Durable garments outside of Europe and Turkey
                                    Split.third

                                else
                                    -- FIXME: how about falling back to country default?
                                    -- country.airTransportRatio
                                    Split.full
                }
            )


computeEndOfLifeImpacts : Db -> Simulator -> Simulator
computeEndOfLifeImpacts { textile } simulator =
    simulator
        |> updateLifeCycleStep Label.EndOfLife
            (\({ country } as step) ->
                let
                    { heat, impacts, kwh } =
                        step.outputMass
                            |> Formula.endOfLifeImpacts step.impacts
                                { countryElecProcess = country.electricityProcess
                                , endOfLife = textile.wellKnown.endOfLife
                                , heatProcess = country.heatProcess
                                , passengerCar = textile.wellKnown.passengerCar
                                , volume = simulator.inputs.product.endOfLife.volume
                                }
                in
                { step
                    | heat = heat
                    , impacts = impacts
                    , kwh = kwh
                }
            )


computeUseImpacts : Simulator -> Simulator
computeUseImpacts ({ inputs, useNbCycles } as simulator) =
    simulator
        |> updateLifeCycleStep Label.Use
            (\({ country } as step) ->
                let
                    { impacts, kwh } =
                        step.outputMass
                            |> Formula.useImpacts step.impacts
                                { countryElecProcess = country.electricityProcess
                                , ironingElec = inputs.product.use.ironingElec
                                , nonIroningProcess = inputs.product.use.nonIroningProcess
                                , useNbCycles = useNbCycles
                                }
                in
                { step | impacts = impacts, kwh = kwh }
            )


computeMakingImpacts : Db -> Simulator -> Simulator
computeMakingImpacts { textile } ({ inputs } as simulator) =
    simulator
        |> updateLifeCycleStep Label.Making
            (\({ country } as step) ->
                let
                    { heat, impacts, kwh } =
                        step.outputMass
                            |> Formula.makingImpacts step.impacts
                                { countryElecProcess = country.electricityProcess
                                , countryHeatProcess = country.heatProcess
                                , fadingProcess =
                                    -- Note: in the future, we may have distinct fading processes per countries
                                    if inputs.fading == Just True then
                                        Just textile.wellKnown.fading

                                    else
                                        Nothing
                                , makingComplexity =
                                    inputs.fabricProcess
                                        |> Fabric.getMakingComplexity inputs.product.making.complexity inputs.makingComplexity
                                }
                in
                { step | heat = heat, impacts = impacts, kwh = kwh }
            )


computeDyeingImpacts : Db -> Simulator -> Simulator
computeDyeingImpacts { textile } ({ inputs } as simulator) =
    simulator
        |> updateLifeCycleStep Label.Ennobling
            (\({ country, dyeingProcessType } as step) ->
                let
                    heatProcess =
                        WellKnown.getEnnoblingHeatProcess textile.wellKnown country

                    dyeingProcess =
                        Dyeing.toProcess textile.wellKnown dyeingProcessType

                    dyeingToxicity =
                        inputs.materials
                            |> List.map
                                (\{ material, share } ->
                                    Formula.materialDyeingToxicityImpacts step.impacts
                                        { aquaticPollutionScenario = step.country.aquaticPollutionScenario
                                        , dyeingToxicityProcess =
                                            if Origin.isSynthetic material.origin then
                                                textile.wellKnown.dyeingSynthetic

                                            else
                                                textile.wellKnown.dyeingCellulosic
                                        }
                                        step.outputMass
                                        share
                                )
                            |> Impact.sumImpacts

                    preTreatments =
                        step |> Step.computePreTreatments textile.wellKnown inputs.materials

                    { heat, impacts, kwh } =
                        step.outputMass
                            |> Formula.dyeingImpacts step.impacts
                                dyeingProcess
                                heatProcess
                                country.electricityProcess
                in
                { step
                    | heat = Quantity.sum [ step.heat, heat, preTreatments.heat ]
                    , impacts = Impact.sumImpacts [ step.impacts, impacts, dyeingToxicity, preTreatments.impacts ]
                    , kwh = Quantity.sum [ step.kwh, kwh, preTreatments.kwh ]
                    , preTreatments = preTreatments
                }
            )


computePrintingImpacts : Db -> Simulator -> Simulator
computePrintingImpacts { textile } ({ inputs } as simulator) =
    simulator
        |> updateLifeCycleStep Label.Ennobling
            (\({ country } as step) ->
                case step.printing of
                    Just { kind, ratio } ->
                        let
                            { printingProcess, printingToxicityProcess } =
                                WellKnown.getPrintingProcess kind textile.wellKnown

                            { heat, impacts, kwh } =
                                step.outputMass
                                    |> Formula.printingImpacts step.impacts
                                        { elecProcess = country.electricityProcess
                                        , heatProcess = WellKnown.getEnnoblingHeatProcess textile.wellKnown country
                                        , printingProcess = printingProcess
                                        , ratio = ratio
                                        , surfaceMass = Maybe.withDefault inputs.product.surfaceMass inputs.surfaceMass
                                        }

                            printingToxicity =
                                step.outputMass
                                    |> Formula.materialPrintingToxicityImpacts
                                        step.impacts
                                        { aquaticPollutionScenario = step.country.aquaticPollutionScenario
                                        , printingToxicityProcess = printingToxicityProcess
                                        }
                                        ratio
                        in
                        { step
                            | heat = step.heat |> Quantity.plus heat
                            , impacts = Impact.sumImpacts [ step.impacts, impacts, printingToxicity ]
                            , kwh = step.kwh |> Quantity.plus kwh
                        }

                    Nothing ->
                        step
            )


computeFinishingImpacts : Db -> Simulator -> Simulator
computeFinishingImpacts { textile } simulator =
    simulator
        |> updateLifeCycleStep Label.Ennobling
            (\({ country } as step) ->
                let
                    { heat, impacts, kwh } =
                        step.outputMass
                            |> Formula.finishingImpacts step.impacts
                                { elecProcess = country.electricityProcess
                                , finishingProcess = textile.wellKnown.finishing
                                , heatProcess = WellKnown.getEnnoblingHeatProcess textile.wellKnown country
                                }
                in
                { step
                    | heat = step.heat |> Quantity.plus heat
                    , impacts = Impact.sumImpacts [ step.impacts, impacts ]
                    , kwh = step.kwh |> Quantity.plus kwh
                }
            )


computeBleachingImpacts : Db -> Simulator -> Simulator
computeBleachingImpacts { textile } simulator =
    simulator
        |> updateLifeCycleStep Label.Ennobling
            (\step ->
                let
                    impacts =
                        step.outputMass
                            |> Formula.bleachingImpacts step.impacts
                                { aquaticPollutionScenario = step.country.aquaticPollutionScenario
                                , bleachingProcess = textile.wellKnown.bleaching
                                }
                in
                { step
                    | impacts = Impact.sumImpacts [ step.impacts, impacts ]
                }
            )


stepMaterialImpacts : Db -> Material -> Step -> Impacts
stepMaterialImpacts { textile } material step =
    case Material.getRecyclingData material textile.materials of
        -- Recycled material: apply CFF
        Just ( sourceMaterial, cffData ) ->
            step.outputMass
                |> Formula.recycledMaterialImpacts step.impacts
                    { cffData = cffData
                    , nonRecycledProcess = sourceMaterial.materialProcess
                    , recycledProcess = material.materialProcess
                    }

        -- Non-recycled Material
        Nothing ->
            step.outputMass
                |> Formula.pureMaterialImpacts step.impacts material.materialProcess


computeMaterialImpacts : Db -> Simulator -> Simulator
computeMaterialImpacts db ({ inputs } as simulator) =
    simulator
        |> updateLifeCycleStep Label.Material
            (\step ->
                { step
                    | impacts =
                        inputs.materials
                            |> List.map
                                (\{ material, share } ->
                                    step
                                        |> stepMaterialImpacts db material
                                        |> Impact.mapImpacts (\_ -> Quantity.multiplyBy (Split.toFloat share))
                                )
                            |> Impact.sumImpacts
                }
            )


stepSpinningImpacts : Material -> Maybe Spinning -> Product -> Step -> { heat : Energy, impacts : Impacts, kwh : Energy }
stepSpinningImpacts material maybeSpinning product step =
    let
        yarnSize =
            step.yarnSize
                |> Maybe.withDefault product.yarnSize

        spinning =
            maybeSpinning
                |> Maybe.withDefault (Spinning.getDefault material.origin)

        kwh =
            spinning
                |> Spinning.getElec step.outputMass yarnSize
                |> Energy.kilowattHours
    in
    Formula.spinningImpacts step.impacts
        { countryElecProcess = step.country.electricityProcess
        , spinningKwh = kwh
        }


computeSpinningImpacts : Simulator -> Simulator
computeSpinningImpacts ({ inputs } as simulator) =
    simulator
        |> updateLifeCycleStep Label.Spinning
            (\step ->
                { step
                    | impacts =
                        inputs.materials
                            |> List.map
                                (\{ material, share, spinning } ->
                                    step
                                        |> stepSpinningImpacts material spinning inputs.product
                                        |> .impacts
                                        |> Impact.mapImpacts (\_ -> Quantity.multiplyBy (Split.toFloat share))
                                )
                            |> Impact.sumImpacts
                    , kwh =
                        inputs.materials
                            |> List.map
                                (\{ material, share, spinning } ->
                                    step
                                        |> stepSpinningImpacts material spinning inputs.product
                                        |> .kwh
                                        |> Quantity.multiplyBy (Split.toFloat share)
                                )
                            |> List.foldl Quantity.plus Quantity.zero
                }
            )


computeFabricImpacts : Db -> Simulator -> Simulator
computeFabricImpacts { textile } ({ inputs, lifeCycle } as simulator) =
    let
        fabricOutputMass =
            lifeCycle
                |> LifeCycle.getStepProp Label.Fabric .outputMass Quantity.zero
    in
    simulator
        |> updateLifeCycleStep Label.Fabric
            (\({ country } as step) ->
                let
                    process =
                        inputs.fabricProcess
                            |> Maybe.withDefault inputs.product.fabric
                            |> Fabric.getProcess textile.wellKnown

                    { impacts, kwh, picking, threadDensity } =
                        if
                            inputs.fabricProcess
                                |> Maybe.withDefault inputs.product.fabric
                                |> Fabric.isKnitted
                        then
                            step.outputMass
                                |> Formula.knittingImpacts step.impacts
                                    { countryElecProcess = country.electricityProcess
                                    , elec = process.elec
                                    }

                        else
                            let
                                surfaceMass =
                                    inputs.surfaceMass
                                        |> Maybe.withDefault inputs.product.surfaceMass
                            in
                            Formula.weavingImpacts step.impacts
                                { countryElecProcess = country.electricityProcess
                                , outputMass = fabricOutputMass
                                , pickingElec = WellKnown.weavingElecPPPM
                                , surfaceMass = surfaceMass
                                , yarnSize = inputs.yarnSize |> Maybe.withDefault inputs.product.yarnSize
                                }
                in
                { step
                    | impacts = impacts
                    , kwh = kwh
                    , picking = picking
                    , threadDensity = threadDensity
                }
            )


computeMakingStepWaste : Simulator -> Simulator
computeMakingStepWaste ({ inputs } as simulator) =
    let
        { fabricProcess, makingWaste, product } =
            inputs

        { mass, waste } =
            inputs.mass
                |> Formula.genericWaste
                    (fabricProcess
                        |> Fabric.getMakingWaste product.making.pcrWaste makingWaste
                    )
    in
    simulator
        |> updateLifeCycleStep Label.Making (Step.updateWasteAndMasses waste mass)
        |> updateLifeCycleSteps Label.upcyclables (Step.initMass mass)


computeMakingStepDeadStock : Simulator -> Simulator
computeMakingStepDeadStock ({ inputs, lifeCycle } as simulator) =
    let
        { deadstock, mass } =
            lifeCycle
                |> LifeCycle.getStepProp Label.Making .inputMass Quantity.zero
                |> Formula.makingDeadStock (Maybe.withDefault Env.defaultDeadStock inputs.makingDeadStock)
    in
    simulator
        |> updateLifeCycleStep Label.Making (Step.updateDeadStock deadstock mass)
        |> updateLifeCycleSteps Label.upcyclables (Step.initMass mass)


computeFabricStepWaste : Db -> Simulator -> Simulator
computeFabricStepWaste { textile } ({ inputs, lifeCycle } as simulator) =
    let
        { mass, waste } =
            lifeCycle
                |> LifeCycle.getStepProp Label.Making .inputMass Quantity.zero
                |> Formula.genericWaste
                    (inputs.fabricProcess
                        |> Maybe.withDefault inputs.product.fabric
                        |> Fabric.getProcess textile.wellKnown
                        |> .waste
                    )
    in
    simulator
        |> updateLifeCycleStep Label.Fabric (Step.updateWasteAndMasses waste mass)
        |> updateLifeCycleSteps [ Label.Material, Label.Spinning ] (Step.initMass mass)


computeMaterialStepWaste : Simulator -> Simulator
computeMaterialStepWaste ({ inputs, lifeCycle } as simulator) =
    let
        { mass, waste } =
            lifeCycle
                |> LifeCycle.getStepProp Label.Spinning .inputMass Quantity.zero
                |> (\inputMass ->
                        inputs.materials
                            |> List.map
                                (\{ material, share } ->
                                    inputMass
                                        |> Quantity.multiplyBy (Split.toFloat share)
                                        |> Formula.genericWaste material.materialProcess.waste
                                )
                            |> List.foldl
                                (\curr acc ->
                                    { mass = curr.mass |> Quantity.plus acc.mass
                                    , waste = curr.waste |> Quantity.plus acc.waste
                                    }
                                )
                                { mass = Quantity.zero, waste = Quantity.zero }
                   )
    in
    simulator
        |> updateLifeCycleStep Label.Material (Step.updateWasteAndMasses waste mass)


computeSpinningStepWaste : Simulator -> Simulator
computeSpinningStepWaste ({ inputs, lifeCycle } as simulator) =
    let
        { mass, waste } =
            lifeCycle
                |> LifeCycle.getStepProp Label.Fabric .inputMass Quantity.zero
                |> (\inputMass ->
                        inputs.materials
                            |> List.map
                                (\{ material, share, spinning } ->
                                    let
                                        spinningProcess =
                                            spinning
                                                |> Maybe.withDefault (Spinning.getDefault material.origin)

                                        processWaste =
                                            Spinning.waste spinningProcess

                                        outputMaterialMass =
                                            -- The output mass is the input mass of the next step
                                            inputMass
                                                |> Quantity.multiplyBy (Split.toFloat share)

                                        inputMaterialMass =
                                            -- Formula : inputMass - inputMass * waste = outputMass
                                            -- => inputMass * (1 - waste) = outputMass
                                            -- => inputMass = outputMass / (1 - waste)
                                            Split.complement processWaste
                                                |> Split.divideBy (Mass.inKilograms outputMaterialMass)
                                                |> Mass.kilograms
                                    in
                                    { mass = inputMaterialMass
                                    , waste = Quantity.difference inputMaterialMass outputMaterialMass
                                    }
                                )
                            |> List.foldl
                                (\curr acc ->
                                    { mass = curr.mass |> Quantity.plus acc.mass
                                    , waste = curr.waste |> Quantity.plus acc.waste
                                    }
                                )
                                { mass = Quantity.zero, waste = Quantity.zero }
                   )
    in
    simulator
        |> updateLifeCycleStep Label.Spinning (Step.updateWasteAndMasses waste mass)


computeStepsTransport : Db -> Simulator -> Simulator
computeStepsTransport db simulator =
    simulator |> updateLifeCycle (LifeCycle.computeStepsTransport db simulator.inputs)


computeTotalTransportImpacts : Simulator -> Simulator
computeTotalTransportImpacts simulator =
    { simulator
        | transport =
            simulator.lifeCycle
                |> LifeCycle.computeTotalTransportImpacts
    }


computeTrims : Db -> Simulator -> Result String Simulator
computeTrims db ({ durability } as simulator) =
    simulator.inputs.trims
        |> Component.compute db
        |> Result.map Component.extractImpacts
        |> Result.map
            (\trimsImpacts ->
                { simulator
                    | impacts =
                        Impact.sumImpacts
                            [ simulator.impacts
                            , trimsImpacts
                                |> Impact.divideBy (Unit.floatDurabilityFromHolistic durability)
                            ]
                    , trimsImpacts = trimsImpacts
                }
            )


computeFinalImpacts : Simulator -> Simulator
computeFinalImpacts ({ durability, lifeCycle } as simulator) =
    let
        complementsImpacts =
            lifeCycle
                |> Array.filter .enabled
                |> LifeCycle.sumComplementsImpacts
                |> Impact.divideComplementsImpactsBy (Unit.floatDurabilityFromHolistic durability)
    in
    { simulator
        | complementsImpacts = complementsImpacts
        , impacts =
            Impact.sumImpacts
                [ simulator.impacts
                , lifeCycle
                    |> LifeCycle.computeFinalImpacts
                    |> Impact.divideBy (Unit.floatDurabilityFromHolistic durability)
                    |> Impact.impactsWithComplements complementsImpacts
                ]
    }


getTotalImpactsWithoutComplements : Simulator -> Impacts
getTotalImpactsWithoutComplements { durability, lifeCycle } =
    lifeCycle
        |> Array.filter .enabled
        |> Array.map Step.getTotalImpactsWithoutComplements
        |> Array.toList
        |> Impact.sumImpacts
        |> Impact.divideBy (Unit.floatDurabilityFromHolistic durability)


getTotalImpactsWithoutDurability : Simulator -> Impacts
getTotalImpactsWithoutDurability { lifeCycle, trimsImpacts } =
    let
        complementsImpactsWithoutDurability =
            lifeCycle
                |> Array.filter .enabled
                |> LifeCycle.sumComplementsImpacts
    in
    Impact.sumImpacts
        [ lifeCycle
            |> LifeCycle.computeFinalImpacts
            |> Impact.impactsWithComplements complementsImpactsWithoutDurability
        , trimsImpacts
        ]


updateLifeCycle : (LifeCycle -> LifeCycle) -> Simulator -> Simulator
updateLifeCycle update simulator =
    { simulator | lifeCycle = update simulator.lifeCycle }


updateLifeCycleStep : Label -> (Step -> Step) -> Simulator -> Simulator
updateLifeCycleStep label update =
    updateLifeCycle (LifeCycle.updateStep label update)


updateLifeCycleSteps : List Label -> (Step -> Step) -> Simulator -> Simulator
updateLifeCycleSteps labels update =
    updateLifeCycle (LifeCycle.updateSteps labels update)


toStepsImpacts : Definition.Trigram -> Simulator -> Impact.StepsImpacts
toStepsImpacts trigram simulator =
    let
        getImpacts label =
            simulator.lifeCycle
                |> Array.filter .enabled
                |> LifeCycle.getStep label
                |> Maybe.map .impacts
                |> Maybe.withDefault Impact.empty

        getImpact =
            Impact.getImpact trigram
                >> Just

        applyComplement complementImpact =
            if trigram == Definition.Ecs then
                Maybe.map
                    (Quantity.minus
                        (complementImpact
                            |> Quantity.multiplyBy (Unit.floatDurabilityFromHolistic simulator.durability)
                        )
                    )

            else
                identity
    in
    { distribution = Nothing
    , endOfLife =
        getImpacts Label.EndOfLife
            |> getImpact
            |> applyComplement simulator.complementsImpacts.outOfEuropeEOL
    , materials =
        getImpacts Label.Material
            |> getImpact
            |> applyComplement simulator.complementsImpacts.microfibers
    , packaging = Nothing
    , transform =
        [ getImpacts Label.Spinning
        , getImpacts Label.Fabric
        , getImpacts Label.Ennobling
        , getImpacts Label.Making
        ]
            |> Impact.sumImpacts
            |> getImpact
    , transports = getImpact simulator.transport.impacts
    , trims = getImpact simulator.trimsImpacts
    , usage = getImpacts Label.Use |> getImpact
    }
        |> Impact.divideStepsImpactsBy (Unit.floatDurabilityFromHolistic simulator.durability)

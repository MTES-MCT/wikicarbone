module Data.Textile.Formula exposing
    ( bleachingImpacts
    , computePicking
    , computeThreadDensity
    , dyeingImpacts
    , endOfLifeImpacts
    , finishingImpacts
    , genericWaste
    , knittingImpacts
    , makingDeadStock
    , makingImpacts
    , makingWaste
    , materialDyeingToxicityImpacts
    , materialPrintingToxicityImpacts
    , printingImpacts
    , pureMaterialImpacts
    , recycledMaterialImpacts
    , spinningImpacts
    , transportRatio
    , useImpacts
    , weavingImpacts
    )

import Area exposing (Area)
import Data.Country as Country
import Data.Impact as Impact exposing (Impacts)
import Data.Split as Split exposing (Split)
import Data.Textile.MakingComplexity as MakingComplexity exposing (MakingComplexity)
import Data.Textile.Material exposing (CFFData)
import Data.Textile.Process as Process exposing (Process)
import Data.Transport as Transport exposing (Transport)
import Data.Unit as Unit
import Duration
import Energy exposing (Energy)
import Mass exposing (Mass)
import Quantity
import Volume exposing (Volume)


type alias StepValues =
    { heat : Energy
    , impacts : Impacts
    , kwh : Energy
    }



-- Waste


{-| Compute source mass needed and waste generated by the operation.
-}
genericWaste : Unit.Ratio -> Mass -> { mass : Mass, waste : Mass }
genericWaste processWaste baseMass =
    let
        waste =
            baseMass
                |> Quantity.multiplyBy (Unit.ratioToFloat processWaste)
    in
    { mass = baseMass |> Quantity.plus waste, waste = waste }


{-| Compute source material mass needed and waste generated by the operation, according to
material & product waste data.
-}
makingWaste : Split -> Mass -> { mass : Mass, waste : Mass }
makingWaste pcrWaste baseMass =
    let
        mass =
            -- (product weight + textile waste for confection) / (1 - PCR product waste rate)
            baseMass
                |> Quantity.divideBy (Split.toFloat (Split.complement pcrWaste))
    in
    { mass = mass, waste = Quantity.minus baseMass mass }


{-| Compute source material mass needed and deadstock generated by the operation, according to
making deadstock data.
-}
makingDeadStock : Split -> Mass -> { deadstock : Mass, mass : Mass }
makingDeadStock deadstock baseMass =
    let
        mass =
            -- (product weight + textile deadstock during confection) / (1 - deadstock rate)
            baseMass
                |> Quantity.divideBy (Split.toFloat (Split.complement deadstock))
    in
    { deadstock = Quantity.minus baseMass mass, mass = mass }



-- Impacts
--


pureMaterialImpacts : Impacts -> Process -> Mass -> Impacts
pureMaterialImpacts impacts process mass =
    impacts
        |> Impact.mapImpacts
            (\trigram _ ->
                mass
                    |> Unit.forKg (Process.getImpact trigram process)
            )


recycledMaterialImpacts :
    Impacts
    -> { cffData : CFFData, nonRecycledProcess : Process, recycledProcess : Process }
    -> Mass
    -> Impacts
recycledMaterialImpacts impacts { cffData, nonRecycledProcess, recycledProcess } outputMass =
    let
        { manufacturerAllocation, recycledQualityRatio } =
            cffData
    in
    impacts
        |> Impact.mapImpacts
            (\trigram _ ->
                let
                    ( recycledImpactPerKg, nonRecycledImpactPerKg ) =
                        ( Process.getImpact trigram recycledProcess |> Unit.impactToFloat
                        , Process.getImpact trigram nonRecycledProcess |> Unit.impactToFloat
                        )
                in
                Mass.inKilograms outputMass
                    * (Split.apply recycledImpactPerKg manufacturerAllocation
                        + Split.apply (Split.toFloat recycledQualityRatio) (Split.complement manufacturerAllocation)
                        * nonRecycledImpactPerKg
                      )
                    |> Unit.impact
            )


spinningImpacts :
    Impacts
    -> { countryElecProcess : Process, spinningKwh : Energy }
    -> StepValues
spinningImpacts impacts { countryElecProcess, spinningKwh } =
    { heat = Quantity.zero
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    spinningKwh |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                )
    , kwh = spinningKwh
    }


dyeingImpacts :
    Impacts
    -> Process -- Inbound: Dyeing process
    -> Process -- Outbound: country heat impact
    -> Process -- Outbound: country electricity impact
    -> Mass
    -> StepValues
dyeingImpacts impacts dyeingProcess heatProcess elecProcess baseMass =
    let
        heatMJ =
            Mass.inKilograms baseMass
                * Energy.inMegajoules dyeingProcess.heat
                |> Energy.megajoules

        kwh =
            Mass.inKilograms baseMass
                * Energy.inMegajoules dyeingProcess.elec
                |> Energy.megajoules
    in
    { heat = heatMJ
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ baseMass |> Unit.forKg (Process.getImpact trigram dyeingProcess)
                        , heatMJ |> Unit.forMJ (Process.getImpact trigram heatProcess)
                        , kwh |> Unit.forKWh (Process.getImpact trigram elecProcess)
                        ]
                )
    , kwh = kwh
    }


printingImpacts :
    Impacts
    ->
        { elecProcess : Process -- Outbound: country electricity impact
        , heatProcess : Process -- Outbound: country heat impact
        , printingProcess : Process -- Inbound: Printing process
        , ratio : Split
        , surfaceMass : Unit.SurfaceMass
        }
    -> Mass
    -> StepValues
printingImpacts impacts { elecProcess, heatProcess, printingProcess, ratio, surfaceMass } baseMass =
    let
        surface =
            Unit.surfaceMassToSurface surfaceMass baseMass
                |> Area.inSquareMeters
                -- Apply ratio
                |> (\surfaceInSquareMeters -> Split.apply surfaceInSquareMeters ratio)

        ( heatMJ, kwh ) =
            -- Note: printing processes heat and elec values are expressed "per square meter"
            ( Quantity.multiplyBy surface printingProcess.heat
            , Quantity.multiplyBy surface printingProcess.elec
            )
    in
    { heat = heatMJ
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ baseMass |> Unit.forKg (Process.getImpact trigram printingProcess)
                        , heatMJ |> Unit.forMJ (Process.getImpact trigram heatProcess)
                        , kwh |> Unit.forKWh (Process.getImpact trigram elecProcess)
                        ]
                )
    , kwh = kwh
    }


finishingImpacts :
    Impacts
    ->
        { elecProcess : Process -- Outbound: country electricity impact
        , finishingProcess : Process -- Inbound: Printing process
        , heatProcess : Process -- Outbound: country heat impact
        }
    -> Mass
    -> StepValues
finishingImpacts impacts { elecProcess, finishingProcess, heatProcess } baseMass =
    let
        ( heatMJ, kwh ) =
            ( Quantity.multiplyBy (Mass.inKilograms baseMass) finishingProcess.heat
            , Quantity.multiplyBy (Mass.inKilograms baseMass) finishingProcess.elec
            )
    in
    { heat = heatMJ
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ baseMass |> Unit.forKg (Process.getImpact trigram finishingProcess)
                        , heatMJ |> Unit.forMJ (Process.getImpact trigram heatProcess)
                        , kwh |> Unit.forKWh (Process.getImpact trigram elecProcess)
                        ]
                )
    , kwh = kwh
    }


getAquaticPollutionRealRatio : Country.AquaticPollutionScenario -> Float
getAquaticPollutionRealRatio scenario =
    -- The toxicity impacts in the "enriched" ennobling processes
    -- "bleaching", "printing-dyes" and "printing-paste",  are based
    -- on the "average" value.
    -- To have the real ratio, we need to do:
    -- ratio / average
    let
        countryRatio =
            Country.getAquaticPollutionRatio scenario |> Split.toFloat

        averageRatio =
            Country.getAquaticPollutionRatio Country.Average |> Split.toFloat
    in
    countryRatio / averageRatio


bleachingImpacts :
    Impacts
    ->
        { aquaticPollutionScenario : Country.AquaticPollutionScenario
        , bleachingProcess : Process -- Inbound: Bleaching process
        }
    -> Mass
    -> Impacts
bleachingImpacts impacts { aquaticPollutionScenario, bleachingProcess } baseMass =
    impacts
        |> Impact.mapImpacts
            (\trigram _ ->
                baseMass
                    |> Unit.forKg (Process.getImpact trigram bleachingProcess)
                    |> Quantity.multiplyBy (getAquaticPollutionRealRatio aquaticPollutionScenario)
            )


materialDyeingToxicityImpacts :
    Impacts
    ->
        { aquaticPollutionScenario : Country.AquaticPollutionScenario
        , dyeingToxicityProcess : Process -- Inbound: dyeing process
        }
    -> Mass
    -> Split
    -> Impacts
materialDyeingToxicityImpacts impacts { aquaticPollutionScenario, dyeingToxicityProcess } baseMass split =
    impacts
        |> Impact.mapImpacts
            (\trigram _ ->
                baseMass
                    |> Unit.forKg (Process.getImpact trigram dyeingToxicityProcess)
                    |> Quantity.multiplyBy (getAquaticPollutionRealRatio aquaticPollutionScenario)
                    |> (\impact -> Split.applyToQuantity impact split)
            )


materialPrintingToxicityImpacts :
    Impacts
    ->
        { aquaticPollutionScenario : Country.AquaticPollutionScenario
        , printingToxicityProcess : Process -- Inbound: printing process
        }
    -> Split
    -> Mass
    -> Impacts
materialPrintingToxicityImpacts impacts { aquaticPollutionScenario, printingToxicityProcess } split baseMass =
    impacts
        |> Impact.mapImpacts
            (\trigram _ ->
                baseMass
                    |> Unit.forKg (Process.getImpact trigram printingToxicityProcess)
                    |> Quantity.multiplyBy (getAquaticPollutionRealRatio aquaticPollutionScenario)
                    |> (\impact -> Split.applyToQuantity impact split)
            )


makingImpacts :
    Impacts
    ->
        { countryElecProcess : Process
        , countryHeatProcess : Process
        , fadingProcess : Maybe Process
        , makingComplexity : MakingComplexity
        }
    -> Mass
    -> StepValues
makingImpacts impacts { countryElecProcess, countryHeatProcess, fadingProcess, makingComplexity } outputMass =
    -- Note: Fading, when enabled, is applied at the Making step because
    -- it can only be applied on finished products (using step output mass).
    -- Also:
    -- - Making impacts are precomputed per "item" (not mass-dependent)
    -- - Fading process, when defined, is mass-dependent
    let
        ( fadingElec, fadingHeat ) =
            ( fadingProcess
                |> Maybe.map .elec
                |> Maybe.withDefault Quantity.zero
                |> Quantity.multiplyBy (Mass.inKilograms outputMass)
            , fadingProcess
                |> Maybe.map .heat
                |> Maybe.withDefault Quantity.zero
                |> Quantity.multiplyBy (Mass.inKilograms outputMass)
            )

        -- Pre-computed constant: energy needed per minute of confection
        kWhPerMinute =
            Energy.kilowattHours 0.029

        elec =
            Quantity.multiplyBy (MakingComplexity.toDuration makingComplexity |> Duration.inMinutes) kWhPerMinute
    in
    { heat = fadingHeat
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ -- Making process (per-item)
                          elec
                            |> Unit.forKWh (Process.getImpact trigram countryElecProcess)

                        -- Fading process (mass-dependent)
                        , outputMass
                            |> Unit.forKg
                                (fadingProcess
                                    |> Maybe.map (Process.getImpact trigram)
                                    |> Maybe.withDefault Quantity.zero
                                )
                        , fadingElec
                            |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                        , fadingHeat
                            |> Unit.forMJ (Process.getImpact trigram countryHeatProcess)
                        ]
                )
    , kwh = Quantity.sum [ elec, fadingElec ]
    }


knittingImpacts :
    Impacts
    -> { countryElecProcess : Process, elec : Energy }
    -> Mass
    ->
        { impacts : Impacts
        , kwh : Energy
        , picking : Maybe Unit.PickPerMeter
        , threadDensity : Maybe Unit.ThreadDensity
        }
knittingImpacts impacts { countryElecProcess, elec } baseMass =
    let
        electricityKWh =
            Energy.kilowattHours
                (Mass.inKilograms baseMass * Energy.inKilowattHours elec)
    in
    -- FIXME: why don't we use threadDensity and picking here?
    { impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    electricityKWh
                        |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                )
    , kwh = electricityKWh
    , picking = Nothing
    , threadDensity = Nothing
    }


weavingImpacts :
    Impacts
    ->
        { countryElecProcess : Process
        , outputMass : Mass
        , pickingElec : Float
        , surfaceMass : Unit.SurfaceMass
        , yarnSize : Unit.YarnSize
        }
    ->
        { impacts : Impacts
        , kwh : Energy
        , picking : Maybe Unit.PickPerMeter
        , threadDensity : Maybe Unit.ThreadDensity
        }
weavingImpacts impacts { countryElecProcess, outputMass, pickingElec, surfaceMass, yarnSize } =
    -- Methodology: https://fabrique-numerique.gitbook.io/ecobalyse/textile/etapes-du-cycle-de-vie/tricotage-tissage
    let
        outputSurface =
            Unit.surfaceMassToSurface surfaceMass outputMass

        threadDensity =
            computeThreadDensity surfaceMass yarnSize

        picking =
            computePicking threadDensity outputSurface

        -- Note: pickingElec is expressed in kWh/(pick,m) per kg of material to process (see Base Impacts)
        electricityKWh =
            pickingElec
                * Unit.pickPerMeterToFloat picking
                |> Energy.kilowattHours
    in
    { impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    electricityKWh
                        |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                )
    , kwh = electricityKWh
    , picking = Just picking
    , threadDensity = Just threadDensity
    }


computeThreadDensity : Unit.SurfaceMass -> Unit.YarnSize -> Unit.ThreadDensity
computeThreadDensity surfaceMass yarnSize =
    -- Densité de fils (# fils/cm) = Grammage(g/m2) * Titrage (Nm) / 100 / 2 / wasteRatio
    let
        -- Taux d'embuvage/retrait = 8% (valeur constante)
        wasteRatio =
            1.08
    in
    toFloat (Unit.surfaceMassInGramsPerSquareMeters surfaceMass)
        * Unit.yarnSizeInKilometers yarnSize
        -- the output surface is in (m2) so we would have the threadDensity is in (# fils / m) but we need it in (# fils / cm)
        / 100
        -- the thread is weaved horizontally and vertically, so the number of threads along one axis is only half of the total thread length
        / 2
        / wasteRatio
        |> Unit.threadDensity


computePicking : Unit.ThreadDensity -> Area -> Unit.PickPerMeter
computePicking threadDensity outputSurface =
    -- Duites.m = Densité de fils (# fils / cm) * Surface sortante (m2) * 100
    Unit.threadDensityToFloat threadDensity
        * Area.inSquareMeters outputSurface
        -- threadDensity is in (# fils / cm) but we need it in (# fils / m) to be in par with the output surface in (m2)
        * 100
        |> round
        |> Unit.PickPerMeter


useImpacts :
    Impacts
    ->
        { countryElecProcess : Process
        , ironingElec : Energy
        , nonIroningProcess : Process
        , useNbCycles : Int
        }
    -> Mass
    -> StepValues
useImpacts impacts { countryElecProcess, ironingElec, nonIroningProcess, useNbCycles } baseMass =
    let
        totalEnergy =
            -- Note: Ironing is expressed per-item, non-ironing is mass-depdendent
            [ ironingElec
            , nonIroningProcess.elec
                |> Quantity.multiplyBy (Mass.inKilograms baseMass)
            ]
                |> Quantity.sum
                |> Quantity.multiplyBy (toFloat useNbCycles)
    in
    { heat = Quantity.zero
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ totalEnergy
                            |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                        , baseMass
                            |> Unit.forKg (Process.getImpact trigram nonIroningProcess)
                            |> Quantity.multiplyBy (toFloat useNbCycles)
                        ]
                )
    , kwh = totalEnergy
    }


endOfLifeImpacts :
    Impacts
    ->
        { countryElecProcess : Process
        , endOfLife : Process
        , heatProcess : Process
        , passengerCar : Process
        , volume : Volume
        }
    -> Mass
    -> StepValues
endOfLifeImpacts impacts { countryElecProcess, endOfLife, heatProcess, passengerCar, volume } baseMass =
    -- Notes:
    -- - passengerCar is expressed per-item
    -- - endOfLife is mass-dependent
    -- - a typical car trunk is 0.2m³ average
    let
        carTrunkAllocationRatio =
            volume
                |> Quantity.divideBy 0.2
                |> Volume.inCubicMeters

        ( elecEnergy, heatEnergy ) =
            ( Quantity.sum
                [ passengerCar.elec
                    |> Quantity.multiplyBy carTrunkAllocationRatio
                , endOfLife.elec
                    |> Quantity.multiplyBy (Mass.inKilograms baseMass)
                ]
            , Quantity.sum
                [ passengerCar.heat
                    |> Quantity.multiplyBy carTrunkAllocationRatio
                , endOfLife.heat
                    |> Quantity.multiplyBy (Mass.inKilograms baseMass)
                ]
            )
    in
    { heat = heatEnergy
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ Process.getImpact trigram passengerCar
                            |> Quantity.multiplyBy carTrunkAllocationRatio
                        , elecEnergy
                            |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                        , heatEnergy
                            |> Unit.forMJ (Process.getImpact trigram heatProcess)
                        , baseMass
                            |> Unit.forKg (Process.getImpact trigram endOfLife)
                        ]
                )
    , kwh = elecEnergy
    }



-- Transports


transportRatio : Split -> Transport -> Transport
transportRatio airTransportRatio ({ road, sea, air } as transport) =
    let
        roadRatio =
            Transport.roadSeaTransportRatio transport

        seaRatio =
            Split.complement roadRatio
    in
    { transport
        | air = air |> Quantity.multiplyBy (Split.toFloat airTransportRatio)
        , road = road |> Quantity.multiplyBy (Split.apply (Split.toFloat roadRatio) (Split.complement airTransportRatio))
        , sea = sea |> Quantity.multiplyBy (Split.apply (Split.toFloat seaRatio) (Split.complement airTransportRatio))
    }

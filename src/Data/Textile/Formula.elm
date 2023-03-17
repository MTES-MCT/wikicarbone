module Data.Textile.Formula exposing
    ( dyeingImpacts
    , endOfLifeImpacts
    , finishingImpacts
    , genericWaste
    , knittingImpacts
    , makingImpacts
    , makingWaste
    , printingImpacts
    , pureMaterialImpacts
    , recycledMaterialImpacts
    , spinningImpacts
    , transportRatio
    , useImpacts
    , weavingImpacts
    )

import Data.Impact as Impact exposing (Impacts)
import Data.Split as Split exposing (Split)
import Data.Textile.Material exposing (CFFData)
import Data.Textile.Process as Process exposing (Process)
import Data.Transport as Transport exposing (Transport)
import Data.Unit as Unit
import Energy exposing (Energy)
import Mass exposing (Mass)
import Quantity
import Volume exposing (Volume)



-- Waste


{-| Compute source mass needed and waste generated by the operation.
-}
genericWaste : Mass -> Mass -> { waste : Mass, mass : Mass }
genericWaste processWaste baseMass =
    let
        waste =
            baseMass
                |> Quantity.multiplyBy (Mass.inKilograms processWaste)
    in
    { waste = waste, mass = baseMass |> Quantity.plus waste }


{-| Compute source material mass needed and waste generated by the operation, according to
material & product waste data.
-}
makingWaste :
    { processWaste : Mass
    , pcrWaste : Split
    }
    -> Mass
    -> { waste : Mass, mass : Mass }
makingWaste { processWaste, pcrWaste } baseMass =
    let
        mass =
            -- (product weight + textile waste for confection) / (1 - PCR product waste rate)
            Mass.kilograms <|
                (Mass.inKilograms baseMass + (Mass.inKilograms baseMass * Mass.inKilograms processWaste))
                    / (Split.complement pcrWaste |> Split.asFloat)
    in
    { waste = Quantity.minus baseMass mass, mass = mass }



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
    -> { recycledProcess : Process, nonRecycledProcess : Process, cffData : CFFData }
    -> Mass
    -> Impacts
recycledMaterialImpacts impacts { recycledProcess, nonRecycledProcess, cffData } outputMass =
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
                    * (Unit.ratioToFloat manufacturerAllocation
                        * recycledImpactPerKg
                        + (1 - Unit.ratioToFloat manufacturerAllocation)
                        * Unit.ratioToFloat recycledQualityRatio
                        * nonRecycledImpactPerKg
                      )
                    |> Unit.impact
            )


spinningImpacts :
    Impacts
    -> { spinningProcess : Process, countryElecProcess : Process }
    -> Mass
    -> { kwh : Energy, impacts : Impacts }
spinningImpacts impacts { spinningProcess, countryElecProcess } mass =
    let
        kwh =
            spinningProcess.elec
                |> Quantity.multiplyBy (Mass.inKilograms mass)
    in
    { kwh = kwh
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    kwh |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                )
    }


dyeingImpacts :
    Impacts
    -> Process -- Inbound: Dyeing process
    -> Process -- Outbound: country heat impact
    -> Process -- Outbound: country electricity impact
    -> Mass
    -> { heat : Energy, kwh : Energy, impacts : Impacts }
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
    , kwh = kwh
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
    }


printingImpacts :
    Impacts
    ->
        { printingProcess : Process -- Inbound: Printing process
        , heatProcess : Process -- Outbound: country heat impact
        , elecProcess : Process -- Outbound: country electricity impact
        , surfaceMass : Unit.SurfaceMass
        , ratio : Unit.Ratio
        }
    -> Mass
    -> { heat : Energy, kwh : Energy, impacts : Impacts }
printingImpacts impacts { printingProcess, heatProcess, elecProcess, surfaceMass, ratio } baseMass =
    let
        surface =
            -- area (m2) = mass (g) / surfaceMass (g/m2)
            Mass.inGrams baseMass
                / Unit.surfaceMassToFloat surfaceMass
                -- Apply ratio
                * Unit.ratioToFloat ratio

        ( heatMJ, kwh ) =
            -- Note: printing processes heat and elec values are expressed "per square meter"
            ( Quantity.multiplyBy surface printingProcess.heat
            , Quantity.multiplyBy surface printingProcess.elec
            )
    in
    { heat = heatMJ
    , kwh = kwh
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
    }


finishingImpacts :
    Impacts
    ->
        { finishingProcess : Process -- Inbound: Printing process
        , heatProcess : Process -- Outbound: country heat impact
        , elecProcess : Process -- Outbound: country electricity impact
        }
    -> Mass
    -> { heat : Energy, kwh : Energy, impacts : Impacts }
finishingImpacts impacts { finishingProcess, heatProcess, elecProcess } baseMass =
    let
        ( heatMJ, kwh ) =
            ( Quantity.multiplyBy (Mass.inKilograms baseMass) finishingProcess.heat
            , Quantity.multiplyBy (Mass.inKilograms baseMass) finishingProcess.elec
            )
    in
    { heat = heatMJ
    , kwh = kwh
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
    }


makingImpacts :
    Impacts
    ->
        { makingProcess : Process
        , fadingProcess : Maybe Process
        , countryElecProcess : Process
        , countryHeatProcess : Process
        }
    -> Mass
    -> { kwh : Energy, heat : Energy, impacts : Impacts }
makingImpacts impacts { makingProcess, fadingProcess, countryElecProcess, countryHeatProcess } outputMass =
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
    in
    { kwh = Quantity.sum [ makingProcess.elec, fadingElec ]
    , heat = Quantity.sum [ makingProcess.heat, fadingHeat ]
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ -- Making process (per-item)
                          makingProcess.elec
                            |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                        , makingProcess.heat
                            |> Unit.forMJ (Process.getImpact trigram countryElecProcess)

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
    }


knittingImpacts :
    Impacts
    -> { elec : Energy, countryElecProcess : Process }
    -> Mass
    -> { kwh : Energy, impacts : Impacts }
knittingImpacts impacts { elec, countryElecProcess } baseMass =
    let
        electricityKWh =
            Energy.kilowattHours
                (Mass.inKilograms baseMass * Energy.inKilowattHours elec)
    in
    { kwh = electricityKWh
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    electricityKWh
                        |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                )
    }


weavingImpacts :
    Impacts
    ->
        { pickingElec : Float
        , countryElecProcess : Process
        , picking : Unit.PickPerMeter
        , surfaceMass : Unit.SurfaceMass
        }
    -> Mass
    -> { kwh : Energy, impacts : Impacts }
weavingImpacts impacts { pickingElec, countryElecProcess, picking, surfaceMass } baseMass =
    let
        electricityKWh =
            (Mass.inKilograms baseMass * 1000 * Unit.pickPerMeterToFloat picking / Unit.surfaceMassToFloat surfaceMass)
                * pickingElec
                |> Energy.kilowattHours
    in
    { kwh = electricityKWh
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    electricityKWh
                        |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                )
    }


useImpacts :
    Impacts
    ->
        { useNbCycles : Int
        , ironingProcess : Process
        , nonIroningProcess : Process
        , countryElecProcess : Process
        }
    -> Mass
    -> { kwh : Energy, impacts : Impacts }
useImpacts impacts { useNbCycles, ironingProcess, nonIroningProcess, countryElecProcess } baseMass =
    let
        totalEnergy =
            -- Note: Ironing is expressed per-item, non-ironing is mass-depdendent
            [ ironingProcess.elec
            , nonIroningProcess.elec
                |> Quantity.multiplyBy (Mass.inKilograms baseMass)
            ]
                |> Quantity.sum
                |> Quantity.multiplyBy (toFloat useNbCycles)
    in
    { kwh = totalEnergy
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    Quantity.sum
                        [ totalEnergy
                            |> Unit.forKWh (Process.getImpact trigram countryElecProcess)
                        , Process.getImpact trigram ironingProcess
                            |> Quantity.multiplyBy (toFloat useNbCycles)
                        , baseMass
                            |> Unit.forKg (Process.getImpact trigram nonIroningProcess)
                            |> Quantity.multiplyBy (toFloat useNbCycles)
                        ]
                )
    }


endOfLifeImpacts :
    Impacts
    ->
        { volume : Volume
        , passengerCar : Process
        , endOfLife : Process
        , countryElecProcess : Process
        , heatProcess : Process
        }
    -> Mass
    -> { kwh : Energy, heat : Energy, impacts : Impacts }
endOfLifeImpacts impacts { volume, passengerCar, endOfLife, countryElecProcess, heatProcess } baseMass =
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
    { kwh = elecEnergy
    , heat = heatEnergy
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
        | road = road |> Quantity.multiplyBy (Split.apply (Split.asFloat roadRatio) (Split.complement airTransportRatio))
        , sea = sea |> Quantity.multiplyBy (Split.apply (Split.asFloat seaRatio) (Split.complement airTransportRatio))
        , air = air |> Quantity.multiplyBy (Split.asFloat airTransportRatio)
    }

module Data.Formula exposing
    ( dyeingImpacts
    , endOfLifeImpacts
    , genericWaste
    , knittingImpacts
    , makingImpacts
    , makingWaste
    , materialAndSpinningImpacts
    , materialRecycledWaste
    , pureMaterialAndSpinningImpacts
    , transportRatio
    , useImpacts
    , weavingImpacts
    )

import Data.Impact as Impact exposing (Impacts)
import Data.Material exposing (CFFData)
import Data.Process as Process exposing (Process)
import Data.Transport as Transport exposing (Transport)
import Data.Unit as Unit
import Energy exposing (Energy)
import Mass exposing (Mass)
import Quantity
import Volume exposing (Volume)



-- Waste


{-| Compute source material mass needed and waste generated by the operation.
-}
genericWaste : Mass -> Mass -> { waste : Mass, mass : Mass }
genericWaste processWaste baseMass =
    let
        waste =
            baseMass
                |> Quantity.multiplyBy (Mass.inKilograms processWaste)
    in
    { waste = waste, mass = baseMass |> Quantity.plus waste }


{-| Compute source material mass needed and waste generated by the operation from
ratioed pristine/recycled material processes data.
-}
materialRecycledWaste :
    { pristineWaste : Mass
    , recycledWaste : Mass
    , recycledRatio : Unit.Ratio
    }
    -> Mass
    -> { waste : Mass, mass : Mass }
materialRecycledWaste { pristineWaste, recycledWaste, recycledRatio } baseMass =
    let
        ( recycledMass, pristineMass ) =
            ( baseMass |> Quantity.multiplyBy (Unit.ratioToFloat recycledRatio)
            , baseMass |> Quantity.multiplyBy (1 - Unit.ratioToFloat recycledRatio)
            )

        ( ratioedRecycledWaste, ratioedPristineWaste ) =
            ( recycledMass |> Quantity.multiplyBy (Mass.inKilograms recycledWaste)
            , pristineMass |> Quantity.multiplyBy (Mass.inKilograms pristineWaste)
            )

        waste =
            Quantity.plus ratioedRecycledWaste ratioedPristineWaste
    in
    { waste = waste
    , mass = Quantity.sum [ pristineMass, recycledMass, waste ]
    }


{-| Compute source material mass needed and waste generated by the operation, according to
material & product waste data.
-}
makingWaste :
    { processWaste : Mass
    , pcrWaste : Unit.Ratio
    }
    -> Mass
    -> { waste : Mass, mass : Mass }
makingWaste { processWaste, pcrWaste } baseMass =
    let
        mass =
            -- (product weight + textile waste for confection) / (1 - PCR product waste rate)
            Mass.kilograms <|
                (Mass.inKilograms baseMass + (Mass.inKilograms baseMass * Mass.inKilograms processWaste))
                    / (1 - Unit.ratioToFloat pcrWaste)
    in
    { waste = Quantity.minus baseMass mass, mass = mass }



-- Impacts


materialAndSpinningImpacts :
    Impacts
    -> ( Process, Process ) -- Inbound: Material processes (recycled, non-recycled)
    -> Unit.Ratio -- Ratio of recycled material (bewteen 0 and 1)
    -> Maybe CFFData -- Circular Footprint Formula data
    -> Mass
    -> Impacts
materialAndSpinningImpacts impacts ( recycledProcess, nonRecycledProcess ) ratio cffData mass =
    impacts
        |> Impact.mapImpacts
            (\trigram _ ->
                case cffData of
                    Just { manufacturerAllocation, recycledQualityRatio } ->
                        -- CFF
                        -- A: manufacturerAllocation
                        -- Qsin/Qp: recycledQualityRatio
                        -- Impact_coton =  0.6 * m * Impact_coton_par_kg
                        -- Impact_coton_recyclé = 0.4 * m ( A * Impact_coton_recyclé_par_kg + (1-A) * Qsin/Qp * Impact_coton_par_kg)
                        let
                            ( recycledImpactPerKg, nonRecycledImpactPerKg ) =
                                ( Process.getImpact trigram recycledProcess |> Unit.impactToFloat
                                , Process.getImpact trigram nonRecycledProcess |> Unit.impactToFloat
                                )

                            nonRecycledImpact =
                                (1 - Unit.ratioToFloat ratio)
                                    * Mass.inKilograms mass
                                    * nonRecycledImpactPerKg

                            recycledImpact =
                                Unit.ratioToFloat ratio
                                    * Mass.inKilograms mass
                                    * (Unit.ratioToFloat manufacturerAllocation
                                        * recycledImpactPerKg
                                        + (1 - Unit.ratioToFloat manufacturerAllocation)
                                        * Unit.ratioToFloat recycledQualityRatio
                                        * nonRecycledImpactPerKg
                                      )
                        in
                        Quantity.sum
                            [ Unit.impact nonRecycledImpact
                            , Unit.impact recycledImpact
                            ]

                    Nothing ->
                        mass
                            |> Unit.ratioedForKg
                                ( Process.getImpact trigram recycledProcess
                                , Process.getImpact trigram nonRecycledProcess
                                )
                                ratio
            )


pureMaterialAndSpinningImpacts : Impacts -> Process -> Mass -> Impacts
pureMaterialAndSpinningImpacts impacts process mass =
    impacts
        |> Impact.mapImpacts
            (\trigram _ ->
                mass
                    |> Unit.forKg (Process.getImpact trigram process)
            )


dyeingImpacts :
    Impacts
    -> ( Process, Process ) -- Inbound: Dyeing processes (low, high)
    -> Unit.Ratio -- Low/high dyeing process ratio
    -> Process -- Outbound: country heat impact
    -> Process -- Outbound: country electricity impact
    -> Mass
    -> { heat : Energy, kwh : Energy, impacts : Impacts }
dyeingImpacts impacts ( dyeingLowProcess, dyeingHighProcess ) (Unit.Ratio highDyeingWeighting) heatProcess elecProcess baseMass =
    let
        lowDyeingWeighting =
            1 - highDyeingWeighting

        ( lowDyeingMass, highDyeingMass ) =
            ( baseMass |> Quantity.multiplyBy lowDyeingWeighting
            , baseMass |> Quantity.multiplyBy highDyeingWeighting
            )

        heatMJ =
            Mass.inKilograms baseMass
                * ((highDyeingWeighting * Energy.inMegajoules dyeingHighProcess.heat)
                    + (lowDyeingWeighting * Energy.inMegajoules dyeingLowProcess.heat)
                  )
                |> Energy.megajoules

        electricity =
            Mass.inKilograms baseMass
                * ((highDyeingWeighting * Energy.inMegajoules dyeingHighProcess.elec)
                    + (lowDyeingWeighting * Energy.inMegajoules dyeingLowProcess.elec)
                  )
                |> Energy.megajoules
    in
    { heat = heatMJ
    , kwh = electricity
    , impacts =
        impacts
            |> Impact.mapImpacts
                (\trigram _ ->
                    let
                        dyeingImpact_ =
                            Quantity.sum
                                [ Unit.forKg (Process.getImpact trigram dyeingLowProcess) lowDyeingMass
                                , Unit.forKg (Process.getImpact trigram dyeingHighProcess) highDyeingMass
                                ]

                        heatImpact =
                            heatMJ |> Unit.forMJ (Process.getImpact trigram heatProcess)

                        elecImpact =
                            electricity |> Unit.forKWh (Process.getImpact trigram elecProcess)
                    in
                    Quantity.sum [ dyeingImpact_, heatImpact, elecImpact ]
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


transportRatio : Unit.Ratio -> Transport -> Transport
transportRatio airTransportRatio ({ road, sea, air } as transport) =
    let
        roadRatio =
            Transport.roadSeaTransportRatio transport

        seaRatio =
            1 - roadRatio
    in
    { transport
        | road = road |> Quantity.multiplyBy (roadRatio * (1 - Unit.ratioToFloat airTransportRatio))
        , sea = sea |> Quantity.multiplyBy (seaRatio * (1 - Unit.ratioToFloat airTransportRatio))
        , air = air |> Quantity.multiplyBy (Unit.ratioToFloat airTransportRatio)
    }

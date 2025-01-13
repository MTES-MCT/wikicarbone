module Data.Textile.SimulatorTest exposing (..)

import Data.Country as Country
import Data.Impact as Impact
import Data.Impact.Definition as Definition
import Data.Split as Split
import Data.Textile.Economics as Economics
import Data.Textile.LifeCycle as LifeCycle
import Data.Textile.Query exposing (Query, tShirtCotonFrance)
import Data.Textile.Simulator as Simulator
import Data.Textile.Step.Label as Label
import Data.Unit as Unit
import Expect exposing (Expectation)
import Static.Db exposing (Db)
import Test exposing (..)
import TestUtils exposing (asTest, suiteWithDb)


getImpact : Db -> Definition.Trigram -> Query -> Result String Float
getImpact db trigram =
    Simulator.compute db
        >> Result.map
            (.impacts
                >> Impact.getImpact trigram
                >> Unit.impactToFloat
            )


expectImpact : Db -> Definition.Trigram -> Float -> Query -> Expectation
expectImpact db trigram value query =
    case getImpact db trigram query of
        Ok result ->
            result
                |> Expect.within (Expect.Absolute 0.01) value

        Err error ->
            Expect.fail error


ecs : Definition.Trigram
ecs =
    Definition.Ecs


suite : Test
suite =
    suiteWithDb "Data.Textile.Simulator"
        (\db ->
            [ describe "Simulator.compute"
                [ { tShirtCotonFrance
                    | countrySpinning = Nothing
                  }
                    |> expectImpact db ecs 1453.0693568489341
                    |> asTest "should compute a simulation ecs impact"
                , describe "disabled steps"
                    [ { tShirtCotonFrance | disabledSteps = [ Label.Ennobling ] }
                        |> Simulator.compute db
                        |> Result.map (.lifeCycle >> LifeCycle.getStepProp Label.Ennobling .enabled True)
                        |> Expect.equal (Ok False)
                        |> asTest "should be handled from passed query"
                    , asTest "should handle disabled steps"
                        (case
                            ( getImpact db ecs tShirtCotonFrance
                            , getImpact db ecs { tShirtCotonFrance | disabledSteps = [ Label.Ennobling ] }
                            )
                         of
                            ( Ok full, Ok partial ) ->
                                full |> Expect.greaterThan partial

                            _ ->
                                Expect.fail "bogus simulator results"
                        )
                    , asTest "should allow disabling steps"
                        (case
                            ( getImpact db ecs tShirtCotonFrance
                            , getImpact db ecs { tShirtCotonFrance | disabledSteps = [ Label.Ennobling ] }
                            )
                         of
                            ( Ok full, Ok partial ) ->
                                full |> Expect.greaterThan partial

                            _ ->
                                Expect.fail "bogus simulator results"
                        )
                    ]
                ]
            , let
                tShirtCotonWithSmallerPhysicalDurability =
                    { tShirtCotonFrance
                        | numberOfReferences = Just 10
                        , price = Just <| Economics.priceFromFloat 100
                        , physicalDurability = Just <| Unit.physicalDurability 1
                    }
              in
              describe "compute holistic durability"
                [ tShirtCotonFrance
                    |> Simulator.compute db
                    |> Result.map .durability
                    |> Expect.equal (Ok { physical = Unit.physicalDurability 1.45, nonPhysical = Unit.nonPhysicalDurability 0.67 })
                    |> asTest "should have default durability"
                , { physical = Unit.physicalDurability 1.45, nonPhysical = Unit.nonPhysicalDurability 0.67 }
                    |> Unit.floatDurabilityFromHolistic
                    |> Expect.within (Expect.Absolute 0.001) 0.67
                    |> asTest "should take the min of the two durabilities"
                , tShirtCotonWithSmallerPhysicalDurability
                    |> Simulator.compute db
                    |> Result.map .durability
                    |> Expect.equal (Ok { physical = Unit.physicalDurability 1, nonPhysical = Unit.nonPhysicalDurability 1.19 })
                    |> asTest "should take into account when non physical durability changes"
                , tShirtCotonWithSmallerPhysicalDurability
                    |> Simulator.compute db
                    |> Result.map (.durability >> Unit.floatDurabilityFromHolistic)
                    |> Expect.equal (Ok 1)
                    |> asTest "should return non physical durability if it is the smallest"
                ]
            , let
                tShirtCotonWithSmallerPhysicalDurabilityCn =
                    { tShirtCotonFrance
                        | numberOfReferences = Just 10
                        , price = Just <| Economics.priceFromFloat 100
                        , physicalDurability = Just <| Unit.physicalDurability 1.1
                        , countryMaking = Just (Country.Code "CN")
                    }
              in
              describe "compute airTransporRatio"
                [ tShirtCotonFrance
                    |> Simulator.compute db
                    |> Result.map (.lifeCycle >> LifeCycle.getStepProp Label.Making .airTransportRatio Split.half)
                    |> Expect.equal (Ok Split.zero)
                    |> asTest "should be zero for products from Europe or Turkey"
                , { tShirtCotonFrance | countryMaking = Just (Country.Code "CN") }
                    |> Simulator.compute db
                    |> Result.map (.lifeCycle >> LifeCycle.getStepProp Label.Making .airTransportRatio Split.half)
                    |> Expect.equal (Ok Split.full)
                    |> asTest "should be full for products not coming from Europe or Turkey"
                , tShirtCotonWithSmallerPhysicalDurabilityCn
                    |> Simulator.compute db
                    |> Result.map (.lifeCycle >> LifeCycle.getStepProp Label.Making .airTransportRatio Split.half)
                    |> Expect.equal (Ok Split.third)
                    |> asTest "should be 0.33 for products not coming from Europe or Turkey but with a durability >= 1"
                , { tShirtCotonFrance
                    | countryMaking = Just (Country.Code "CN")
                    , airTransportRatio = Just Split.two
                  }
                    |> Simulator.compute db
                    |> Result.map (.lifeCycle >> LifeCycle.getStepProp Label.Making .airTransportRatio Split.half)
                    |> Expect.equal (Ok Split.two)
                    |> asTest "should keep the user provided value"
                ]
            , describe "getTotalImpactsWithoutComplements"
                [ tShirtCotonFrance
                    |> Simulator.compute db
                    |> Result.map
                        (Simulator.getTotalImpactsWithoutComplements
                            >> Impact.getImpact Definition.Ecs
                            >> Unit.impactToFloat
                        )
                    |> Result.withDefault 0
                    |> Expect.greaterThan 0
                    |> asTest "should compute total impacts without complements"
                ]
            ]
        )

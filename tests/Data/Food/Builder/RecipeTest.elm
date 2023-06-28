module Data.Food.Builder.RecipeTest exposing (..)

import Data.Country as Country
import Data.Food.Builder.Query as Query exposing (carrotCake)
import Data.Food.Builder.Recipe as Recipe
import Data.Food.Ingredient as Ingredient
import Data.Food.Preparation as Preparation
import Data.Food.Process as Process
import Data.Food.Retail as Retail
import Data.Impact as Impact
import Data.Impact.Definition as Definition
import Data.Split as Split
import Data.Unit as Unit
import Expect
import Length
import Mass
import Test exposing (..)
import TestUtils exposing (asTest, suiteWithDb)


expectImpactEqual : Unit.Impact -> Unit.Impact -> Expect.Expectation
expectImpactEqual expectedImpactUnit =
    let
        expectedImpact =
            Unit.impactToFloat expectedImpactUnit
    in
    Unit.impactToFloat
        >> Expect.within (Expect.Relative 0.0000000000000001) expectedImpact


suite : Test
suite =
    suiteWithDb "Data.Food.Builder.Recipe"
        (\{ builderDb } ->
            [ let
                testComputedBonuses bonuses =
                    Impact.empty
                        |> Impact.updateImpact builderDb.impactDefinitions Definition.Ecs (Unit.impact 1000)
                        |> Impact.updateImpact builderDb.impactDefinitions Definition.Ldu (Unit.impact 100)
                        |> Recipe.computeIngredientBonusesImpacts builderDb.impactDefinitions bonuses
              in
              describe "computeIngredientBonusesImpacts"
                [ describe "with zero bonuses applied"
                    (let
                        bonusImpacts =
                            testComputedBonuses
                                { agroDiversity = Split.zero
                                , agroEcology = Split.zero
                                , animalWelfare = Split.zero
                                }
                     in
                     [ bonusImpacts.agroDiversity
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero agro-diversity ingredient bonus"
                     , bonusImpacts.agroEcology
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero agro-ecology ingredient bonus"
                     , bonusImpacts.animalWelfare
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero animal-welfare ingredient bonus"
                     , bonusImpacts.total
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero total bonus"
                     ]
                    )
                , describe "with non-zero bonuses applied"
                    (let
                        bonusImpacts =
                            testComputedBonuses
                                { agroDiversity = Split.half
                                , agroEcology = Split.half
                                , animalWelfare = Split.half
                                }
                     in
                     [ bonusImpacts.agroDiversity
                        |> expectImpactEqual (Unit.impact 8.223326963580142)
                        |> asTest "should compute a non-zero agro-diversity ingredient bonus"
                     , bonusImpacts.agroEcology
                        |> expectImpactEqual (Unit.impact 8.223326963580142)
                        |> asTest "should compute a non-zero agro-ecology ingredient bonus"
                     , bonusImpacts.animalWelfare
                        |> expectImpactEqual (Unit.impact 5.3630393240740055)
                        |> asTest "should compute a non-zero animal-welfare ingredient bonus"
                     , bonusImpacts.total
                        |> expectImpactEqual (Unit.impact 21.80969325123429)
                        |> asTest "should compute a non-zero total bonus"
                     ]
                    )
                , describe "with maluses avoided"
                    (let
                        bonusImpacts =
                            Impact.empty
                                |> Impact.updateImpact builderDb.impactDefinitions Definition.Ecs (Unit.impact 1000)
                                |> Impact.updateImpact builderDb.impactDefinitions Definition.Ldu (Unit.impact -100)
                                |> Recipe.computeIngredientBonusesImpacts builderDb.impactDefinitions
                                    { agroDiversity = Split.full
                                    , agroEcology = Split.full
                                    , animalWelfare = Split.full
                                    }
                     in
                     [ bonusImpacts.agroDiversity
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero agro-diversity ingredient bonus"
                     , bonusImpacts.agroEcology
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero agro-ecology ingredient bonus"
                     , bonusImpacts.animalWelfare
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero animal-welfare ingredient bonus"
                     , bonusImpacts.total
                        |> expectImpactEqual (Unit.impact 0)
                        |> asTest "should compute a zero total bonus"
                     ]
                    )
                ]
            , let
                recipe =
                    carrotCake
                        |> Recipe.fromQuery builderDb
              in
              describe "fromQuery"
                [ recipe
                    |> Expect.ok
                    |> asTest "should return an Ok for a valid query"
                , { carrotCake
                    | transform =
                        Just
                            { code = Process.codeFromString "not a process"
                            , mass = Mass.kilograms 0
                            }
                  }
                    |> Recipe.fromQuery builderDb
                    |> Result.map .transform
                    |> Expect.err
                    |> asTest "should return an Err for an invalid processing"
                , { carrotCake
                    | ingredients =
                        carrotCake.ingredients
                            |> List.map (\ingredient -> { ingredient | planeTransport = Ingredient.ByPlane })
                  }
                    |> Recipe.fromQuery builderDb
                    |> Expect.err
                    |> asTest "should return an Err for an invalid 'planeTransport' value for an ingredient without a default origin by plane"
                ]
            , describe "compute"
                [ describe "standard carrot cake"
                    (let
                        carrotCakeResults =
                            carrotCake
                                |> Recipe.compute builderDb
                     in
                     [ carrotCakeResults
                        |> Result.map (Tuple.second >> .total)
                        |> Result.withDefault Impact.empty
                        |> Expect.all
                            [ \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Acd subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Bvi subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Cch subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Ecs subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Etf subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.EtfC subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Fru subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Fwe subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Htc subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.HtcC subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Htn subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.HtnC subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Ior subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Ldu subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Mru subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Ozd subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Pco subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Pef subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Pma subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Swe subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Tre subject))
                            , \subject -> Expect.greaterThan 0 (Unit.impactToFloat (Impact.getImpact Definition.Wtu subject))
                            ]
                        |> asTest "should return computed impacts where none equals zero"
                     , carrotCakeResults
                        |> Result.map (Tuple.second >> .recipe >> .total >> Impact.getImpact Definition.Ecs)
                        |> Result.map (expectImpactEqual (Unit.impact 108.4322609789048))
                        |> Expect.equal (Ok Expect.pass)
                        |> asTest "should have the total ecs impact with the bonus taken into account"
                     , carrotCakeResults
                        |> Result.map (Tuple.second >> .recipe >> .ingredientsTotal >> Impact.getImpact Definition.Ecs)
                        |> Result.map (expectImpactEqual (Unit.impact 73.23635314324639))
                        |> Expect.equal (Ok Expect.pass)
                        |> asTest "should have the ingredients' total ecs impact with the bonus taken into account"
                     , describe "Scoring"
                        (case carrotCakeResults |> Result.map (Tuple.second >> .scoring) of
                            Err err ->
                                [ Expect.fail err
                                    |> asTest "should not fail"
                                ]

                            Ok scoring ->
                                [ Unit.impactToFloat scoring.all
                                    |> Expect.within (Expect.Absolute 0.01) 190.9
                                    |> asTest "should properly score total impact"
                                , Unit.impactToFloat scoring.allWithoutBonuses
                                    |> Expect.within (Expect.Absolute 0.01) 192.9
                                    |> asTest "should properly score total impact without bonuses"
                                , Unit.impactToFloat scoring.bonuses
                                    |> Expect.within (Expect.Absolute 0.01) 2.0
                                    |> asTest "should properly score bonuses impact"
                                , (Unit.impactToFloat scoring.allWithoutBonuses - Unit.impactToFloat scoring.bonuses)
                                    |> Expect.within (Expect.Absolute 0.0001) (Unit.impactToFloat scoring.all)
                                    |> asTest "should expose coherent scoring"
                                , Unit.impactToFloat scoring.biodiversity
                                    |> Expect.within (Expect.Absolute 0.01) 76.84
                                    |> asTest "should properly score impact on biodiversity protected area"
                                , Unit.impactToFloat scoring.climate
                                    |> Expect.within (Expect.Absolute 0.01) 42.24
                                    |> asTest "should properly score impact on climate protected area"
                                , Unit.impactToFloat scoring.health
                                    |> Expect.within (Expect.Absolute 0.01) 37.64
                                    |> asTest "should properly score impact on health protected area"
                                , Unit.impactToFloat scoring.resources
                                    |> Expect.within (Expect.Absolute 0.01) 36.17
                                    |> asTest "should properly score impact on resources protected area"
                                ]
                        )
                     ]
                    )
                , describe "raw-to-cooked checks"
                    [ -- Carrot cake is cooked at plant, let's apply oven cooking at consumer: the
                      -- raw-to-cooked ratio should have been applied to resulting mass just once.
                      let
                        withPreps preps =
                            { carrotCake | preparation = preps }
                                |> Recipe.compute builderDb
                                |> Result.map (Tuple.second >> .preparedMass >> Mass.inKilograms)
                                |> Result.withDefault 0
                      in
                      withPreps [ Preparation.Id "oven" ]
                        |> Expect.within (Expect.Absolute 0.0001) (withPreps [])
                        |> asTest "should apply raw-to-cooked ratio once"
                    ]
                , describe "custom ingredient bonuses"
                    [ let
                        computeEcoscore =
                            Recipe.compute builderDb
                                >> Result.map (Tuple.second >> .total >> Impact.getImpact Definition.Ecs >> Unit.impactToFloat)
                                >> Result.withDefault 0

                        carrotCakeResults =
                            computeEcoscore carrotCake

                        customBonusesResults =
                            computeEcoscore
                                { carrotCake
                                    | ingredients =
                                        carrotCake.ingredients
                                            |> List.map
                                                (\ingredientQuery ->
                                                    if ingredientQuery.id == Ingredient.Id "carrot" then
                                                        { ingredientQuery
                                                            | bonuses =
                                                                Just
                                                                    { agroDiversity = Split.full
                                                                    , agroEcology = Split.zero
                                                                    , animalWelfare = Split.zero
                                                                    }
                                                        }

                                                    else
                                                        ingredientQuery
                                                )
                                }
                      in
                      carrotCakeResults
                        |> Expect.greaterThan customBonusesResults
                        |> asTest "should apply custom bonuses"
                    ]
                ]
            , describe "getMassAtPackaging"
                [ { ingredients =
                        [ { id = Ingredient.idFromString "egg"
                          , mass = Mass.grams 120
                          , variant = Query.DefaultVariant
                          , country = Nothing
                          , planeTransport = Ingredient.PlaneNotApplicable
                          , bonuses = Nothing
                          }
                        , { id = Ingredient.idFromString "wheat"
                          , mass = Mass.grams 140
                          , variant = Query.DefaultVariant
                          , country = Nothing
                          , planeTransport = Ingredient.PlaneNotApplicable
                          , bonuses = Nothing
                          }
                        ]
                  , transform = Nothing
                  , packaging = []
                  , distribution = Nothing
                  , preparation = []
                  }
                    |> Recipe.compute builderDb
                    |> Result.map (Tuple.first >> Recipe.getMassAtPackaging)
                    |> Expect.equal (Ok (Mass.kilograms 0.26))
                    |> asTest "should compute recipe ingredients mass with no cooking involved"
                , carrotCake
                    |> Recipe.compute builderDb
                    |> Result.map (Tuple.first >> Recipe.getMassAtPackaging)
                    |> Expect.equal (Ok (Mass.kilograms 0.79074))
                    |> asTest "should compute recipe ingredients mass applying raw to cooked ratio"
                ]
            , let
                carrotCakeWithPackaging =
                    carrotCake
                        |> Recipe.compute builderDb
                        |> Result.map (Tuple.first >> Recipe.getTransformedIngredientsMass)

                carrotCakeWithNoPackaging =
                    { carrotCake | packaging = [] }
                        |> Recipe.compute builderDb
                        |> Result.map (Tuple.first >> Recipe.getTransformedIngredientsMass)
              in
              describe "getTransformedIngredientsMass"
                [ carrotCakeWithPackaging
                    |> Expect.equal (Ok (Mass.kilograms 0.68574))
                    |> asTest "should compute recipe treansformed ingredients mass excluding packaging one"
                , carrotCakeWithPackaging
                    |> Expect.equal carrotCakeWithNoPackaging
                    |> asTest "should give the same mass including packaging or not"
                ]
            , let
                mango =
                    { id = Ingredient.idFromString "mango"
                    , mass = Mass.grams 120
                    , variant = Query.DefaultVariant
                    , country = Nothing
                    , planeTransport = Ingredient.ByPlane
                    , bonuses = Nothing
                    }

                firstIngredientAirDistance ( recipe, _ ) =
                    recipe
                        |> .ingredients
                        |> List.head
                        |> Maybe.map (Recipe.computeIngredientTransport builderDb)
                        |> Maybe.map .air
                        |> Maybe.map Length.inKilometers
              in
              describe "computeIngredientTransport"
                [ { ingredients =
                        [ { id = Ingredient.idFromString "egg"
                          , mass = Mass.grams 120
                          , variant = Query.DefaultVariant
                          , country = Nothing
                          , planeTransport = Ingredient.PlaneNotApplicable
                          , bonuses = Nothing
                          }
                        ]
                  , transform = Nothing
                  , packaging = []
                  , distribution = Nothing
                  , preparation = []
                  }
                    |> Recipe.compute builderDb
                    |> Result.map firstIngredientAirDistance
                    |> Expect.equal (Ok (Just 0))
                    |> asTest "should have no air transport for standard ingredients"
                , { ingredients = [ mango ]
                  , transform = Nothing
                  , packaging = []
                  , distribution = Nothing
                  , preparation = []
                  }
                    |> Recipe.compute builderDb
                    |> Result.map firstIngredientAirDistance
                    |> Expect.equal (Ok (Just 18000))
                    |> asTest "should have air transport for mango from its default origin"
                , { ingredients = [ { mango | country = Just (Country.codeFromString "CN"), planeTransport = Ingredient.ByPlane } ]
                  , transform = Nothing
                  , packaging = []
                  , distribution = Just Retail.ambient
                  , preparation = []
                  }
                    |> Recipe.compute builderDb
                    |> Result.map firstIngredientAirDistance
                    |> Expect.equal (Ok (Just 8189))
                    |> asTest "should always have air transport for mango even from other countries if 'planeTransport' is 'byPlane'"
                , { ingredients = [ { mango | country = Just (Country.codeFromString "CN"), planeTransport = Ingredient.NoPlane } ]
                  , transform = Nothing
                  , packaging = []
                  , distribution = Just Retail.ambient
                  , preparation = []
                  }
                    |> Recipe.compute builderDb
                    |> Result.map firstIngredientAirDistance
                    |> Expect.equal (Ok (Just 0))
                    |> asTest "should not have air transport for mango from other countries if 'planeTransport' is 'noPlane'"
                ]
            ]
        )

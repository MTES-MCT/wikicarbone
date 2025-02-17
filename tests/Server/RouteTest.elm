module Server.RouteTest exposing (..)

import Data.Country as Country
import Data.Example as Example
import Data.Food.Query as FoodQuery
import Data.Impact.Definition as Definition
import Data.Split as Split
import Data.Textile.Material as Material
import Data.Textile.Material.Origin as Origin
import Data.Textile.Material.Spinning as Spinning
import Data.Textile.Query as Query exposing (Query, tShirtCotonFrance)
import Data.Textile.Step.Label as Label
import Data.Unit as Unit
import Dict exposing (Dict)
import Expect
import Json.Encode as Encode
import Server.Route as Route
import Static.Db as StaticDb
import Test exposing (..)
import TestUtils exposing (asTest, createServerRequest, suiteWithDb)


sampleQuery : Query
sampleQuery =
    { tShirtCotonFrance | countrySpinning = Nothing }


suite : Test
suite =
    suiteWithDb "Route"
        (\db ->
            [ foodEndpoints db
                |> describe "Food"
            , textileEndpoints db
                |> describe "Textile"
            ]
        )


foodEndpoints : StaticDb.Db -> List Test
foodEndpoints db =
    case
        db.food.examples
            |> Example.findByName "Pizza royale (350g) - 6"
            |> Result.map .query
    of
        Err err ->
            [ Expect.fail err
                |> asTest "should retrieve Royal Pizza example"
            ]

        Ok royalPizza ->
            [ describe "GET endpoints"
                [ testEndpoint db "GET" Encode.null "/food/ingredients"
                    |> Expect.equal (Just Route.FoodGetIngredientList)
                    |> asTest "should map the /food/ingredients endpoint"
                , testEndpoint db "GET" Encode.null "/food/transforms"
                    |> Expect.equal (Just Route.FoodGetTransformList)
                    |> asTest "should map the /food/transforms endpoint"
                , testEndpoint db "GET" Encode.null "/food/packagings"
                    |> Expect.equal (Just Route.FoodGetPackagingList)
                    |> asTest "should map the /food/packagings endpoint"
                , [ "/food?"
                  , "ingredients[]=a2e25aca-1f42-4bc8-bc0e-4d7c751775aa;97"
                  , "ingredients[]=6523c72b-2c28-474d-9bf8-45e018bbd0be;89"
                  , "ingredients[]=faa513ae-9c32-4e6c-874e-58c13309339e;70"
                  , "ingredients[]=755225f1-f0c5-497f-b8a9-263828a84a22;16"
                  , "ingredients[]=8f075c25-9ebf-430c-b41d-51d165c6e0d8;5"
                  , "ingredients[]=5d22559d-c16d-4545-94b9-be2ff51e2879;31"
                  , "ingredients[]=96b301d9-d21b-4cea-8903-bd7917e95a30;16"
                  , "ingredients[]=47b15759-416e-4c86-9127-4c75a55b7b8f;1"
                  , "ingredients[]=36b3ffec-51e7-4e26-b1b5-7d52554e0aa6;22"
                  , "transform=7541cf94-1d4d-4d1c-99e3-a9d5be0e7569;363"
                  , "packaging[]=c352add7-8037-464e-bff2-7da517419f88;100"
                  , "distribution=frozen"
                  , "preparation[]=freezing"
                  , "preparation[]=oven"
                  , "category=cakes"
                  ]
                    |> String.join "&"
                    |> testEndpoint db "GET" Encode.null
                    |> Expect.equal (Just <| Route.FoodGetRecipe (Ok royalPizza))
                    |> asTest "should map the /food endpoint"
                ]
            , describe "POST endpoints"
                [ "/food"
                    |> testEndpoint db "POST" (FoodQuery.encode royalPizza)
                    |> Expect.equal (Just (Route.FoodPostRecipe (Ok royalPizza)))
                    |> asTest "should map the POST /food endpoint"
                , "/food"
                    |> testEndpoint db "POST" Encode.null
                    |> expectFoodSingleErrorContains "ingredients"
                    |> asTest "should fail on invalid query passed"
                ]
            , describe "validation"
                [ testEndpoint db "GET" Encode.null "/food?ingredients[]=9cbc31e9-80a4-4b87-ac4b-ddc051c47f69|0"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "ingredients")
                    |> Expect.equal (Just "Format d'ingrédient invalide : 9cbc31e9-80a4-4b87-ac4b-ddc051c47f69|0.")
                    |> asTest "should validate ingredient format"
                , testEndpoint db "GET" Encode.null "/food?ingredients[]=invalid;100"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "ingredients")
                    |> Expect.equal (Just "Identifiant d’ingrédient invalide\u{202F}: invalid. Un `uuid` est attendu.")
                    |> asTest "should validate that an ingredient id is valid"
                , testEndpoint db "GET" Encode.null "/food?ingredients[]=9cbc31e9-80a4-4b87-ac4b-ddc051c47f69;-1"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "ingredients")
                    |> Expect.equal (Just "La masse doit être supérieure ou égale à zéro.")
                    |> asTest "should validate that an ingredient mass is greater than zero"
                , testEndpoint db "GET" Encode.null "/food?ingredients[]=9cbc31e9-80a4-4b87-ac4b-ddc051c47f69;1;invalidCountry"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "ingredients")
                    |> Expect.equal (Just "Code pays invalide: invalidCountry.")
                    |> asTest "should validate that an ingredient country is valid"
                , testEndpoint db "GET" Encode.null "/food?ingredients[]=9cbc31e9-80a4-4b87-ac4b-ddc051c47f69;1;FR;byPlane"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "ingredients")
                    |> Expect.equal (Just "Impossible de spécifier un acheminement par avion pour cet ingrédient, son origine par défaut ne le permet pas.")
                    |> asTest "should validate that an ingredient can be transported by plane"
                , testEndpoint db "GET" Encode.null "/food?ingredients[]=9cbc31e9-80a4-4b87-ac4b-ddc051c47f69;1;BD"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "ingredients")
                    |> Expect.equal (Just "Le code pays BD n'est pas utilisable dans un contexte Alimentaire.")
                    |> asTest "should validate that an ingredient country scope is valid"
                , testEndpoint db "GET" Encode.null "/food?transform=7541cf94-1d4d-4d1c-99e3-a9d5be0e7569;-1"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "transform")
                    |> Expect.equal (Just "La masse doit être supérieure ou égale à zéro.")
                    |> asTest "should validate that a transform mass is greater than zero"
                , testEndpoint db "GET" Encode.null "/food?transform=invalid;100"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "transform")
                    |> Expect.equal (Just "Identifiant invalide: invalid")
                    |> asTest "should validate that a transform code is valid"
                , testEndpoint db "GET" Encode.null "/food?packaging[]=c352add7-8037-464e-bff2-7da517419f88;-1"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "packaging")
                    |> Expect.equal (Just "La masse doit être supérieure ou égale à zéro.")
                    |> asTest "should validate that a packaging mass is greater than zero"
                , testEndpoint db "GET" Encode.null "/food?packaging[]=invalid;100"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "packaging")
                    |> Expect.equal (Just "Identifiant invalide: invalid")
                    |> asTest "should validate that a packaging code is valid"
                , testEndpoint db "GET" Encode.null "/food?distribution=invalid"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "distribution")
                    |> Expect.equal (Just "Choix invalide pour la distribution : invalid")
                    |> asTest "should validate that a distribution is valid"
                , testEndpoint db "GET" Encode.null "/food?preparation[]=invalid"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "preparation")
                    |> Expect.equal (Just "Préparation inconnue: invalid")
                    |> asTest "should validate that a preparation list entry is valid"
                , testEndpoint db "GET" Encode.null "/food?preparation[]=freezing&preparation[]=frying&preparation[]=oven"
                    |> Maybe.andThen extractFoodErrors
                    |> Maybe.andThen (Dict.get "preparation")
                    |> Expect.equal (Just "Deux techniques de préparation maximum.")
                    |> asTest "should validate preparation list length"
                ]
            ]


textileEndpoints : StaticDb.Db -> List Test
textileEndpoints db =
    [ describe "GET endpoints"
        [ String.join "&"
            [ "/textile/simulator?mass=0.17"
            , "product=tshirt"
            , "materials[]=ei-coton;1"
            , "countryFabric=FR"
            , "countryDyeing=FR"
            , "countryMaking=FR"
            ]
            |> testEndpoint db "GET" Encode.null
            |> Expect.equal (Just <| Route.TextileGetSimulator (Ok sampleQuery))
            |> asTest "should map the /textile/simulator endpoint"
        , [ "/textile/simulator?mass=0.17"
          , "product=tshirt"
          , "materials[]=ei-coton;1"
          , "countryFabric=FR"
          , "countryDyeing=FR"
          , "countryMaking=FR"
          , "disabledSteps=making,ennobling"
          ]
            |> String.join "&"
            |> testEndpoint db "GET" Encode.null
            |> Expect.equal
                (Just <|
                    Route.TextileGetSimulator <|
                        Ok { sampleQuery | disabledSteps = [ Label.Making, Label.Ennobling ] }
                )
            |> asTest "should map the /textile/simulator endpoint with the disabledSteps parameter set"
        , [ "/textile/simulator/fwe?mass=0.17"
          , "product=tshirt"
          , "materials[]=ei-coton;1"
          , "countryFabric=FR"
          , "countryDyeing=FR"
          , "countryMaking=FR"
          ]
            |> String.join "&"
            |> testEndpoint db "GET" Encode.null
            |> Expect.equal
                (Just <|
                    Route.TextileGetSimulatorSingle Definition.Fwe <|
                        Ok sampleQuery
                )
            |> asTest "should map the /textile/simulator/{impact} endpoint"
        , [ "/textile/simulator/detailed?mass=0.17"
          , "product=tshirt"
          , "materials[]=ei-coton;1"
          , "countryFabric=FR"
          , "countryDyeing=FR"
          , "countryMaking=FR"
          ]
            |> String.join "&"
            |> testEndpoint db "GET" Encode.null
            |> Expect.equal
                (Just <|
                    Route.TextileGetSimulatorDetailed <|
                        Ok sampleQuery
                )
            |> asTest "should map the /textile/simulator/detailed endpoint"
        ]
    , describe "POST endpoints"
        [ "/textile/simulator"
            |> testEndpoint db "POST" (Query.encode tShirtCotonFrance)
            |> Expect.equal (Just (Route.TextilePostSimulator (Ok tShirtCotonFrance)))
            |> asTest "should map the POST /textile/simulator endpoint with the body parsed as a valid query"
        , "/textile/simulator"
            |> testEndpoint db "POST" Encode.null
            |> Expect.equal (Just (Route.TextilePostSimulator (Err "Problem with the given value:\n\nnull\n\nExpecting an OBJECT with a field named `product`")))
            |> asTest "should map the POST /textile/simulator endpoint with an error when json body is invalid"
        , "/textile/simulator"
            |> testEndpoint db
                "POST"
                (Query.encode
                    { tShirtCotonFrance
                        | physicalDurability =
                            Just <| Unit.physicalDurability 9900000
                    }
                )
            |> expectTextileSingleErrorContains "physicalDurability"
            |> asTest "should reject invalid physicalDurability"
        , "/textile/simulator"
            |> testEndpoint db
                "POST"
                (Query.encode
                    { tShirtCotonFrance
                        | countrySpinning = Just (Country.Code "invalid")
                    }
                )
            |> Expect.equal (Just (Route.TextilePostSimulator (Err "Code pays invalide: invalid.")))
            |> asTest "should reject invalid spinning country"
        , "/textile/simulator"
            |> testEndpoint db
                "POST"
                (Query.encode
                    { tShirtCotonFrance
                        | materials =
                            [ { country = Just (Country.Code "invalid")
                              , id = Material.Id "ei-coton"
                              , share = Split.full
                              , spinning = Nothing
                              }
                            ]
                    }
                )
            |> Expect.equal (Just (Route.TextilePostSimulator (Err "Code pays invalide: invalid.")))
            |> asTest "should reject invalid materials country"
        ]
    , describe "materials param checks"
        [ [ "/textile/simulator?mass=0.17"
          , "product=tshirt"
          , "materials[]=ei-coton;0.3;;FR"
          , "materials[]=coton-rdp;0.3;UnconventionalSpinning"
          , "materials[]=ei-pet;0.4"
          , "countryFabric=FR"
          , "countryDyeing=FR"
          , "countryMaking=FR"
          ]
            |> String.join "&"
            |> testEndpoint db "GET" Encode.null
            |> Maybe.andThen extractQuery
            |> Maybe.map .materials
            |> Expect.equal
                (Just
                    [ { id = Material.Id "ei-coton"
                      , share = Split.thirty
                      , spinning = Nothing
                      , country = Just (Country.Code "FR")
                      }
                    , { id = Material.Id "coton-rdp"
                      , share = Split.thirty
                      , spinning = Just Spinning.Unconventional
                      , country = Nothing
                      }
                    , { id = Material.Id "ei-pet"
                      , share = Split.fourty
                      , spinning = Nothing
                      , country = Nothing
                      }
                    ]
                )
            |> asTest "should map the /textile/simulator endpoint with the list of materials"
        , testEndpoint db "GET" Encode.null "/textile/simulator?materials[]="
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "materials")
            |> Expect.equal (Just "Format de matière vide.")
            |> asTest "should validate empty material format"
        , testEndpoint db "GET" Encode.null "/textile/simulator?materials[]=notAnID"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "materials")
            |> Expect.equal (Just "Format de matière invalide : notAnID.")
            |> asTest "should validate invalid material format"
        , testEndpoint db "GET" Encode.null "/textile/simulator?materials[]=ei-coton"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "materials")
            |> Expect.equal (Just "Format de matière invalide : ei-coton.")
            |> asTest "should validate invalid material format even when valid material id"
        , testEndpoint db "GET" Encode.null "/textile/simulator?materials[]=ei-coton;12"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "materials")
            |> Expect.equal (Just "Une part (en nombre flottant) doit être comprise entre 0 et 1 inclus (ici: 12)")
            |> asTest "should validate invalid material ratios"
        , testEndpoint db "GET" Encode.null "/textile/simulator?materials[]=ei-coton;1;PasUnProcedeDeFilature"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "materials")
            |> Expect.equal
                (Just <|
                    "Un procédé de filature/filage doit être choisi parmi ("
                        ++ (Spinning.getAvailableProcesses Origin.NaturalFromVegetal
                                |> List.map Spinning.toString
                                |> String.join "|"
                           )
                        ++ ") (ici: PasUnProcedeDeFilature)"
                )
            |> asTest "should validate invalid material spinning for natural/artificial threads"
        , testEndpoint db "GET" Encode.null "/textile/simulator?materials[]=ei-coton;1;SyntheticSpinning"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "materials")
            |> Expect.equal
                (Just <|
                    "Un procédé de filature/filage doit être choisi parmi ("
                        ++ (Spinning.getAvailableProcesses Origin.NaturalFromVegetal
                                |> List.map Spinning.toString
                                |> String.join "|"
                           )
                        ++ ") (ici: SyntheticSpinning)"
                )
            |> asTest "should validate invalid material spinning for synthetic threads"
        , testEndpoint db "GET" Encode.null "/textile/simulator?materials[]=ei-coton;1;UnconventionalSpinning;NotACountryCode"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "materials")
            |> Expect.equal (Just "Code pays invalide: NotACountryCode.")
            |> asTest "should validate invalid material country code"
        , testEndpoint db "GET" Encode.null "/textile/simulator?printing=plop"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "printing")
            |> Expect.equal (Just "Format de type et surface d'impression invalide: plop")
            |> asTest "should validate invalid printing method identifier"
        , testEndpoint db "GET" Encode.null "/textile/simulator?printing=substantive;1.2"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "printing")
            |> Expect.equal (Just "Le ratio de surface d'impression doit être supérieur à zéro et inférieur à 1.")
            |> asTest "should validate invalid printing ratio"
        , testEndpoint db "GET" Encode.null "/textile/simulator?countryDyeing=US"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "countryDyeing")
            |> Expect.equal (Just "Le code pays US n'est pas utilisable dans un contexte Textile.")
            |> asTest "should validate that an ingredient country scope is valid"
        , testEndpoint db "GET" Encode.null "/textile/simulator?physicalDurability=99"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "physicalDurability")
            |> Expect.equal (Just "La durabilité doit être comprise entre 0.67 et 1.45.")
            |> asTest "should validate that the physical durability param is invalid"
        , testEndpoint db "GET" Encode.null "/textile/simulator?trims[]=invalid"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "trims")
            |> Expect.equal (Just "Format d'accessoire invalide : invalid.")
            |> asTest "should validate trims parameter format"
        , testEndpoint db "GET" Encode.null "/textile/simulator?trims[]=invalid;1"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "trims")
            |> Expect.equal (Just "Identifiant de composant invalide : invalid")
            |> asTest "should validate trims parameter identifier format"
        , testEndpoint db "GET" Encode.null "/textile/simulator?trims[]=0e8ea799-9b06-490c-a925-37564746c454;-1"
            |> Maybe.andThen extractTextileErrors
            |> Maybe.andThen (Dict.get "trims")
            |> Expect.equal (Just "La quantité doit être un nombre entier positif")
            |> asTest "should validate that a trim item quantity is a positive integer"
        ]
    , describe "multiple parameters checks"
        [ testEndpoint db "GET" Encode.null "/textile/simulator"
            |> Maybe.andThen extractTextileErrors
            |> Expect.equal
                (Dict.fromList
                    [ ( "mass", "La masse est manquante." )
                    , ( "product", "Identifiant du type de produit manquant." )
                    ]
                    |> Just
                )
            |> asTest "should expose query validation errors"
        , [ "/textile/simulator?mass=-0.17"
          , "product=notAProductID"
          , "fabricProcess=notAFabricProcess"
          , "material=notAnID"
          , "materials[]=notAnID"
          , "surfaceMass=-2"
          , "countryFabric=notACountryCode"
          , "countryDyeing=notACountryCode"
          , "countryMaking=US"
          , "disabledSteps=invalid"
          , "fading=untrue"
          , "dyeingProcessType=yolo"
          , "printing=yolo"
          , "yarnSize=0"
          , "trims[]=invalid"
          ]
            |> String.join "&"
            |> testEndpoint db "GET" Encode.null
            |> Maybe.andThen extractTextileErrors
            |> Expect.equal
                (Dict.fromList
                    [ ( "countryFabric", "Code pays invalide: notACountryCode." )
                    , ( "countryDyeing", "Code pays invalide: notACountryCode." )
                    , ( "countryMaking", "Le code pays US n'est pas utilisable dans un contexte Textile." )
                    , ( "mass", "La masse doit être supérieure ou égale à zéro." )
                    , ( "materials", "Format de matière invalide : notAnID." )
                    , ( "fabricProcess", "Procédé de tissage/tricotage inconnu: notAFabricProcess" )
                    , ( "surfaceMass", "Le grammage (surfaceMass) doit être compris entre 80 et 500 g/m²." )
                    , ( "product", "Produit non trouvé id=notAProductID." )
                    , ( "disabledSteps", "Impossible d'interpréter la liste des étapes désactivées; Code étape inconnu: invalid" )
                    , ( "fading", "La valeur ne peut être que true ou false." )
                    , ( "dyeingProcessType", "Type de teinture inconnu: yolo" )
                    , ( "printing", "Format de type et surface d'impression invalide: yolo" )
                    , ( "yarnSize", "Le titrage (yarnSize) doit être compris entre 9 et 200 Nm (entre 50 et 1111 Dtex)" )
                    , ( "trims", "Format d'accessoire invalide : invalid." )
                    ]
                    |> Just
                )
            |> asTest "should expose detailed query validation errors"
        ]
    ]


testEndpoint : StaticDb.Db -> String -> Encode.Value -> String -> Maybe Route.Route
testEndpoint dbs method body =
    createServerRequest dbs method body >> Route.endpoint dbs


extractQuery : Route.Route -> Maybe Query
extractQuery route =
    case route of
        Route.TextileGetSimulator (Ok query) ->
            Just query

        _ ->
            Nothing


extractFoodErrors : Route.Route -> Maybe (Dict String String)
extractFoodErrors route =
    case route of
        Route.FoodGetRecipe (Err errors) ->
            Just errors

        _ ->
            Nothing


extractTextileErrors : Route.Route -> Maybe (Dict String String)
extractTextileErrors route =
    case route of
        Route.TextileGetSimulator (Err errors) ->
            Just errors

        _ ->
            Nothing


expectFoodSingleErrorContains : String -> Maybe Route.Route -> Expect.Expectation
expectFoodSingleErrorContains str route =
    case route of
        Just (Route.FoodPostRecipe (Err err)) ->
            Expect.equal (String.contains str err) True

        _ ->
            Expect.fail "No matching error found"


expectTextileSingleErrorContains : String -> Maybe Route.Route -> Expect.Expectation
expectTextileSingleErrorContains str route =
    case route of
        Just (Route.TextilePostSimulator (Err err)) ->
            Expect.equal (String.contains str err) True

        _ ->
            Expect.fail "No matching error found"

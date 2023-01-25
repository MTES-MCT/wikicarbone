module Data.Dataset exposing
    ( Dataset(..)
    , datasets
    , isDetailed
    , label
    , parseSlug
    , reset
    , same
    , slugWithId
    , toRoutePath
    )

import Data.Country as Country
import Data.Food.Ingredient as Ingredient
import Data.Impact as Impact
import Data.Scope as Scope exposing (Scope)
import Data.Textile.Material as Material
import Data.Textile.Product as Product
import Url.Parser as Parser exposing (Parser)


{-| A Dataset represents a target dataset and an optional id in this dataset.

It's used by Page.Explore and related routes.

-}
type Dataset
    = Countries (Maybe Country.Code)
    | Impacts (Maybe Impact.Trigram)
    | FoodIngredients (Maybe Ingredient.Id)
    | TextileProducts (Maybe Product.Id)
    | TextileMaterials (Maybe Material.Id)


datasets : Scope -> List Dataset
datasets scope =
    Impacts Nothing
        :: Countries Nothing
        :: (case scope of
                Scope.Food ->
                    [ FoodIngredients Nothing ]

                Scope.Textile ->
                    [ TextileProducts Nothing
                    , TextileMaterials Nothing
                    ]
           )


fromSlug : String -> Dataset
fromSlug string =
    case string of
        "countries" ->
            Countries Nothing

        "ingredients" ->
            FoodIngredients Nothing

        "products" ->
            TextileProducts Nothing

        "materials" ->
            TextileMaterials Nothing

        _ ->
            Impacts Nothing


isDetailed : Dataset -> Bool
isDetailed dataset =
    case dataset of
        Countries (Just _) ->
            True

        Impacts (Just _) ->
            True

        FoodIngredients (Just _) ->
            True

        TextileProducts (Just _) ->
            True

        TextileMaterials (Just _) ->
            True

        _ ->
            False


label : Dataset -> String
label =
    strings >> .label


parseSlug : Parser (Dataset -> a) a
parseSlug =
    Parser.custom "DATASET" <|
        \string ->
            Just (fromSlug string)


reset : Dataset -> Dataset
reset dataset =
    case dataset of
        Countries _ ->
            Countries Nothing

        Impacts _ ->
            Impacts Nothing

        FoodIngredients _ ->
            FoodIngredients Nothing

        TextileProducts _ ->
            TextileProducts Nothing

        TextileMaterials _ ->
            TextileMaterials Nothing


same : Dataset -> Dataset -> Bool
same a b =
    case ( a, b ) of
        ( Countries _, Countries _ ) ->
            True

        ( Impacts _, Impacts _ ) ->
            True

        ( FoodIngredients _, FoodIngredients _ ) ->
            True

        ( TextileProducts _, TextileProducts _ ) ->
            True

        ( TextileMaterials _, TextileMaterials _ ) ->
            True

        _ ->
            False


slug : Dataset -> String
slug =
    strings >> .slug


slugWithId : Dataset -> String -> Dataset
slugWithId dataset idString =
    case dataset of
        Countries _ ->
            Countries (Just (Country.codeFromString idString))

        Impacts _ ->
            Impacts (Just (Impact.trg idString))

        FoodIngredients _ ->
            FoodIngredients (Just (Ingredient.idFromString idString))

        TextileProducts _ ->
            TextileProducts (Just (Product.Id idString))

        TextileMaterials _ ->
            TextileMaterials (Just (Material.Id idString))


strings : Dataset -> { slug : String, label : String }
strings dataset =
    case dataset of
        Countries _ ->
            { slug = "countries", label = "Pays" }

        Impacts _ ->
            { slug = "impacts", label = "Impacts" }

        FoodIngredients _ ->
            { slug = "ingredients", label = "Ingrédients" }

        TextileProducts _ ->
            { slug = "products", label = "Produits" }

        TextileMaterials _ ->
            { slug = "materials", label = "Matières" }


toRoutePath : Dataset -> List String
toRoutePath dataset =
    case dataset of
        Countries Nothing ->
            [ slug dataset ]

        Countries (Just code) ->
            [ slug dataset, Country.codeToString code ]

        Impacts Nothing ->
            []

        FoodIngredients Nothing ->
            [ slug dataset ]

        FoodIngredients (Just id) ->
            [ slug dataset, Ingredient.idToString id ]

        Impacts (Just trigram) ->
            [ slug dataset, Impact.toString trigram ]

        TextileProducts Nothing ->
            [ slug dataset ]

        TextileProducts (Just id) ->
            [ slug dataset, Product.idToString id ]

        TextileMaterials Nothing ->
            [ slug dataset ]

        TextileMaterials (Just id) ->
            [ slug dataset, Material.idToString id ]

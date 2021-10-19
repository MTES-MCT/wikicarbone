module Data.Inputs exposing (..)

import Array
import Base64
import Data.Country as Country exposing (Country)
import Data.Db exposing (Db)
import Data.Material as Material exposing (Material)
import Data.Process as Process
import Data.Product as Product exposing (Product)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), frenchLocale)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Mass exposing (Mass)


type alias Inputs =
    { mass : Mass
    , material : Material
    , product : Product
    , countries : List Country
    , dyeingWeighting : Maybe Float
    , airTransportRatio : Maybe Float
    }


type alias Query =
    -- a shorter version than Inputs (identifiers only)
    { mass : Mass
    , material : Process.Uuid
    , product : Product.Id
    , countries : List Country
    , dyeingWeighting : Maybe Float
    , airTransportRatio : Maybe Float
    }


fromQuery : Db -> Query -> Result String Inputs
fromQuery db query =
    -- FIXME: do we really need Inputs and Query now we have a Db? Can we only rely on Query for simplicity?
    let
        ( material, product ) =
            ( db.materials |> Material.findByProcessUuid2 query.material
            , db.products |> Product.findById2 query.product
            )

        build material_ product_ =
            { mass = query.mass
            , material = material_
            , product = product_
            , countries = query.countries
            , dyeingWeighting = query.dyeingWeighting
            , airTransportRatio = query.airTransportRatio
            }
    in
    Result.map2 build material product


toQuery : Inputs -> Query
toQuery { mass, material, product, countries, airTransportRatio, dyeingWeighting } =
    { mass = mass
    , material = material.materialProcessUuid
    , product = product.id
    , countries = countries
    , dyeingWeighting = dyeingWeighting
    , airTransportRatio = airTransportRatio
    }


toLabel : Inputs -> String
toLabel { mass, material, product } =
    String.join " "
        [ product.name
        , "en"
        , material.name
        , "de"
        , FormatNumber.format { frenchLocale | decimals = Exact 2 } (Mass.inKilograms mass) ++ "\u{202F}kg"
        ]


updateStepCountry : Int -> Country -> Inputs -> Inputs
updateStepCountry index country inputs =
    { inputs
        | countries = inputs.countries |> Array.fromList |> Array.set index country |> Array.toList
        , dyeingWeighting =
            -- FIXME: index 2 is Ennoblement step; how could we use th step label instead?
            if index == 2 && Array.get index (Array.fromList inputs.countries) /= Just country then
                -- reset custom value as we just switched country, which dyeing weighting is totally different
                Nothing

            else
                inputs.dyeingWeighting
        , airTransportRatio =
            -- FIXME: index 3 is Making step; how could we use th step label instead?
            if index == 3 && Array.get index (Array.fromList inputs.countries) /= Just country then
                -- reset custom value as we just switched country
                Nothing

            else
                inputs.airTransportRatio
    }


default : Inputs
default =
    tShirtCotonIndia


defaultQuery : Query
defaultQuery =
    toQuery default


tShirtCotonFrance : Inputs
tShirtCotonFrance =
    -- T-shirt circuit France
    { mass = Product.tShirt.mass
    , material = Material.cotton
    , product = Product.tShirt
    , countries =
        [ Country.China
        , Country.France
        , Country.France
        , Country.France
        , Country.France
        ]
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    }


tShirtCotonEurope : Inputs
tShirtCotonEurope =
    -- T-shirt circuit Europe
    { tShirtCotonFrance
        | countries =
            [ Country.China
            , Country.Turkey
            , Country.Tunisia
            , Country.Spain
            , Country.France
            ]
    }


tShirtCotonIndia : Inputs
tShirtCotonIndia =
    -- T-shirt circuit France
    { mass = Product.tShirt.mass
    , material = Material.cotton
    , product = Product.tShirt
    , countries =
        [ Country.China
        , Country.India
        , Country.India
        , Country.India
        , Country.France
        ]
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    }


tShirtCotonAsie : Inputs
tShirtCotonAsie =
    -- T-shirt circuit Europe
    { tShirtCotonFrance
        | countries =
            [ Country.China
            , Country.China
            , Country.China
            , Country.China
            , Country.France
            ]
    }


jupeCircuitAsie : Inputs
jupeCircuitAsie =
    -- Jupe circuit Asie
    { mass = Product.findByName "Jupe" |> .mass
    , material = Material.findByName "Filament d'acrylique"
    , product = Product.findByName "Jupe"
    , countries =
        [ Country.China
        , Country.China
        , Country.China
        , Country.China
        , Country.France
        ]
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    }


manteauCircuitEurope : Inputs
manteauCircuitEurope =
    -- Manteau circuit Europe
    { mass = Product.findByName "Manteau" |> .mass
    , material = Material.findByName "Fil de cachemire"
    , product = Product.findByName "Manteau"
    , countries =
        [ Country.China
        , Country.Turkey
        , Country.Tunisia
        , Country.Spain
        , Country.France
        ]
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    }


pantalonCircuitEurope : Inputs
pantalonCircuitEurope =
    { mass = Product.findByName "Pantalon" |> .mass
    , material = Material.findByName "Fil de lin (filasse)"
    , product = Product.findByName "Pantalon"
    , countries =
        [ Country.China
        , Country.Turkey
        , Country.Turkey
        , Country.Turkey
        , Country.France
        ]
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    }


robeCircuitBangladesh : Inputs
robeCircuitBangladesh =
    -- Jupe circuit Asie
    { mass = Mass.kilograms 0.5
    , material = Material.findByName "Filament d'aramide"
    , product = Product.findByName "Robe"
    , countries =
        [ Country.China
        , Country.Bangladesh
        , Country.Portugal
        , Country.Tunisia
        , Country.France
        ]
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    }


presets : List Inputs
presets =
    [ tShirtCotonFrance
    , tShirtCotonEurope
    , tShirtCotonAsie
    , jupeCircuitAsie
    , manteauCircuitEurope
    , pantalonCircuitEurope
    ]


decode : Decoder Inputs
decode =
    Decode.map6 Inputs
        (Decode.field "mass" (Decode.map Mass.kilograms Decode.float))
        (Decode.field "material" Material.decode)
        (Decode.field "product" Product.decode)
        (Decode.field "countries" (Decode.list Country.decode))
        (Decode.field "dyeingWeighting" (Decode.maybe Decode.float))
        (Decode.field "airTransportRatio" (Decode.maybe Decode.float))


encode : Inputs -> Encode.Value
encode inputs =
    Encode.object
        [ ( "mass", Encode.float (Mass.inKilograms inputs.mass) )
        , ( "material", Material.encode inputs.material )
        , ( "product", Product.encode inputs.product )
        , ( "countries", Encode.list Country.encode inputs.countries )
        , ( "dyeingWeighting", inputs.dyeingWeighting |> Maybe.map Encode.float |> Maybe.withDefault Encode.null )
        , ( "airTransportRatio", inputs.airTransportRatio |> Maybe.map Encode.float |> Maybe.withDefault Encode.null )
        ]


decodeQuery : Decoder Query
decodeQuery =
    Decode.map6 Query
        (Decode.field "mass" (Decode.map Mass.kilograms Decode.float))
        (Decode.field "material" (Decode.map Process.Uuid Decode.string))
        (Decode.field "product" (Decode.map Product.Id Decode.string))
        (Decode.field "countries" (Decode.list Country.decode))
        (Decode.field "dyeingWeighting" (Decode.maybe Decode.float))
        (Decode.field "airTransportRatio" (Decode.maybe Decode.float))


encodeQuery : Query -> Encode.Value
encodeQuery query =
    Encode.object
        [ ( "mass", Encode.float (Mass.inKilograms query.mass) )
        , ( "material", query.material |> Process.uuidToString |> Encode.string )
        , ( "product", query.product |> Product.idToString |> Encode.string )
        , ( "countries", Encode.list Country.encode query.countries )
        , ( "dyeingWeighting", query.dyeingWeighting |> Maybe.map Encode.float |> Maybe.withDefault Encode.null )
        , ( "airTransportRatio", query.airTransportRatio |> Maybe.map Encode.float |> Maybe.withDefault Encode.null )
        ]


b64decode : String -> Result String Query
b64decode =
    Base64.decode
        >> Result.andThen
            (Decode.decodeString decodeQuery
                >> Result.mapError Decode.errorToString
            )


b64encode : Query -> String
b64encode =
    encodeQuery >> Encode.encode 0 >> Base64.encode

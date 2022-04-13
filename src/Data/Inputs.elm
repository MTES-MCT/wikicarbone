module Data.Inputs exposing
    ( Inputs
    , MaterialInput
    , MaterialQuery
    , Query
    , addMaterial
    , b64decode
    , b64encode
    , countryList
    , decodeQuery
    , defaultQuery
    , encode
    , encodeQuery
    , fromQuery
    , jupeCircuitAsie
    , parseBase64Query
    , presets
    , removeMaterial
    , tShirtCotonAsie
    , tShirtCotonFrance
    , toQuery
    , toString
    , updateMaterial
    , updateMaterialRecycledRatio
    , updateMaterialShare
    , updateProduct
    , updateStepCountry
    )

import Base64
import Data.Country as Country exposing (Country)
import Data.Db exposing (Db)
import Data.Material as Material exposing (Material)
import Data.Product as Product exposing (Product)
import Data.Unit as Unit
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Json.Encode as Encode
import List.Extra as LE
import Mass exposing (Mass)
import Result.Extra as RE
import Url.Parser as Parser exposing (Parser)
import Views.Format as Format


type alias MaterialInput =
    { material : Material
    , share : Unit.Ratio
    , recycledRatio : Unit.Ratio
    }


type alias Inputs =
    { mass : Mass
    , materials : List MaterialInput
    , product : Product
    , countryMaterial : Country
    , countryFabric : Country
    , countryDyeing : Country
    , countryMaking : Country
    , countryDistribution : Country
    , countryUse : Country
    , countryEndOfLife : Country
    , dyeingWeighting : Maybe Unit.Ratio
    , airTransportRatio : Maybe Unit.Ratio
    , quality : Maybe Unit.Quality
    , reparability : Maybe Unit.Reparability
    }


type alias MaterialQuery =
    { id : Material.Id
    , share : Unit.Ratio
    , recycledRatio : Unit.Ratio
    }


type alias Query =
    -- a shorter version than of (identifiers only)
    { mass : Mass
    , materials : List MaterialQuery
    , product : Product.Id
    , countryFabric : Country.Code
    , countryDyeing : Country.Code
    , countryMaking : Country.Code
    , dyeingWeighting : Maybe Unit.Ratio
    , airTransportRatio : Maybe Unit.Ratio
    , quality : Maybe Unit.Quality
    , reparability : Maybe Unit.Reparability
    }


toMaterialInputs : List Material -> List MaterialQuery -> Result String (List MaterialInput)
toMaterialInputs materials =
    List.map
        (\{ id, share, recycledRatio } ->
            Material.findById id materials
                |> Result.map
                    (\material_ ->
                        { material = material_
                        , share = share
                        , recycledRatio = recycledRatio
                        }
                    )
        )
        >> RE.combine


toMaterialQuery : List MaterialInput -> List MaterialQuery
toMaterialQuery =
    List.map
        (\{ material, share, recycledRatio } ->
            { id = material.id
            , share = share
            , recycledRatio = recycledRatio
            }
        )


firstMaterialCountry : List Country -> List MaterialInput -> Result String Country
firstMaterialCountry countries =
    List.head
        >> Maybe.map
            (\{ material } -> Country.findByCode material.defaultCountry countries)
        >> Result.fromMaybe "La liste de matières est vide."
        >> RE.join


fromQuery : Db -> Query -> Result String Inputs
fromQuery db query =
    let
        materials =
            query.materials
                |> toMaterialInputs db.materials

        franceResult =
            Country.findByCode (Country.Code "FR") db.countries
    in
    Ok Inputs
        |> RE.andMap (Ok query.mass)
        |> RE.andMap materials
        |> RE.andMap (db.products |> Product.findById query.product)
        -- The material country is constrained to be the first material's default country
        |> RE.andMap (materials |> Result.andThen (firstMaterialCountry db.countries))
        |> RE.andMap (db.countries |> Country.findByCode query.countryFabric)
        |> RE.andMap (db.countries |> Country.findByCode query.countryDyeing)
        |> RE.andMap (db.countries |> Country.findByCode query.countryMaking)
        -- The distribution country is always France
        |> RE.andMap franceResult
        -- The use country is always France
        |> RE.andMap franceResult
        -- The end of life country is always France
        |> RE.andMap franceResult
        |> RE.andMap (Ok query.dyeingWeighting)
        |> RE.andMap (Ok query.airTransportRatio)
        |> RE.andMap (Ok query.quality)
        |> RE.andMap (Ok query.reparability)


toQuery : Inputs -> Query
toQuery inputs =
    { mass = inputs.mass
    , materials = toMaterialQuery inputs.materials
    , product = inputs.product.id
    , countryFabric = inputs.countryFabric.code
    , countryDyeing = inputs.countryDyeing.code
    , countryMaking = inputs.countryMaking.code
    , dyeingWeighting = inputs.dyeingWeighting
    , airTransportRatio = inputs.airTransportRatio
    , quality = inputs.quality
    , reparability = inputs.reparability
    }


toString : Inputs -> String
toString inputs =
    [ Just [ inputs.product.name ]
    , Just [ materialsToString inputs.materials ++ "de " ++ Format.kgToString inputs.mass ]
    , Just [ "matière et filature", inputs.countryMaterial.name ]
    , Just [ "tricotage", inputs.countryFabric.name ]
    , Just [ "teinture", inputs.countryDyeing.name ++ dyeingWeightingToString inputs.dyeingWeighting ]
    , Just [ "confection", inputs.countryMaking.name ++ airTransportRatioToString inputs.airTransportRatio ]
    , Just [ "distribution", inputs.countryDistribution.name ]
    , Just [ "utilisation", inputs.countryUse.name ++ intrinsicQualityToString inputs.quality ]
    , Just [ "fin de vie", inputs.countryEndOfLife.name ]
    , inputs.quality |> Maybe.map (Unit.qualityToFloat >> String.fromFloat >> (\q -> [ "qualité", q ]))
    , inputs.reparability |> Maybe.map (Unit.reparabilityToFloat >> String.fromFloat >> (\r -> [ "réparabilité", r ]))
    ]
        |> List.filterMap identity
        |> List.map (String.join "\u{00A0}: ")
        |> String.join ", "


materialsToString : List MaterialInput -> String
materialsToString materials =
    materials
        |> List.filter (\{ share } -> Unit.ratioToFloat share > 0)
        |> List.map
            (\{ material, share, recycledRatio } ->
                Format.formatFloat 0 (Unit.ratioToFloat share * 100)
                    ++ "% "
                    ++ Material.fullName (Just recycledRatio) material
                    ++ ", "
            )
        |> List.foldr (++) ""


dyeingWeightingToString : Maybe Unit.Ratio -> String
dyeingWeightingToString maybeRatio =
    case maybeRatio of
        Nothing ->
            " (avec un procédé représentatif)"

        Just ratio ->
            if Unit.ratioToFloat ratio == 0 then
                " (avec un procédé représentatif)"

            else
                ratio
                    |> Format.ratioToPercentString
                    |> (\percent ->
                            " (avec un procédé " ++ percent ++ " majorant)"
                       )


airTransportRatioToString : Maybe Unit.Ratio -> String
airTransportRatioToString maybeRatio =
    case maybeRatio of
        Nothing ->
            ""

        Just ratio ->
            if Unit.ratioToFloat ratio == 0 then
                ""

            else
                ratio
                    |> Format.ratioToPercentString
                    |> (\percent ->
                            " (avec " ++ percent ++ " de transport aérien)"
                       )


intrinsicQualityToString : Maybe Unit.Quality -> String
intrinsicQualityToString maybeQuality =
    case maybeQuality of
        Nothing ->
            ""

        Just quality ->
            if Unit.qualityToFloat quality == 1 then
                ""

            else
                quality
                    |> Unit.qualityToFloat
                    |> String.fromFloat
                    |> (\q ->
                            " (qualité intrinsèque : " ++ q ++ ")"
                       )


countryList : Inputs -> List Country
countryList inputs =
    [ inputs.countryMaterial
    , inputs.countryFabric
    , inputs.countryDyeing
    , inputs.countryMaking
    , inputs.countryDistribution
    , inputs.countryUse
    , inputs.countryEndOfLife
    ]


updateStepCountry : Int -> Country.Code -> Query -> Query
updateStepCountry index code query =
    let
        updatedQuery =
            case index of
                1 ->
                    -- FIXME: index 1 is WeavingKnitting step; how could we use the step label instead?
                    { query | countryFabric = code }

                2 ->
                    -- FIXME: index 2 is Ennoblement step; how could we use the step label instead?
                    { query | countryDyeing = code }

                3 ->
                    -- FIXME: index 3 is Making step; how could we use the step label instead?
                    { query | countryMaking = code }

                _ ->
                    query
    in
    { updatedQuery
        | dyeingWeighting =
            -- FIXME: index 2 is Ennoblement step; how could we use th step label instead?
            if index == 2 && query.countryDyeing /= code then
                -- reset custom value as we just switched country, which dyeing weighting is totally different
                Nothing

            else
                query.dyeingWeighting
        , airTransportRatio =
            -- FIXME: index 3 is Making step; how could we use th step label instead?
            if index == 3 && query.countryMaking /= code then
                -- reset custom value as we just switched country
                Nothing

            else
                query.airTransportRatio
    }


addMaterial : Db -> Query -> Query
addMaterial db query =
    let
        ( length, polyester, elasthanne ) =
            ( List.length query.materials
            , Material.Id "pet"
            , Material.Id "pu"
            )

        notUsed id =
            query.materials
                |> List.map .id
                |> List.member id
                |> not

        newMaterialId =
            if length == 1 && notUsed polyester then
                Just polyester

            else if length == 2 && notUsed elasthanne then
                Just elasthanne

            else
                db.materials
                    |> List.filter (.id >> notUsed)
                    |> List.sortBy .priority
                    |> List.map .id
                    |> LE.last
    in
    case newMaterialId of
        Just id ->
            { query
                | materials =
                    query.materials
                        ++ [ { id = id
                             , share = Unit.ratio 0
                             , recycledRatio = Unit.ratio 0
                             }
                           ]
            }

        Nothing ->
            query


updateMaterialAt : Int -> (MaterialQuery -> MaterialQuery) -> Query -> Query
updateMaterialAt index update query =
    { query | materials = query.materials |> LE.updateAt index update }


updateMaterial : Int -> Material -> Query -> Query
updateMaterial index { id } =
    -- Note: The first material country is always extracted and applied in `fromQuery`.
    updateMaterialAt index
        (\({ share } as m) ->
            { m | id = id, share = share, recycledRatio = Unit.ratio 0 }
        )


updateMaterialRecycledRatio : Int -> Unit.Ratio -> Query -> Query
updateMaterialRecycledRatio index recycledRatio =
    updateMaterialAt index (\m -> { m | recycledRatio = recycledRatio })


updateMaterialShare : Int -> Unit.Ratio -> Query -> Query
updateMaterialShare index share =
    updateMaterialAt index (\m -> { m | share = share })


removeMaterial : Int -> Query -> Query
removeMaterial index query =
    { query | materials = query.materials |> LE.removeAt index }
        |> (\({ materials } as q) ->
                -- set share to 100% when a single material remains
                if List.length materials == 1 then
                    updateMaterialShare 0 (Unit.ratio 1) q

                else
                    q
           )


updateProduct : Product -> Query -> Query
updateProduct product query =
    { query
        | product = product.id
        , mass = product.mass
        , quality =
            -- ensure resetting quality when product is changed
            if product.id /= query.product then
                Nothing

            else
                query.quality
        , reparability =
            -- ensure resetting reparability when product is changed
            if product.id /= query.product then
                Nothing

            else
                query.reparability
    }


defaultQuery : Query
defaultQuery =
    tShirtCotonIndia


tShirtCotonFrance : Query
tShirtCotonFrance =
    -- T-shirt circuit France
    { mass = Mass.kilograms 0.17
    , materials =
        [ { id = Material.Id "coton"
          , share = Unit.ratio 1
          , recycledRatio = Unit.ratio 0
          }
        ]
    , product = Product.Id "tshirt"
    , countryFabric = Country.Code "FR"
    , countryDyeing = Country.Code "FR"
    , countryMaking = Country.Code "FR"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    }


tShirtCotonEurope : Query
tShirtCotonEurope =
    -- T-shirt circuit Europe
    { tShirtCotonFrance
        | countryFabric = Country.Code "TR"
        , countryDyeing = Country.Code "TN"
        , countryMaking = Country.Code "ES"
    }


tShirtCotonIndia : Query
tShirtCotonIndia =
    -- T-shirt circuit Inde
    { tShirtCotonFrance
        | countryFabric = Country.Code "IN"
        , countryDyeing = Country.Code "IN"
        , countryMaking = Country.Code "IN"
    }


tShirtCotonAsie : Query
tShirtCotonAsie =
    -- T-shirt circuit Asie
    { tShirtCotonFrance
        | countryFabric = Country.Code "CN"
        , countryDyeing = Country.Code "CN"
        , countryMaking = Country.Code "CN"
    }


jupeCircuitAsie : Query
jupeCircuitAsie =
    -- Jupe circuit Asie
    { mass = Mass.kilograms 0.3
    , materials =
        [ { id = Material.Id "acrylique"
          , share = Unit.ratio 1
          , recycledRatio = Unit.ratio 0
          }
        ]
    , product = Product.Id "jupe"
    , countryFabric = Country.Code "CN"
    , countryDyeing = Country.Code "CN"
    , countryMaking = Country.Code "CN"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    }


manteauCircuitEurope : Query
manteauCircuitEurope =
    -- Manteau circuit Europe
    { mass = Mass.kilograms 0.95
    , materials =
        [ { id = Material.Id "cachemire"
          , share = Unit.ratio 1
          , recycledRatio = Unit.ratio 0
          }
        ]
    , product = Product.Id "manteau"
    , countryFabric = Country.Code "TR"
    , countryDyeing = Country.Code "TN"
    , countryMaking = Country.Code "ES"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    }


pantalonCircuitEurope : Query
pantalonCircuitEurope =
    -- Pantalon circuit Europe
    { mass = Mass.kilograms 0.45
    , materials =
        [ { id = Material.Id "lin-filasse"
          , share = Unit.ratio 1
          , recycledRatio = Unit.ratio 0
          }
        ]
    , product = Product.Id "pantalon"
    , countryFabric = Country.Code "TR"
    , countryDyeing = Country.Code "TR"
    , countryMaking = Country.Code "TR"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    }


presets : List Query
presets =
    [ tShirtCotonFrance
    , tShirtCotonEurope
    , tShirtCotonAsie
    , jupeCircuitAsie
    , manteauCircuitEurope
    , pantalonCircuitEurope
    ]


encode : Inputs -> Encode.Value
encode inputs =
    Encode.object
        [ ( "mass", Encode.float (Mass.inKilograms inputs.mass) )
        , ( "materials", Encode.list encodeMaterialInput inputs.materials )
        , ( "product", Product.encode inputs.product )
        , ( "countryFabric", Country.encode inputs.countryFabric )
        , ( "countryDyeing", Country.encode inputs.countryDyeing )
        , ( "countryMaking", Country.encode inputs.countryMaking )
        , ( "dyeingWeighting", inputs.dyeingWeighting |> Maybe.map Unit.encodeRatio |> Maybe.withDefault Encode.null )
        , ( "airTransportRatio", inputs.airTransportRatio |> Maybe.map Unit.encodeRatio |> Maybe.withDefault Encode.null )
        , ( "quality", inputs.quality |> Maybe.map Unit.encodeQuality |> Maybe.withDefault Encode.null )
        , ( "reparability", inputs.reparability |> Maybe.map Unit.encodeReparability |> Maybe.withDefault Encode.null )
        ]


encodeMaterialInput : MaterialInput -> Encode.Value
encodeMaterialInput v =
    Encode.object
        [ ( "material", Material.encode v.material )
        , ( "share", Unit.encodeRatio v.share )
        , ( "recycledRatio", Unit.encodeRatio v.recycledRatio )
        ]


decodeQuery : Decoder Query
decodeQuery =
    Decode.succeed Query
        |> Pipe.required "mass" (Decode.map Mass.kilograms Decode.float)
        |> Pipe.required "materials" (Decode.list decodeMaterialQuery)
        |> Pipe.required "product" (Decode.map Product.Id Decode.string)
        |> Pipe.required "countryFabric" (Decode.map Country.Code Decode.string)
        |> Pipe.required "countryDyeing" (Decode.map Country.Code Decode.string)
        |> Pipe.required "countryMaking" (Decode.map Country.Code Decode.string)
        |> Pipe.optional "dyeingWeighting" (Decode.maybe Unit.decodeRatio) Nothing
        |> Pipe.optional "airTransportRatio" (Decode.maybe Unit.decodeRatio) Nothing
        |> Pipe.optional "quality" (Decode.maybe Unit.decodeQuality) Nothing
        |> Pipe.optional "reparability" (Decode.maybe Unit.decodeReparability) Nothing


decodeMaterialQuery : Decoder MaterialQuery
decodeMaterialQuery =
    Decode.succeed MaterialQuery
        |> Pipe.required "id" (Decode.map Material.Id Decode.string)
        |> Pipe.required "share" Unit.decodeRatio
        |> Pipe.required "recycledRatio" Unit.decodeRatio


encodeQuery : Query -> Encode.Value
encodeQuery query =
    Encode.object
        [ ( "mass", Encode.float (Mass.inKilograms query.mass) )
        , ( "materials", Encode.list encodeMaterialQuery query.materials )
        , ( "product", query.product |> Product.idToString |> Encode.string )
        , ( "countryFabric", query.countryFabric |> Country.codeToString |> Encode.string )
        , ( "countryDyeing", query.countryDyeing |> Country.codeToString |> Encode.string )
        , ( "countryMaking", query.countryMaking |> Country.codeToString |> Encode.string )
        , ( "dyeingWeighting", query.dyeingWeighting |> Maybe.map Unit.encodeRatio |> Maybe.withDefault Encode.null )
        , ( "airTransportRatio", query.airTransportRatio |> Maybe.map Unit.encodeRatio |> Maybe.withDefault Encode.null )
        , ( "quality", query.quality |> Maybe.map Unit.encodeQuality |> Maybe.withDefault Encode.null )
        , ( "reparability", query.reparability |> Maybe.map Unit.encodeReparability |> Maybe.withDefault Encode.null )
        ]


encodeMaterialQuery : MaterialQuery -> Encode.Value
encodeMaterialQuery v =
    Encode.object
        [ ( "id", Material.encodeId v.id )
        , ( "share", Unit.encodeRatio v.share )
        , ( "recycledRatio", Unit.encodeRatio v.recycledRatio )
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



-- Parser


parseBase64Query : Parser (Maybe Query -> a) a
parseBase64Query =
    Parser.custom "QUERY" <|
        b64decode
            >> Result.toMaybe
            >> Just

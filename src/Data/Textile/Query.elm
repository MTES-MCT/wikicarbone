module Data.Textile.Query exposing
    ( MaterialQuery
    , Query
    , addMaterial
    , addTrim
    , b64decode
    , b64encode
    , buildApiQuery
    , decode
    , default
    , encode
    , handleUpcycling
    , isAdvancedQuery
    , jupeCotonAsie
    , parseBase64Query
    , regulatory
    , removeMaterial
    , removeTrim
    , tShirtCotonFrance
    , toggleStep
    , updateMaterial
    , updateMaterialSpinning
    , updateProduct
    , updateStepCountry
    , updateTrim
    , validateMaterials
    )

import Base64
import Data.Common.DecodeUtils as DU
import Data.Component as Component exposing (Item)
import Data.Country as Country
import Data.Split as Split exposing (Split)
import Data.Textile.Dyeing as Dyeing exposing (ProcessType)
import Data.Textile.Economics as Economics
import Data.Textile.Fabric as Fabric exposing (Fabric)
import Data.Textile.MakingComplexity as MakingComplexity exposing (MakingComplexity)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Material.Spinning as Spinning exposing (Spinning)
import Data.Textile.Printing as Printing exposing (Printing)
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Step.Label as Label exposing (Label)
import Data.Unit as Unit
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Json.Encode as Encode
import List.Extra as LE
import Mass exposing (Mass)
import Url.Parser as Parser exposing (Parser)


type alias MaterialQuery =
    { country : Maybe Country.Code
    , id : Material.Id
    , share : Split
    , spinning : Maybe Spinning
    }


type alias Query =
    { airTransportRatio : Maybe Split
    , business : Maybe Economics.Business
    , countryDyeing : Maybe Country.Code
    , countryFabric : Maybe Country.Code
    , countryMaking : Maybe Country.Code
    , countrySpinning : Maybe Country.Code
    , disabledSteps : List Label
    , dyeingProcessType : Maybe ProcessType
    , fabricProcess : Maybe Fabric
    , fading : Maybe Bool
    , makingComplexity : Maybe MakingComplexity
    , makingDeadStock : Maybe Split
    , makingWaste : Maybe Split
    , mass : Mass
    , materials : List MaterialQuery
    , numberOfReferences : Maybe Int
    , physicalDurability : Maybe Unit.PhysicalDurability
    , price : Maybe Economics.Price
    , printing : Maybe Printing
    , product : Product.Id
    , surfaceMass : Maybe Unit.SurfaceMass
    , traceability : Maybe Bool
    , trims : List Item
    , upcycled : Bool
    , yarnSize : Maybe Unit.YarnSize
    }


addMaterial : Material -> Query -> Query
addMaterial material query =
    let
        materialQuery =
            { country = Nothing
            , id = material.id
            , share = Split.zero
            , spinning = Nothing
            }
    in
    { query
        | materials =
            query.materials ++ [ materialQuery ]
    }


addTrim : Component.Id -> Query -> Query
addTrim id query =
    { query
        | trims =
            query.trims
                ++ [ { id = id, quantity = Component.quantityFromInt 1 } ]
    }


removeTrim : Component.Id -> Query -> Query
removeTrim id ({ trims } as query) =
    { query
        | trims = trims |> List.filter (.id >> (/=) id)
    }


updateTrim : Item -> Query -> Query
updateTrim newItem query =
    { query
        | trims =
            query.trims
                |> List.map
                    (\item ->
                        if item.id == newItem.id then
                            newItem

                        else
                            item
                    )
    }


buildApiQuery : String -> Query -> String
buildApiQuery clientUrl query =
    """curl -sS -X POST %apiUrl% \\
  -H "accept: application/json" \\
  -H "content-type: application/json" \\
  -d '%json%'
"""
        |> String.replace "%apiUrl%" (clientUrl ++ "api/textile/simulator")
        |> String.replace "%json%" (encode query |> Encode.encode 0)


decode : Decoder Query
decode =
    Decode.succeed Query
        |> DU.strictOptional "airTransportRatio" Split.decodeFloat
        |> DU.strictOptional "business" Economics.decodeBusiness
        |> DU.strictOptional "countryDyeing" Country.decodeCode
        |> DU.strictOptional "countryFabric" Country.decodeCode
        |> DU.strictOptional "countryMaking" Country.decodeCode
        |> DU.strictOptional "countrySpinning" Country.decodeCode
        |> Pipe.optional "disabledSteps" (Decode.list Label.decodeFromCode) []
        |> DU.strictOptional "dyeingProcessType" Dyeing.decode
        |> DU.strictOptional "fabricProcess" Fabric.decode
        |> DU.strictOptional "fading" Decode.bool
        |> DU.strictOptional "makingComplexity" MakingComplexity.decode
        |> DU.strictOptional "makingDeadStock" Split.decodeFloat
        |> DU.strictOptional "makingWaste" Split.decodeFloat
        |> Pipe.required "mass" (Decode.map Mass.kilograms Decode.float)
        |> Pipe.required "materials" (Decode.list decodeMaterialQuery)
        |> DU.strictOptional "numberOfReferences" Decode.int
        |> DU.strictOptional "physicalDurability" Unit.decodePhysicalDurability
        |> DU.strictOptional "price" Economics.decodePrice
        |> DU.strictOptional "printing" Printing.decode
        |> Pipe.required "product" (Decode.map Product.Id Decode.string)
        |> DU.strictOptional "surfaceMass" Unit.decodeSurfaceMass
        |> DU.strictOptional "traceability" Decode.bool
        |> Pipe.optional "trims" (Decode.list Component.decodeItem) []
        |> Pipe.optional "upcycled" Decode.bool False
        |> DU.strictOptional "yarnSize" Unit.decodeYarnSize


decodeMaterialQuery : Decoder MaterialQuery
decodeMaterialQuery =
    Decode.succeed MaterialQuery
        |> DU.strictOptional "country" Country.decodeCode
        |> Pipe.required "id" (Decode.map Material.Id Decode.string)
        |> Pipe.required "share" Split.decodeFloat
        |> DU.strictOptional "spinning" Spinning.decode


encode : Query -> Encode.Value
encode query =
    [ ( "airTransportRatio", query.airTransportRatio |> Maybe.map Split.encodeFloat )
    , ( "business", query.business |> Maybe.map Economics.encodeBusiness )
    , ( "countryDyeing", query.countryDyeing |> Maybe.map Country.encodeCode )
    , ( "countryFabric", query.countryFabric |> Maybe.map Country.encodeCode )
    , ( "countryMaking", query.countryMaking |> Maybe.map Country.encodeCode )
    , ( "countrySpinning", query.countrySpinning |> Maybe.map Country.encodeCode )
    , ( "disabledSteps"
      , case query.disabledSteps of
            [] ->
                Nothing

            list ->
                Encode.list Label.encode list |> Just
      )
    , ( "dyeingProcessType", query.dyeingProcessType |> Maybe.map Dyeing.encode )
    , ( "fabricProcess", query.fabricProcess |> Maybe.map Fabric.encode )
    , ( "fading", query.fading |> Maybe.map Encode.bool )
    , ( "makingComplexity", query.makingComplexity |> Maybe.map (MakingComplexity.toString >> Encode.string) )
    , ( "makingDeadStock", query.makingDeadStock |> Maybe.map Split.encodeFloat )
    , ( "makingWaste", query.makingWaste |> Maybe.map Split.encodeFloat )
    , ( "mass", query.mass |> Mass.inKilograms |> Encode.float |> Just )
    , ( "materials", query.materials |> Encode.list encodeMaterialQuery |> Just )
    , ( "numberOfReferences", query.numberOfReferences |> Maybe.map Encode.int )
    , ( "physicalDurability", query.physicalDurability |> Maybe.map Unit.encodePhysicalDurability )
    , ( "price", query.price |> Maybe.map Economics.encodePrice )
    , ( "printing", query.printing |> Maybe.map Printing.encode )
    , ( "product", query.product |> Product.idToString |> Encode.string |> Just )
    , ( "surfaceMass", query.surfaceMass |> Maybe.map Unit.encodeSurfaceMass )
    , ( "traceability", query.traceability |> Maybe.map Encode.bool )
    , ( "trims", query.trims |> Encode.list Component.encodeItem |> Just )
    , ( "upcycled", Encode.bool query.upcycled |> Just )
    , ( "yarnSize", query.yarnSize |> Maybe.map Unit.encodeYarnSize )
    ]
        -- For concision, drop keys where no param is defined
        |> List.filterMap (\( key, maybeVal ) -> maybeVal |> Maybe.map (\val -> ( key, val )))
        |> Encode.object


encodeMaterialQuery : MaterialQuery -> Encode.Value
encodeMaterialQuery v =
    [ ( "country", v.country |> Maybe.map Country.encodeCode )
    , ( "id", Material.encodeId v.id |> Just )
    , ( "share", Split.encodeFloat v.share |> Just )
    , ( "spinning", v.spinning |> Maybe.map Spinning.encode )
    ]
        |> List.filterMap (\( key, maybeVal ) -> maybeVal |> Maybe.map (\val -> ( key, val )))
        |> Encode.object


removeMaterial : Material.Id -> Query -> Query
removeMaterial materialId query =
    { query | materials = query.materials |> List.filter (\m -> m.id /= materialId) }
        |> (\newQuery ->
                -- set share to 100% when a single material remains
                if List.length newQuery.materials == 1 then
                    newQuery.materials
                        |> List.head
                        |> Maybe.map (\m -> updateMaterialShare m.id Split.full newQuery)
                        |> Maybe.withDefault newQuery

                else
                    newQuery
           )


{-| Handle the case of upcycling: when a garment is upcycled, we disable the Material, Spinning,
Fabric and Ennobling steps and enforce the use of a high making complexity
-}
handleUpcycling : Query -> Query
handleUpcycling query =
    if query.upcycled then
        { query
            | disabledSteps = LE.unique <| Label.upcyclables ++ query.disabledSteps
            , makingComplexity = query.makingComplexity |> Maybe.withDefault MakingComplexity.High |> Just
        }

    else
        query


isAdvancedQuery : List Product -> Query -> Bool
isAdvancedQuery products query =
    List.any identity
        [ query.dyeingProcessType /= Nothing || query.dyeingProcessType /= Just Dyeing.Average
        , query.fabricProcess /= Nothing
        , query.makingComplexity /= Nothing
        , query.makingDeadStock /= Nothing
        , query.makingWaste /= Nothing
        , query.materials |> List.any (.spinning >> (/=) Nothing)
        , query.physicalDurability /= Nothing
        , query.surfaceMass /= Nothing
        , products
            |> Product.findById query.product
            |> Result.map (.trims >> (/=) query.trims)
            |> Result.withDefault False
        , not query.upcycled && List.length query.disabledSteps > 0
        , query.yarnSize /= Nothing
        ]


{-| Resets a query to use only regulatory-level fields.
-}
regulatory : List Product -> Query -> Query
regulatory products query =
    { query
        | disabledSteps = []
        , dyeingProcessType = Nothing
        , fabricProcess = Nothing
        , makingComplexity = Nothing
        , makingDeadStock = Nothing
        , makingWaste = Nothing
        , materials = query.materials |> List.map (\m -> { m | spinning = Nothing })
        , physicalDurability = Nothing
        , surfaceMass = Nothing
        , trims =
            products
                |> Product.findById query.product
                |> Result.map .trims
                |> Result.withDefault []
        , yarnSize = Nothing
    }


toggleStep : Label -> Query -> Query
toggleStep label query =
    { query
        | disabledSteps =
            if List.member label query.disabledSteps then
                List.filter ((/=) label) query.disabledSteps

            else
                label :: query.disabledSteps
    }


updateMaterial : Material.Id -> MaterialQuery -> Query -> Query
updateMaterial oldMaterialId newMaterial =
    updateMaterialQuery oldMaterialId
        (\materialQuery ->
            { materialQuery
                | country = newMaterial.country
                , id = newMaterial.id
                , share = newMaterial.share
                , spinning = Nothing
            }
        )


updateMaterialQuery : Material.Id -> (MaterialQuery -> MaterialQuery) -> Query -> Query
updateMaterialQuery materialId update query =
    { query | materials = query.materials |> LE.updateIf (.id >> (==) materialId) update }


updateMaterialShare : Material.Id -> Split -> Query -> Query
updateMaterialShare materialId share =
    updateMaterialQuery materialId
        (\materialQuery -> { materialQuery | share = share })


updateMaterialSpinning : Material -> Spinning -> Query -> Query
updateMaterialSpinning material spinning query =
    { query
        | materials =
            query.materials
                |> List.map
                    (\materialQuery ->
                        if materialQuery.id == material.id then
                            { materialQuery | spinning = Just spinning }

                        else
                            materialQuery
                    )
    }


updateProduct : Product -> Query -> Query
updateProduct product query =
    if product.id /= query.product then
        -- Product category has changed, reset a bunch of related query params
        { query
            | dyeingProcessType = Nothing
            , fabricProcess = Nothing
            , makingComplexity = Nothing
            , makingDeadStock = Nothing
            , makingWaste = Nothing
            , printing = Nothing
            , product = product.id
            , surfaceMass = Nothing
            , trims = product.trims
            , yarnSize = Nothing
        }

    else
        query


updateStepCountry : Label -> Country.Code -> Query -> Query
updateStepCountry label code query =
    let
        maybeCode =
            if code == Country.unknownCountryCode then
                Nothing

            else
                Just code
    in
    case label of
        Label.Ennobling ->
            { query | countryDyeing = maybeCode }

        Label.Fabric ->
            { query | countryFabric = maybeCode }

        Label.Making ->
            { query
                | airTransportRatio =
                    if query.countryMaking /= maybeCode then
                        -- reset custom value as we just switched country
                        Nothing

                    else
                        query.airTransportRatio
                , countryMaking = maybeCode
            }

        Label.Spinning ->
            { query | countrySpinning = maybeCode }

        _ ->
            query


validateMaterials : List MaterialQuery -> Result String (List MaterialQuery)
validateMaterials materials =
    if materials == [] then
        Ok []

    else
        let
            total =
                materials
                    |> List.map (.share >> Split.toFloat)
                    |> List.sum
        in
        -- Note: taking care of float number rounding precision errors https://en.wikipedia.org/wiki/Round-off_error
        if not (List.member total [ 1, 0.6 + 0.3 + 0.1 ]) then
            Err <|
                "La somme des parts de matières doit être égale à 1 (ici : "
                    ++ String.fromFloat total
                    ++ ")"

        else
            Ok materials



-- Sample data


default : Query
default =
    { airTransportRatio = Nothing
    , business = Nothing
    , countryDyeing = Just (Country.Code "CN")
    , countryFabric = Just (Country.Code "CN")
    , countryMaking = Just (Country.Code "CN")
    , countrySpinning = Just (Country.Code "CN")
    , disabledSteps = []
    , dyeingProcessType = Nothing
    , fabricProcess = Nothing
    , fading = Nothing
    , makingComplexity = Nothing
    , makingDeadStock = Nothing
    , makingWaste = Nothing
    , mass = Mass.kilograms 0.17
    , materials =
        [ { country = Nothing
          , id = Material.Id "ei-coton"
          , share = Split.full
          , spinning = Nothing
          }
        ]
    , numberOfReferences = Nothing
    , physicalDurability = Nothing
    , price = Nothing
    , printing = Nothing
    , product = Product.Id "tshirt"
    , surfaceMass = Nothing
    , traceability = Nothing
    , trims = []
    , upcycled = False
    , yarnSize = Nothing
    }


jupeCotonAsie : Query
jupeCotonAsie =
    { default
        | fabricProcess = Just Fabric.Weaving
        , mass = Mass.kilograms 0.3
        , product = Product.Id "jupe"
    }


tShirtCotonFrance : Query
tShirtCotonFrance =
    { default
        | countryDyeing = Just (Country.Code "FR")
        , countryFabric = Just (Country.Code "FR")
        , countryMaking = Just (Country.Code "FR")
        , countrySpinning = Just (Country.Code "FR")
    }



-- Parser


b64decode : String -> Result String Query
b64decode =
    Base64.decode
        >> Result.andThen
            (Decode.decodeString decode
                >> Result.mapError Decode.errorToString
            )


b64encode : Query -> String
b64encode =
    encode >> Encode.encode 0 >> Base64.encode


parseBase64Query : Parser (Maybe Query -> a) a
parseBase64Query =
    Parser.custom "QUERY" <|
        b64decode
            >> Result.toMaybe
            >> Just

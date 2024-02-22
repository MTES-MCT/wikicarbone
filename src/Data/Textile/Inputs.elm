module Data.Textile.Inputs exposing
    ( Inputs
    , MaterialInput
    , MaterialQuery
    , Query
    , addMaterial
    , b64decode
    , b64encode
    , buildApiQuery
    , computeMaterialTransport
    , countryList
    , decodeQuery
    , defaultQuery
    , encode
    , encodeQuery
    , exampleProductToCategory
    , exampleProductToString
    , exampleProducts
    , fromQuery
    , getMaterialMicrofibersComplement
    , getMaterialsOriginShares
    , getOutOfEuropeEOLComplement
    , getOutOfEuropeEOLProbability
    , getTotalMicrofibersComplement
    , isFaded
    , jupeCotonAsie
    , parseBase64Query
    , productsAndNames
    , removeMaterial
    , tShirtCotonAsie
    , tShirtCotonFrance
    , toQuery
    , toString
    , toggleStep
    , updateMaterial
    , updateMaterialSpinning
    , updateProduct
    , updateStepCountry
    )

import Base64
import Data.Country as Country exposing (Country)
import Data.Impact as Impact
import Data.Scope as Scope
import Data.Split as Split exposing (Split)
import Data.Textile.DyeingMedium as DyeingMedium exposing (DyeingMedium)
import Data.Textile.Economics as Economics
import Data.Textile.Fabric as Fabric exposing (Fabric)
import Data.Textile.MakingComplexity as MakingComplexity exposing (MakingComplexity)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Material.Origin as Origin exposing (Origin)
import Data.Textile.Material.Spinning as Spinning exposing (Spinning)
import Data.Textile.Printing as Printing exposing (Printing)
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Step.Label as Label exposing (Label)
import Data.Transport as Transport exposing (Distances, Transport)
import Data.Unit as Unit
import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Json.Encode as Encode
import List.Extra as LE
import Mass exposing (Mass)
import Quantity
import Result.Extra as RE
import Static.Db exposing (Db)
import Url.Parser as Parser exposing (Parser)
import Views.Format as Format


type alias MaterialInput =
    { material : Material
    , share : Split
    , spinning : Maybe Spinning
    , country : Maybe Country
    }


type alias Inputs =
    { mass : Mass
    , materials : List MaterialInput
    , product : Product

    -- TODO: countryMaterial isn't used anymore, but we still need it because `countryList` uses it,
    -- which in turn is used to build the lifecycle, which needs a country for each step.
    , countryMaterial : Country
    , countrySpinning : Country
    , countryFabric : Country
    , countryDyeing : Country
    , countryMaking : Country
    , countryDistribution : Country
    , countryUse : Country
    , countryEndOfLife : Country
    , airTransportRatio : Maybe Split
    , makingWaste : Maybe Split
    , makingDeadStock : Maybe Split
    , makingComplexity : Maybe MakingComplexity
    , yarnSize : Maybe Unit.YarnSize
    , surfaceMass : Maybe Unit.SurfaceMass
    , fabricProcess : Fabric
    , disabledSteps : List Label
    , fading : Maybe Bool
    , dyeingMedium : Maybe DyeingMedium
    , printing : Maybe Printing
    , business : Maybe Economics.Business
    , marketingDuration : Maybe Duration
    , numberOfReferences : Maybe Int
    , price : Maybe Economics.Price
    , traceability : Maybe Bool
    }


type alias MaterialQuery =
    { id : Material.Id
    , share : Split
    , spinning : Maybe Spinning
    , country : Maybe Country.Code
    }


type alias Query =
    { mass : Mass
    , materials : List MaterialQuery
    , product : Product.Id
    , countrySpinning : Maybe Country.Code
    , countryFabric : Country.Code
    , countryDyeing : Country.Code
    , countryMaking : Country.Code
    , airTransportRatio : Maybe Split
    , makingWaste : Maybe Split
    , makingDeadStock : Maybe Split
    , makingComplexity : Maybe MakingComplexity
    , yarnSize : Maybe Unit.YarnSize
    , surfaceMass : Maybe Unit.SurfaceMass
    , fabricProcess : Fabric
    , disabledSteps : List Label
    , fading : Maybe Bool
    , dyeingMedium : Maybe DyeingMedium
    , printing : Maybe Printing
    , business : Maybe Economics.Business
    , marketingDuration : Maybe Duration
    , numberOfReferences : Maybe Int
    , price : Maybe Economics.Price
    , traceability : Maybe Bool
    }


isFaded : Inputs -> Bool
isFaded inputs =
    inputs.fading == Just True || (inputs.fading == Nothing && Product.isFadedByDefault inputs.product)


toMaterialInputs : List Material -> List Country -> List MaterialQuery -> Result String (List MaterialInput)
toMaterialInputs materials countries =
    List.map
        (\{ id, share, spinning, country } ->
            let
                countryResult =
                    case country of
                        Just countryCode ->
                            Country.findByCode countryCode countries
                                |> Result.map Just

                        Nothing ->
                            Ok Nothing
            in
            Result.map2
                (\material_ country_ ->
                    { material = material_
                    , share = share
                    , spinning = spinning
                    , country = country_
                    }
                )
                (Material.findById id materials)
                countryResult
        )
        >> RE.combine


toMaterialQuery : List MaterialInput -> List MaterialQuery
toMaterialQuery =
    List.map
        (\{ material, share, spinning, country } ->
            { id = material.id
            , share = share
            , spinning = spinning
            , country = country |> Maybe.map .code
            }
        )


getMainMaterial : List MaterialInput -> Result String Material
getMainMaterial =
    List.sortBy (.share >> Split.toFloat)
        >> List.reverse
        >> List.head
        >> Maybe.map .material
        >> Result.fromMaybe "La liste de matières est vide."


getMainMaterialCountry : List Country -> List MaterialInput -> Result String Country
getMainMaterialCountry countries =
    getMainMaterial
        >> Result.andThen
            (\{ defaultCountry } ->
                Country.findByCode defaultCountry countries
            )


fromQuery : Db -> Query -> Result String Inputs
fromQuery { countries, textile } query =
    let
        materials =
            query.materials
                |> toMaterialInputs textile.materials countries

        franceResult =
            Country.findByCode (Country.Code "FR") countries

        -- TODO: we don't use the main material country anymore as each material can specify
        -- its own country. We still need a country per step though, so we'll just default
        -- to using France.
        mainMaterialCountry =
            materials
                |> Result.andThen (getMainMaterialCountry countries)
                |> RE.orElse franceResult
    in
    Ok Inputs
        |> RE.andMap (Ok query.mass)
        |> RE.andMap materials
        |> RE.andMap (textile.products |> Product.findById query.product)
        -- Material country is constrained to be the first material's default country
        |> RE.andMap mainMaterialCountry
        -- Spinning country is either provided by query or fallbacks to material's default
        -- country, making the parameter optional
        |> RE.andMap
            (case query.countrySpinning of
                Just spinningCountryCode ->
                    Country.findByCode spinningCountryCode countries

                Nothing ->
                    mainMaterialCountry
            )
        |> RE.andMap (countries |> Country.findByCode query.countryFabric)
        |> RE.andMap (countries |> Country.findByCode query.countryDyeing)
        |> RE.andMap (countries |> Country.findByCode query.countryMaking)
        -- The distribution country is always France
        |> RE.andMap franceResult
        -- The use country is always France
        |> RE.andMap franceResult
        -- The end of life country is always France
        |> RE.andMap franceResult
        |> RE.andMap (Ok query.airTransportRatio)
        |> RE.andMap (Ok query.makingWaste)
        |> RE.andMap (Ok query.makingDeadStock)
        |> RE.andMap (Ok query.makingComplexity)
        |> RE.andMap (Ok query.yarnSize)
        |> RE.andMap (Ok query.surfaceMass)
        |> RE.andMap (Ok query.fabricProcess)
        |> RE.andMap (Ok query.disabledSteps)
        |> RE.andMap (Ok query.fading)
        |> RE.andMap (Ok query.dyeingMedium)
        |> RE.andMap (Ok query.printing)
        |> RE.andMap (Ok query.business)
        |> RE.andMap (Ok query.marketingDuration)
        |> RE.andMap (Ok query.numberOfReferences)
        |> RE.andMap (Ok query.price)
        |> RE.andMap (Ok query.traceability)


toQuery : Inputs -> Query
toQuery inputs =
    { mass = inputs.mass
    , materials = toMaterialQuery inputs.materials
    , product = inputs.product.id
    , countrySpinning = Just inputs.countrySpinning.code
    , countryFabric = inputs.countryFabric.code
    , countryDyeing = inputs.countryDyeing.code
    , countryMaking = inputs.countryMaking.code
    , airTransportRatio = inputs.airTransportRatio
    , makingWaste = inputs.makingWaste
    , makingDeadStock = inputs.makingDeadStock
    , makingComplexity = inputs.makingComplexity
    , yarnSize = inputs.yarnSize
    , surfaceMass = inputs.surfaceMass
    , fabricProcess = inputs.fabricProcess
    , disabledSteps = inputs.disabledSteps
    , fading = inputs.fading
    , dyeingMedium = inputs.dyeingMedium
    , printing = inputs.printing
    , business = inputs.business
    , marketingDuration = inputs.marketingDuration
    , numberOfReferences = inputs.numberOfReferences
    , price = inputs.price
    , traceability = inputs.traceability
    }


stepsToStrings : Inputs -> List (List String)
stepsToStrings inputs =
    let
        ifStepEnabled label list =
            if not (List.member label inputs.disabledSteps) then
                list

            else
                []
    in
    [ [ inputs.product.name, Format.kgToString inputs.mass ]
    , ifStepEnabled Label.Material
        [ "matière"
        , materialsToString inputs.materials
        ]
    , ifStepEnabled Label.Spinning
        [ "filature"
        , inputs.countrySpinning.name
        ]
    , case inputs.yarnSize of
        Just yarnSize ->
            [ "titrage", String.fromInt (Unit.yarnSizeInKilometers yarnSize) ++ "Nm" ]

        Nothing ->
            []
    , ifStepEnabled Label.Fabric
        [ Fabric.toLabel inputs.fabricProcess
        , inputs.countryFabric.name
        ]
    , ifStepEnabled Label.Ennobling
        [ case inputs.dyeingMedium of
            Just dyeingMedium ->
                "ennoblissement\u{00A0}: teinture sur " ++ DyeingMedium.toLabel dyeingMedium

            Nothing ->
                "ennoblissement"
        , inputs.countryDyeing.name
        ]
    , ifStepEnabled Label.Ennobling
        [ "impression"
        , case inputs.printing of
            Just printing ->
                "impression " ++ Printing.toFullLabel printing ++ "\u{00A0}: " ++ inputs.countryDyeing.name

            Nothing ->
                "non"
        ]
    , ifStepEnabled Label.Making
        [ "confection"
        , inputs.countryMaking.name ++ makingOptionsToString inputs
        ]
    , ifStepEnabled Label.Distribution
        [ "distribution"
        , inputs.countryDistribution.name
        ]
    , ifStepEnabled Label.Use
        [ "utilisation"
        , inputs.countryUse.name
        ]
    , ifStepEnabled Label.EndOfLife
        [ "fin de vie"
        , inputs.countryEndOfLife.name
        ]
    ]
        |> List.filter (not << List.isEmpty)


toString : Inputs -> String
toString inputs =
    inputs
        |> stepsToStrings
        |> List.map (String.join "\u{00A0}: ")
        |> String.join ", "


materialsToString : List MaterialInput -> String
materialsToString materials =
    materials
        |> List.filter (\{ share } -> Split.toFloat share > 0)
        |> List.map
            (\{ material, share, country } ->
                let
                    countryName =
                        country
                            |> Maybe.map .name
                            |> Maybe.withDefault (" par défaut (" ++ material.geographicOrigin ++ ")")
                in
                Split.toPercentString share
                    ++ "% "
                    ++ material.shortName
                    ++ " provenance "
                    ++ countryName
            )
        |> String.join ", "


makingOptionsToString : Inputs -> String
makingOptionsToString { makingWaste, makingDeadStock, makingComplexity, airTransportRatio, fading } =
    [ makingWaste
        |> Maybe.map (Split.toPercentString >> (\s -> s ++ "\u{202F}% de perte"))
    , makingDeadStock
        |> Maybe.map (Split.toPercentString >> (\s -> s ++ "\u{202F}% de stocks dormants"))
    , makingComplexity
        |> Maybe.map (\complexity -> "complexité de confection " ++ MakingComplexity.toLabel complexity)
    , airTransportRatio
        |> Maybe.andThen
            (\ratio ->
                if Split.toPercent ratio == 0 then
                    Nothing

                else
                    Just (Split.toPercentString ratio ++ " de transport aérien")
            )
    , if fading == Just True then
        Just "délavé"

      else
        Nothing
    ]
        |> List.filterMap identity
        |> String.join ", "
        |> (\s ->
                if s /= "" then
                    " (" ++ s ++ ")"

                else
                    ""
           )


countryList : Inputs -> List Country
countryList inputs =
    [ inputs.countryMaterial
    , inputs.countrySpinning
    , inputs.countryFabric
    , inputs.countryDyeing
    , inputs.countryMaking
    , inputs.countryDistribution
    , inputs.countryUse
    , inputs.countryEndOfLife
    ]


updateStepCountry : Label -> Country.Code -> Query -> Query
updateStepCountry label code query =
    case label of
        Label.Spinning ->
            { query | countrySpinning = Just code }

        Label.Fabric ->
            { query | countryFabric = code }

        Label.Ennobling ->
            { query | countryDyeing = code }

        Label.Making ->
            { query
                | countryMaking = code
                , airTransportRatio =
                    if query.countryMaking /= code then
                        -- reset custom value as we just switched country
                        Nothing

                    else
                        query.airTransportRatio
            }

        _ ->
            query


toggleStep : Label -> Query -> Query
toggleStep label query =
    { query
        | disabledSteps =
            if List.member label query.disabledSteps then
                List.filter ((/=) label) query.disabledSteps

            else
                label :: query.disabledSteps
    }


addMaterial : Material -> Query -> Query
addMaterial material query =
    let
        materialQuery =
            { id = material.id
            , share = Split.zero
            , spinning = Nothing
            , country = Nothing
            }
    in
    { query
        | materials =
            query.materials ++ [ materialQuery ]
    }


updateMaterialQuery : Material.Id -> (MaterialQuery -> MaterialQuery) -> Query -> Query
updateMaterialQuery materialId update query =
    { query | materials = query.materials |> LE.updateIf (.id >> (==) materialId) update }


updateMaterial : Material.Id -> MaterialQuery -> Query -> Query
updateMaterial oldMaterialId newMaterial =
    updateMaterialQuery oldMaterialId
        (\materialQuery ->
            { materialQuery
                | id = newMaterial.id
                , share = newMaterial.share
                , spinning = Nothing
                , country = newMaterial.country
            }
        )


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


updateProduct : Product -> Query -> Query
updateProduct product query =
    if product.id /= query.product then
        -- Product has changed, reset a bunch of related query params
        { query
            | product = product.id
            , mass = product.mass
            , makingWaste = Nothing
            , makingDeadStock = Nothing
            , makingComplexity = Nothing
            , yarnSize = Nothing
            , surfaceMass = Nothing
            , fabricProcess = product.fabric
            , fading = Nothing
            , dyeingMedium = Nothing
            , printing = Nothing
        }

    else
        query


getMaterialMicrofibersComplement : Mass -> MaterialInput -> Unit.Impact
getMaterialMicrofibersComplement finalProductMass { material, share } =
    -- Note: Impact is computed against the final product mass, because microfibers
    --       are always released in the environment from finished products.
    let
        materialMassInKg =
            share
                |> Split.applyToQuantity finalProductMass
                |> Mass.inKilograms
    in
    Origin.toMicrofibersComplement material.origin
        |> Quantity.multiplyBy materialMassInKg


getTotalMicrofibersComplement : Inputs -> Unit.Impact
getTotalMicrofibersComplement { mass, materials } =
    materials
        |> List.map (getMaterialMicrofibersComplement mass)
        |> Quantity.sum


getMaterialsOriginShares : List MaterialInput -> Origin.Shares
getMaterialsOriginShares materialInputs =
    { artificialFromInorganic = materialInputs |> getMaterialCategoryShare Origin.ArtificialFromInorganic
    , artificialFromOrganic = materialInputs |> getMaterialCategoryShare Origin.ArtificialFromOrganic
    , naturalFromAnimal = materialInputs |> getMaterialCategoryShare Origin.NaturalFromAnimal
    , naturalFromVegetal = materialInputs |> getMaterialCategoryShare Origin.NaturalFromVegetal
    , synthetic = materialInputs |> getMaterialCategoryShare Origin.Synthetic
    }


getMaterialCategoryShare : Origin -> List MaterialInput -> Split
getMaterialCategoryShare origin =
    List.filterMap
        (\{ material, share } ->
            if material.origin == origin then
                Just (Split.toPercent share)

            else
                Nothing
        )
        >> List.sum
        >> Split.fromPercent
        >> Result.withDefault Split.zero


getOutOfEuropeEOLProbability : List MaterialInput -> Split
getOutOfEuropeEOLProbability materialInputs =
    -- We consider that the garment enters the "synthetic materials" category as
    -- soon as synthetic materials represent more than 10% of its composition.
    let
        syntheticMaterialsShare =
            materialInputs
                |> getMaterialCategoryShare Origin.Synthetic
    in
    Split.fromFloat
        (if Split.toPercent syntheticMaterialsShare >= 10 then
            0.11

         else
            0.06
        )
        |> Result.withDefault Split.zero


getOutOfEuropeEOLComplement : Inputs -> Unit.Impact
getOutOfEuropeEOLComplement { mass, materials } =
    -- Note: this complement is a malus, hence the minus sign
    Unit.impact
        -(Split.toFloat (getOutOfEuropeEOLProbability materials)
            * Mass.inKilograms mass
            * 5000
         )


computeMaterialTransport : Distances -> Country.Code -> MaterialInput -> Transport
computeMaterialTransport distances nextCountryCode { material, country, share } =
    if share /= Split.zero then
        let
            emptyImpacts =
                Impact.empty

            countryCode =
                country
                    |> Maybe.map .code
                    |> Maybe.withDefault material.defaultCountry
        in
        distances
            |> Transport.getTransportBetween
                Scope.Textile
                emptyImpacts
                countryCode
                nextCountryCode

    else
        Transport.default Impact.empty


buildApiQuery : String -> Query -> String
buildApiQuery clientUrl query =
    """curl -X POST %apiUrl% \\
  -H "accept: application/json" \\
  -H "content-type: application/json" \\
  -d '%json%'
"""
        |> String.replace "%apiUrl%" (clientUrl ++ "api/textile/simulator")
        |> String.replace "%json%" (encodeQuery query |> Encode.encode 0)


encode : Inputs -> Encode.Value
encode inputs =
    Encode.object
        [ ( "mass", Encode.float (Mass.inKilograms inputs.mass) )
        , ( "materials", Encode.list encodeMaterialInput inputs.materials )
        , ( "product", Product.encode inputs.product )
        , ( "countryFabric", Country.encode inputs.countryFabric )
        , ( "countryDyeing", Country.encode inputs.countryDyeing )
        , ( "countryMaking", Country.encode inputs.countryMaking )
        , ( "airTransportRatio", inputs.airTransportRatio |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "makingWaste", inputs.makingWaste |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "makingDeadStock", inputs.makingDeadStock |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "makingComplexity", inputs.makingComplexity |> Maybe.map (MakingComplexity.toString >> Encode.string) |> Maybe.withDefault Encode.null )
        , ( "yarnSize", inputs.yarnSize |> Maybe.map Unit.encodeYarnSize |> Maybe.withDefault Encode.null )
        , ( "surfaceMass", inputs.surfaceMass |> Maybe.map Unit.encodeSurfaceMass |> Maybe.withDefault Encode.null )
        , ( "fabricProcess", inputs.fabricProcess |> Fabric.encode )
        , ( "disabledSteps", Encode.list Label.encode inputs.disabledSteps )
        , ( "fading", inputs.fading |> Maybe.map Encode.bool |> Maybe.withDefault Encode.null )
        , ( "dyeingMedium", inputs.dyeingMedium |> Maybe.map DyeingMedium.encode |> Maybe.withDefault Encode.null )
        , ( "printing", inputs.printing |> Maybe.map Printing.encode |> Maybe.withDefault Encode.null )
        , ( "business", inputs.business |> Maybe.map Economics.encodeBusiness |> Maybe.withDefault Encode.null )
        , ( "marketingDuration", inputs.marketingDuration |> Maybe.map (Duration.inDays >> Encode.float) |> Maybe.withDefault Encode.null )
        , ( "numberOfReferences", inputs.numberOfReferences |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "price", inputs.price |> Maybe.map Economics.encodePrice |> Maybe.withDefault Encode.null )
        , ( "traceability", inputs.traceability |> Maybe.map Encode.bool |> Maybe.withDefault Encode.null )
        ]


encodeMaterialInput : MaterialInput -> Encode.Value
encodeMaterialInput v =
    [ ( "material", Material.encode v.material |> Just )
    , ( "share", Split.encodeFloat v.share |> Just )
    , ( "spinning", v.spinning |> Maybe.map Spinning.encode )
    , ( "country", v.country |> Maybe.map (.code >> Country.encodeCode) )
    ]
        |> List.filterMap (\( key, maybeVal ) -> maybeVal |> Maybe.map (\val -> ( key, val )))
        |> Encode.object


decodeQuery : Decoder Query
decodeQuery =
    Decode.succeed Query
        |> Pipe.required "mass" (Decode.map Mass.kilograms Decode.float)
        |> Pipe.required "materials" (Decode.list decodeMaterialQuery)
        |> Pipe.required "product" (Decode.map Product.Id Decode.string)
        |> Pipe.optional "countrySpinning" (Decode.maybe Country.decodeCode) Nothing
        |> Pipe.required "countryFabric" Country.decodeCode
        |> Pipe.required "countryDyeing" Country.decodeCode
        |> Pipe.required "countryMaking" Country.decodeCode
        |> Pipe.optional "airTransportRatio" (Decode.maybe Split.decodeFloat) Nothing
        |> Pipe.optional "makingWaste" (Decode.maybe Split.decodeFloat) Nothing
        |> Pipe.optional "makingDeadStock" (Decode.maybe Split.decodeFloat) Nothing
        |> Pipe.optional "makingComplexity" (Decode.maybe MakingComplexity.decode) Nothing
        |> Pipe.optional "yarnSize" (Decode.maybe Unit.decodeYarnSize) Nothing
        |> Pipe.optional "surfaceMass" (Decode.maybe Unit.decodeSurfaceMass) Nothing
        |> Pipe.required "fabricProcess" Fabric.decode
        |> Pipe.optional "disabledSteps" (Decode.list Label.decodeFromCode) []
        |> Pipe.optional "fading" (Decode.maybe Decode.bool) Nothing
        |> Pipe.optional "dyeingMedium" (Decode.maybe DyeingMedium.decode) Nothing
        |> Pipe.optional "printing" (Decode.maybe Printing.decode) Nothing
        |> Pipe.optional "business" (Decode.maybe Economics.decodeBusiness) Nothing
        |> Pipe.optional "marketingDuration" (Decode.maybe (Decode.map Duration.days Decode.float)) Nothing
        |> Pipe.optional "numberOfReferences" (Decode.maybe Decode.int) Nothing
        |> Pipe.optional "price" (Decode.maybe Economics.decodePrice) Nothing
        |> Pipe.optional "traceability" (Decode.maybe Decode.bool) Nothing


decodeMaterialQuery : Decoder MaterialQuery
decodeMaterialQuery =
    Decode.succeed MaterialQuery
        |> Pipe.required "id" (Decode.map Material.Id Decode.string)
        |> Pipe.required "share" Split.decodeFloat
        |> Pipe.optional "spinning" (Decode.maybe Spinning.decode) Nothing
        |> Pipe.optional "country" (Decode.maybe Country.decodeCode) Nothing


encodeQuery : Query -> Encode.Value
encodeQuery query =
    [ ( "mass", query.mass |> Mass.inKilograms |> Encode.float |> Just )
    , ( "materials", query.materials |> Encode.list encodeMaterialQuery |> Just )
    , ( "product", query.product |> Product.idToString |> Encode.string |> Just )
    , ( "countrySpinning", query.countrySpinning |> Maybe.map Country.encodeCode )
    , ( "countryFabric", query.countryFabric |> Country.encodeCode |> Just )
    , ( "countryDyeing", query.countryDyeing |> Country.encodeCode |> Just )
    , ( "countryMaking", query.countryMaking |> Country.encodeCode |> Just )
    , ( "airTransportRatio", query.airTransportRatio |> Maybe.map Split.encodeFloat )
    , ( "makingWaste", query.makingWaste |> Maybe.map Split.encodeFloat )
    , ( "makingDeadStock", query.makingDeadStock |> Maybe.map Split.encodeFloat )
    , ( "makingComplexity", query.makingComplexity |> Maybe.map (MakingComplexity.toString >> Encode.string) )
    , ( "yarnSize", query.yarnSize |> Maybe.map Unit.encodeYarnSize )
    , ( "surfaceMass", query.surfaceMass |> Maybe.map Unit.encodeSurfaceMass )
    , ( "fabricProcess", query.fabricProcess |> Fabric.encode |> Just )
    , ( "disabledSteps"
      , case query.disabledSteps of
            [] ->
                Nothing

            list ->
                Encode.list Label.encode list |> Just
      )
    , ( "fading", query.fading |> Maybe.map Encode.bool )
    , ( "dyeingMedium", query.dyeingMedium |> Maybe.map DyeingMedium.encode )
    , ( "printing", query.printing |> Maybe.map Printing.encode )
    , ( "business", query.business |> Maybe.map Economics.encodeBusiness )
    , ( "marketingDuration", query.marketingDuration |> Maybe.map (Duration.inDays >> Encode.float) )
    , ( "numberOfReferences", query.numberOfReferences |> Maybe.map Encode.int )
    , ( "price", query.price |> Maybe.map Economics.encodePrice )
    , ( "traceability", query.traceability |> Maybe.map Encode.bool )
    ]
        -- For concision, drop keys where no param is defined
        |> List.filterMap (\( key, maybeVal ) -> maybeVal |> Maybe.map (\val -> ( key, val )))
        |> Encode.object


encodeMaterialQuery : MaterialQuery -> Encode.Value
encodeMaterialQuery v =
    [ ( "id", Material.encodeId v.id |> Just )
    , ( "share", Split.encodeFloat v.share |> Just )
    , ( "spinning", v.spinning |> Maybe.map Spinning.encode )
    , ( "country", v.country |> Maybe.map Country.encodeCode )
    ]
        |> List.filterMap (\( key, maybeVal ) -> maybeVal |> Maybe.map (\val -> ( key, val )))
        |> Encode.object


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



---- Example products


type alias ExampleProduct =
    { name : String
    , query : Query
    , category : String
    }


productsAndNames : List ExampleProduct
productsAndNames =
    -- 7 base products, from China
    [ { name = "Jupe 100% coton Asie (300g)", query = jupeCotonAsie, category = "Jupe / Robe" }
    , { name = "Chemise 100% coton Asie (250g)", query = chemiseCotonAsie, category = "Chemise" }
    , { name = "Jean 100% coton Asie (450g)", query = jeanCotonAsie, category = "Jean" }
    , { name = "Manteau 100% coton Asie (950g)", query = manteauCotonAsie, category = "Manteau / Veste" }
    , { name = "Pantalon 100% coton Asie (450g)", query = pantalonCotonAsie, category = "Pantalon / Short" }

    -- 7 base products, from France
    , { name = "Jupe 100% coton France (300g)", query = jupeCotonFrance, category = "Jupe / Robe" }
    , { name = "Chemise 100% coton France (250g)", query = chemiseCotonFrance, category = "Chemise" }
    , { name = "Jean 100% coton France (450g)", query = jeanCotonFrance, category = "Jean" }
    , { name = "Manteau 100% coton France (950g)", query = manteauCotonFrance, category = "Manteau / Veste" }
    , { name = "Pantalon 100% coton France (450g)", query = pantalonCotonFrance, category = "Pantalon / Short" }

    -- Various examples
    , { name = "Jupe 100% polyester Asie (300g)", query = jupePolyesterAsie, category = "Jupe / Robe" }
    , { name = "Manteau 50% polyamide 50% coton Asie (950g)", query = manteauMixAsie, category = "Manteau / Veste" }

    -- More examples
    , { name = "Tshirt coton bio France marque éthique (150g)", query = tshirtCotonBiofranceMarqueEthique, category = "Tshirt / Polo" }
    , { name = "Tshirt synthétique Chine ultra fast fashion (150g)", query = tshirtSynthetiqueChineUltraFastFashion, category = "Tshirt / Polo" }
    , { name = "Tshirt coton Asie fast fashion (150g)", query = tshirtCotonAsieFastFashion, category = "Tshirt / Polo" }
    , { name = "Tshirt coton Pakistan marque traditionnelle (150g)", query = tshirtCotonPakistanMarqueTraditionnelle, category = "Tshirt / Polo" }
    , { name = "Tshirt coton France marque traditionnelle (150g)", query = tshirtCotonFranceMarqueTraditionnelle, category = "Tshirt / Polo" }
    , { name = "Tshirt lin France (100%) marque éthique (150g)", query = tshirtLinfranceMarqueEthique, category = "Tshirt / Polo" }
    , { name = "Pull laine France marque éthique (550g) [A CONSOLIDER]", query = pullLaineFranceMarqueEthique, category = "Pull / Couche intermédiaire" }
    , { name = "Pull laine paysanne France (100%) marque éthique (550g) [A CONSOLIDER]", query = pullLainePaysanneFranceMarqueEthique, category = "Pull / Couche intermédiaire" }
    , { name = "Pull polyester Chine ultra fast fashion (500g)", query = pullPolyesterChineUltraFastFashion, category = "Pull / Couche intermédiaire" }
    , { name = "Pull viscose Chine fast fashion (500g)", query = pullViscoseChineFastFashion, category = "Pull / Couche intermédiaire" }
    , { name = "Pull coton Pakistan marque traditionnelle (500g)", query = pullCotonPakistanMarqueTraditionnelle, category = "Pull / Couche intermédiaire" }
    , { name = "Pull coton bio France marque traditionnelle (500g)", query = pullCotonBioFranceMarqueTraditionnelle, category = "Pull / Couche intermédiaire" }
    , { name = "Pull coton Asie fast fashion (500g)", query = pullCotonAsieFastFashion, category = "Pull / Couche intermédiaire" }
    ]


exampleProductToString : Query -> String
exampleProductToString q =
    productsAndNames
        |> List.filterMap
            (\{ name, query } ->
                if q == query then
                    Just name

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault "Produit personnalisé"


exampleProductToCategory : Query -> String
exampleProductToCategory q =
    productsAndNames
        |> List.filterMap
            (\{ category, query } ->
                if q == query then
                    Just category

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault "Produit personnalisé"


exampleProducts : List Query
exampleProducts =
    productsAndNames
        |> List.sortBy .name
        |> List.map .query


defaultQuery : Query
defaultQuery =
    tShirtCotonAsie



-- 7 base products, from China


tShirtCotonAsie : Query
tShirtCotonAsie =
    { mass = Mass.kilograms 0.17
    , materials = [ { id = Material.Id "coton", share = Split.full, spinning = Nothing, country = Nothing } ]
    , product = Product.Id "tshirt"
    , countrySpinning = Just (Country.Code "CN")
    , countryFabric = Country.Code "CN"
    , countryDyeing = Country.Code "CN"
    , countryMaking = Country.Code "CN"
    , airTransportRatio = Nothing
    , makingWaste = Nothing
    , makingDeadStock = Nothing
    , makingComplexity = Nothing
    , yarnSize = Nothing
    , surfaceMass = Nothing
    , fabricProcess = Fabric.KnittingMix
    , disabledSteps = []
    , fading = Nothing
    , dyeingMedium = Nothing
    , printing = Nothing
    , business = Nothing
    , marketingDuration = Nothing
    , numberOfReferences = Nothing
    , price = Nothing
    , traceability = Nothing
    }


jupeCotonAsie : Query
jupeCotonAsie =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.3
        , product = Product.Id "jupe"
        , fabricProcess = Fabric.Weaving
    }


chemiseCotonAsie : Query
chemiseCotonAsie =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.25
        , product = Product.Id "chemise"
        , fabricProcess = Fabric.Weaving
    }


jeanCotonAsie : Query
jeanCotonAsie =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.45
        , product = Product.Id "jean"
        , fabricProcess = Fabric.Weaving
    }


manteauCotonAsie : Query
manteauCotonAsie =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.95
        , product = Product.Id "manteau"
        , fabricProcess = Fabric.Weaving
    }


pantalonCotonAsie : Query
pantalonCotonAsie =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.45
        , product = Product.Id "pantalon"
        , fabricProcess = Fabric.Weaving
    }


pullCotonAsie : Query
pullCotonAsie =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.5
        , product = Product.Id "pull"
    }



-- 7 base products from France


tShirtCotonFrance : Query
tShirtCotonFrance =
    { tShirtCotonAsie
        | countrySpinning = Just (Country.Code "FR")
        , countryFabric = Country.Code "FR"
        , countryDyeing = Country.Code "FR"
        , countryMaking = Country.Code "FR"
    }


jupeCotonFrance : Query
jupeCotonFrance =
    { tShirtCotonFrance
        | mass = Mass.kilograms 0.3
        , product = Product.Id "jupe"
        , fabricProcess = Fabric.Weaving
    }


chemiseCotonFrance : Query
chemiseCotonFrance =
    { tShirtCotonFrance
        | mass = Mass.kilograms 0.25
        , product = Product.Id "chemise"
        , fabricProcess = Fabric.Weaving
    }


jeanCotonFrance : Query
jeanCotonFrance =
    { tShirtCotonFrance
        | mass = Mass.kilograms 0.45
        , product = Product.Id "jean"
        , fabricProcess = Fabric.Weaving
    }


manteauCotonFrance : Query
manteauCotonFrance =
    { tShirtCotonFrance
        | mass = Mass.kilograms 0.95
        , product = Product.Id "manteau"
        , fabricProcess = Fabric.Weaving
    }


pantalonCotonFrance : Query
pantalonCotonFrance =
    { tShirtCotonFrance
        | mass = Mass.kilograms 0.45
        , product = Product.Id "pantalon"
        , fabricProcess = Fabric.Weaving
    }



-- Various examples


jupePolyesterAsie : Query
jupePolyesterAsie =
    { jupeCotonAsie
        | materials = [ { id = Material.Id "pet", share = Split.full, spinning = Nothing, country = Nothing } ]
    }


manteauMixAsie : Query
manteauMixAsie =
    { manteauCotonAsie
        | materials =
            [ { id = Material.Id "pa", share = Split.half, spinning = Nothing, country = Nothing }
            , { id = Material.Id "coton", share = Split.half, spinning = Nothing, country = Nothing }
            ]
    }



-- More examples


tshirtCotonBiofranceMarqueEthique : Query
tshirtCotonBiofranceMarqueEthique =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.15
        , materials =
            [ { id = Material.Id "ei-coton-organic"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "tshirt"
        , countrySpinning = Just (Country.Code "TR")
        , countryFabric = Country.Code "FR"
        , countryDyeing = Country.Code "FR"
        , countryMaking = Country.Code "FR"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.SmallBusiness
        , marketingDuration = Just (Duration.days 300)
        , numberOfReferences = Just 200
        , price = Just (Economics.priceFromFloat 30)
        , traceability = Just True
    }


tshirtSynthetiqueChineUltraFastFashion : Query
tshirtSynthetiqueChineUltraFastFashion =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.15
        , materials =
            [ { id = Material.Id "ei-pet"
              , share = Split.ninetyFive
              , spinning = Nothing
              , country = Nothing
              }
            , { id = Material.Id "pu"
              , share = Split.five
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "tshirt"
        , countrySpinning = Just (Country.Code "CN")
        , countryFabric = Country.Code "CN"
        , countryDyeing = Country.Code "CN"
        , countryMaking = Country.Code "CN"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithoutServices
        , marketingDuration = Just (Duration.days 65)
        , numberOfReferences = Just 100000
        , price = Just (Economics.priceFromFloat 10)
        , traceability = Just False
    }


tshirtCotonAsieFastFashion : Query
tshirtCotonAsieFastFashion =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.15
        , materials =
            [ { id = Material.Id "ei-coton"
              , share = Split.seventyFive
              , spinning = Nothing
              , country = Nothing
              }
            , { id = Material.Id "coton-rdp"
              , share = Split.quarter
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "tshirt"
        , countrySpinning = Just (Country.Code "IN")
        , countryFabric = Country.Code "IN"
        , countryDyeing = Country.Code "IN"
        , countryMaking = Country.Code "IN"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithoutServices
        , marketingDuration = Just (Duration.days 115)
        , numberOfReferences = Just 12000
        , price = Just (Economics.priceFromFloat 10)
        , traceability = Just False
    }


tshirtCotonPakistanMarqueTraditionnelle : Query
tshirtCotonPakistanMarqueTraditionnelle =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.15
        , materials =
            [ { id = Material.Id "ei-coton"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "tshirt"
        , countrySpinning = Just (Country.Code "PK")
        , countryFabric = Country.Code "PK"
        , countryDyeing = Country.Code "PK"
        , countryMaking = Country.Code "PK"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithServices
        , marketingDuration = Just (Duration.days 115)
        , numberOfReferences = Just 2500
        , price = Just (Economics.priceFromFloat 40)
        , traceability = Just False
    }


tshirtCotonFranceMarqueTraditionnelle : Query
tshirtCotonFranceMarqueTraditionnelle =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.15
        , materials =
            [ { id = Material.Id "ei-coton"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "tshirt"
        , countrySpinning = Just (Country.Code "TR")
        , countryFabric = Country.Code "FR"
        , countryDyeing = Country.Code "FR"
        , countryMaking = Country.Code "FR"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithServices
        , marketingDuration = Just (Duration.days 115)
        , numberOfReferences = Just 2500
        , price = Just (Economics.priceFromFloat 40)
        , traceability = Just False
    }


tshirtLinfranceMarqueEthique : Query
tshirtLinfranceMarqueEthique =
    { tShirtCotonAsie
        | mass = Mass.kilograms 0.15
        , materials =
            [ { id = Material.Id "ei-lin"
              , share = Split.full
              , spinning = Nothing
              , country = Just (Country.Code "FR")
              }
            ]
        , product = Product.Id "tshirt"
        , countrySpinning = Just (Country.Code "FR")
        , countryFabric = Country.Code "FR"
        , countryDyeing = Country.Code "FR"
        , countryMaking = Country.Code "FR"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.SmallBusiness
        , marketingDuration = Just (Duration.days 300)
        , numberOfReferences = Just 200
        , price = Just (Economics.priceFromFloat 30)
        , traceability = Just True
    }


pullLaineFranceMarqueEthique : Query
pullLaineFranceMarqueEthique =
    { pullCotonAsie
        | mass = Mass.kilograms 0.55
        , materials =
            [ { id = Material.Id "ei-laine-par-defaut"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "pull"
        , countrySpinning = Just (Country.Code "CN")
        , countryFabric = Country.Code "FR"
        , countryDyeing = Country.Code "FR"
        , countryMaking = Country.Code "FR"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.SmallBusiness
        , marketingDuration = Just (Duration.days 300)
        , numberOfReferences = Just 200
        , price = Just (Economics.priceFromFloat 95)
        , traceability = Just True
    }


pullLainePaysanneFranceMarqueEthique : Query
pullLainePaysanneFranceMarqueEthique =
    { pullCotonAsie
        | mass = Mass.kilograms 0.55
        , materials =
            [ { id = Material.Id "ei-laine-nouvelle-filiere"
              , share = Split.full
              , spinning = Nothing
              , country = Just (Country.Code "FR")
              }
            ]
        , product = Product.Id "pull"
        , countrySpinning = Just (Country.Code "FR")
        , countryFabric = Country.Code "FR"
        , countryDyeing = Country.Code "FR"
        , countryMaking = Country.Code "FR"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.SmallBusiness
        , marketingDuration = Just (Duration.days 300)
        , numberOfReferences = Just 200
        , price = Just (Economics.priceFromFloat 95)
        , traceability = Just True
    }


pullPolyesterChineUltraFastFashion : Query
pullPolyesterChineUltraFastFashion =
    { pullCotonAsie
        | mass = Mass.kilograms 0.5
        , materials =
            [ { id = Material.Id "ei-pet"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "pull"
        , countrySpinning = Just (Country.Code "CN")
        , countryFabric = Country.Code "CN"
        , countryDyeing = Country.Code "CN"
        , countryMaking = Country.Code "CN"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithoutServices
        , marketingDuration = Just (Duration.days 65)
        , numberOfReferences = Just 100000
        , price = Just (Economics.priceFromFloat 20)
        , traceability = Just False
    }


pullViscoseChineFastFashion : Query
pullViscoseChineFastFashion =
    { pullCotonAsie
        | mass = Mass.kilograms 0.5
        , materials =
            [ { id = Material.Id "ei-viscose"
              , share = Split.seventy
              , spinning = Nothing
              , country = Nothing
              }
            , { id = Material.Id "pa"
              , share = Split.thirty
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "pull"
        , countrySpinning = Just (Country.Code "CN")
        , countryFabric = Country.Code "CN"
        , countryDyeing = Country.Code "CN"
        , countryMaking = Country.Code "CN"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithoutServices
        , marketingDuration = Just (Duration.days 115)
        , numberOfReferences = Just 12000
        , price = Just (Economics.priceFromFloat 20)
        , traceability = Just False
    }


pullCotonPakistanMarqueTraditionnelle : Query
pullCotonPakistanMarqueTraditionnelle =
    { pullCotonAsie
        | mass = Mass.kilograms 0.5
        , materials =
            [ { id = Material.Id "ei-coton"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "pull"
        , countrySpinning = Just (Country.Code "PK")
        , countryFabric = Country.Code "PK"
        , countryDyeing = Country.Code "PK"
        , countryMaking = Country.Code "PK"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithServices
        , marketingDuration = Just (Duration.days 115)
        , numberOfReferences = Just 2500
        , price = Just (Economics.priceFromFloat 70)
        , traceability = Just False
    }


pullCotonBioFranceMarqueTraditionnelle : Query
pullCotonBioFranceMarqueTraditionnelle =
    { pullCotonAsie
        | mass = Mass.kilograms 0.5
        , materials =
            [ { id = Material.Id "ei-coton-organic"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "pull"
        , countrySpinning = Just (Country.Code "IN")
        , countryFabric = Country.Code "FR"
        , countryDyeing = Country.Code "FR"
        , countryMaking = Country.Code "FR"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithServices
        , marketingDuration = Just (Duration.days 115)
        , numberOfReferences = Just 2500
        , price = Just (Economics.priceFromFloat 70)
        , traceability = Just True
    }


pullCotonAsieFastFashion : Query
pullCotonAsieFastFashion =
    { pullCotonAsie
        | mass = Mass.kilograms 0.5
        , materials =
            [ { id = Material.Id "ei-coton"
              , share = Split.full
              , spinning = Nothing
              , country = Nothing
              }
            ]
        , product = Product.Id "pull"
        , countrySpinning = Just (Country.Code "CN")
        , countryFabric = Country.Code "CN"
        , countryDyeing = Country.Code "CN"
        , countryMaking = Country.Code "CN"
        , fabricProcess = Fabric.KnittingMix
        , business = Just Economics.LargeBusinessWithoutServices
        , marketingDuration = Just (Duration.days 115)
        , numberOfReferences = Just 12000
        , price = Just (Economics.priceFromFloat 20)
        , traceability = Just False
    }
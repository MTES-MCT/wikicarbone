module Data.Textile.Inputs exposing
    ( Inputs
    , MaterialInput
    , MaterialQuery
    , Query
    , addMaterial
    , b64decode
    , b64encode
    , countryList
    , defaultQuery
    , fromQuery
    , getMainMaterial
    , inputsCodec
    , jupeCircuitAsie
    , parseBase64Query
    , presets
    , queryCodec
    , removeMaterial
    , tShirtCotonAsie
    , tShirtCotonFrance
    , toQuery
    , toString
    , toggleStep
    , updateMaterial
    , updateMaterialShare
    , updateProduct
    , updateStepCountry
    )

import Base64
import Codec exposing (Codec)
import Data.Country as Country exposing (Country)
import Data.Textile.Db exposing (Db)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Process exposing (Process)
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Step.Label as Label exposing (Label)
import Data.Unit as Unit
import Json.Decode as Decode
import List.Extra as LE
import Mass exposing (Mass)
import Result.Extra as RE
import Url.Parser as Parser exposing (Parser)
import Views.Format as Format


type alias MaterialInput =
    { material : Material
    , share : Unit.Ratio
    }


type alias Inputs =
    { mass : Mass
    , materials : List MaterialInput
    , product : Product
    , countryMaterial : Country
    , countrySpinning : Country
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
    , makingWaste : Maybe Unit.Ratio
    , picking : Maybe Unit.PickPerMeter
    , surfaceMass : Maybe Unit.SurfaceMass
    , disabledSteps : List Label
    , disabledFading : Maybe Bool
    }


type alias MaterialQuery =
    { id : Material.Id
    , share : Unit.Ratio
    }


type alias Query =
    { mass : Mass
    , materials : List MaterialQuery
    , product : Product.Id
    , countrySpinning : Maybe Country.Code
    , countryFabric : Country.Code
    , countryDyeing : Country.Code
    , countryMaking : Country.Code
    , dyeingWeighting : Maybe Unit.Ratio
    , airTransportRatio : Maybe Unit.Ratio
    , quality : Maybe Unit.Quality
    , reparability : Maybe Unit.Reparability
    , makingWaste : Maybe Unit.Ratio
    , picking : Maybe Unit.PickPerMeter
    , surfaceMass : Maybe Unit.SurfaceMass
    , disabledSteps : List Label
    , disabledFading : Maybe Bool
    }


toMaterialInputs : List Material -> List MaterialQuery -> Result String (List MaterialInput)
toMaterialInputs materials =
    List.map
        (\{ id, share } ->
            Material.findById id materials
                |> Result.map
                    (\material_ ->
                        { material = material_
                        , share = share
                        }
                    )
        )
        >> RE.combine


toMaterialQuery : List MaterialInput -> List MaterialQuery
toMaterialQuery =
    List.map (\{ material, share } -> { id = material.id, share = share })


getMainMaterial : List MaterialInput -> Result String Material
getMainMaterial =
    List.sortBy (.share >> Unit.ratioToFloat)
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
fromQuery db query =
    let
        materials =
            query.materials
                |> toMaterialInputs db.materials

        franceResult =
            Country.findByCode (Country.Code "FR") db.countries

        mainMaterialCountry =
            materials |> Result.andThen (getMainMaterialCountry db.countries)
    in
    Ok Inputs
        |> RE.andMap (Ok query.mass)
        |> RE.andMap materials
        |> RE.andMap (db.products |> Product.findById query.product)
        -- Material country is constrained to be the first material's default country
        |> RE.andMap mainMaterialCountry
        -- Spinning country is either provided by query or fallbacks to material's default
        -- country, making the parameter optional
        |> RE.andMap
            (case query.countrySpinning of
                Just spinningCountryCode ->
                    Country.findByCode spinningCountryCode db.countries

                Nothing ->
                    mainMaterialCountry
            )
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
        |> RE.andMap (Ok query.makingWaste)
        |> RE.andMap (Ok query.picking)
        |> RE.andMap (Ok query.surfaceMass)
        |> RE.andMap (Ok query.disabledSteps)
        |> RE.andMap (Ok query.disabledFading)


toQuery : Inputs -> Query
toQuery inputs =
    { mass = inputs.mass
    , materials = toMaterialQuery inputs.materials
    , product = inputs.product.id
    , countrySpinning =
        if
            -- Discard custom spinning country if same as material default country
            (getMainMaterial inputs.materials |> Result.map .defaultCountry)
                == Ok inputs.countrySpinning.code
        then
            Nothing

        else
            Just inputs.countrySpinning.code
    , countryFabric = inputs.countryFabric.code
    , countryDyeing = inputs.countryDyeing.code
    , countryMaking = inputs.countryMaking.code
    , dyeingWeighting = inputs.dyeingWeighting
    , airTransportRatio = inputs.airTransportRatio
    , quality = inputs.quality
    , reparability = inputs.reparability
    , makingWaste = inputs.makingWaste
    , picking = inputs.picking
    , surfaceMass = inputs.surfaceMass
    , disabledSteps = inputs.disabledSteps
    , disabledFading = inputs.disabledFading
    }


toString : Inputs -> String
toString inputs =
    [ [ inputs.product.name ++ " de " ++ Format.kgToString inputs.mass ]
    , [ materialsToString inputs.materials ]
    , [ "matière", inputs.countryMaterial.name ]
    , [ "filature", inputs.countrySpinning.name ]
    , case inputs.product.fabric of
        Product.Knitted _ ->
            [ "tricotage", inputs.countryFabric.name ]

        Product.Weaved _ _ _ ->
            [ "tissage", inputs.countryFabric.name ++ weavingOptionsToString inputs.picking inputs.surfaceMass ]
    , [ "teinture", inputs.countryDyeing.name ++ dyeingOptionsToString inputs.dyeingWeighting ]
    , [ "confection", inputs.countryMaking.name ++ makingOptionsToString inputs ]
    , [ "distribution", inputs.countryDistribution.name ]
    , [ "utilisation", inputs.countryUse.name ++ useOptionsToString inputs.quality inputs.reparability ]
    , [ "fin de vie", inputs.countryEndOfLife.name ]
    ]
        |> List.map (String.join "\u{00A0}: ")
        |> String.join ", "


materialsToString : List MaterialInput -> String
materialsToString materials =
    materials
        |> List.filter (\{ share } -> Unit.ratioToFloat share > 0)
        |> List.map
            (\{ material, share } ->
                Format.formatFloat 0 (Unit.ratioToFloat share * 100)
                    ++ "% "
                    ++ material.shortName
            )
        |> String.join ", "


weavingOptionsToString : Maybe Unit.PickPerMeter -> Maybe Unit.SurfaceMass -> String
weavingOptionsToString _ _ =
    -- FIXME: migrate Step.*ToString fns to avoid circular import so we can reuse them here?
    ""


dyeingOptionsToString : Maybe Unit.Ratio -> String
dyeingOptionsToString maybeRatio =
    case maybeRatio of
        Nothing ->
            " (procédé représentatif)"

        Just ratio ->
            if Unit.ratioToFloat ratio == 0 then
                " (procédé représentatif)"

            else
                ratio
                    |> Format.ratioToPercentString
                    |> (\percent -> " (procédé " ++ percent ++ " majorant)")


makingOptionsToString : Inputs -> String
makingOptionsToString { product, makingWaste, airTransportRatio, disabledFading } =
    [ makingWaste
        |> Maybe.map (Format.ratioToPercentString >> (\s -> s ++ " de perte"))
    , airTransportRatio
        |> Maybe.andThen
            (\ratio ->
                if Unit.ratioToFloat ratio == 0 then
                    Nothing

                else
                    Just (Format.ratioToPercentString ratio ++ " de transport aérien")
            )
    , if product.making.fadable && disabledFading == Just True then
        Just "non-délavé"

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


useOptionsToString : Maybe Unit.Quality -> Maybe Unit.Reparability -> String
useOptionsToString maybeQuality maybeReparability =
    let
        ( quality, reparability ) =
            ( maybeQuality
                |> Maybe.map (Unit.qualityToFloat >> String.fromFloat)
                |> Maybe.withDefault "standard"
            , maybeReparability
                |> Maybe.map (Unit.reparabilityToFloat >> String.fromFloat)
                |> Maybe.withDefault "standard"
            )
    in
    if quality /= "standard" || reparability /= "standard" then
        " (qualité " ++ quality ++ ", réparabilité " ++ reparability ++ ")"

    else
        ""


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

        Label.Dyeing ->
            { query
                | countryDyeing = code
                , dyeingWeighting =
                    if query.countryDyeing /= code then
                        -- reset custom value as we just switched country, which dyeing weighting is totally different
                        Nothing

                    else
                        query.dyeingWeighting
            }

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
                    query.materials ++ [ { id = id, share = Unit.ratio 0 } ]
            }

        Nothing ->
            query


updateMaterialAt : Int -> (MaterialQuery -> MaterialQuery) -> Query -> Query
updateMaterialAt index update query =
    { query | materials = query.materials |> LE.updateAt index update }


updateMaterial : Int -> Material -> Query -> Query
updateMaterial index { id } =
    -- Note: The first material country is always extracted and applied in `fromQuery`.
    updateMaterialAt index (\({ share } as m) -> { m | id = id, share = share })


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
    if product.id /= query.product then
        -- Product has changed, reset a bunch of related query params
        { query
            | product = product.id
            , mass = product.mass
            , quality = Nothing
            , reparability = Nothing
            , makingWaste = Nothing
            , picking = Nothing
            , surfaceMass = Nothing
            , disabledFading = Nothing
        }

    else
        query


defaultQuery : Query
defaultQuery =
    tShirtCotonIndia


tShirtCotonFrance : Query
tShirtCotonFrance =
    -- T-shirt circuit France
    { mass = Mass.kilograms 0.17
    , materials = [ { id = Material.Id "coton", share = Unit.ratio 1 } ]
    , product = Product.Id "tshirt"
    , countrySpinning = Nothing
    , countryFabric = Country.Code "FR"
    , countryDyeing = Country.Code "FR"
    , countryMaking = Country.Code "FR"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    , makingWaste = Nothing
    , picking = Nothing
    , surfaceMass = Nothing
    , disabledSteps = []
    , disabledFading = Nothing
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
    , materials = [ { id = Material.Id "acrylique", share = Unit.ratio 1 } ]
    , product = Product.Id "jupe"
    , countrySpinning = Nothing
    , countryFabric = Country.Code "CN"
    , countryDyeing = Country.Code "CN"
    , countryMaking = Country.Code "CN"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    , makingWaste = Nothing
    , picking = Nothing
    , surfaceMass = Nothing
    , disabledSteps = []
    , disabledFading = Nothing
    }


manteauCircuitEurope : Query
manteauCircuitEurope =
    -- Manteau circuit Europe
    { mass = Mass.kilograms 0.95
    , materials = [ { id = Material.Id "cachemire", share = Unit.ratio 1 } ]
    , product = Product.Id "manteau"
    , countrySpinning = Nothing
    , countryFabric = Country.Code "TR"
    , countryDyeing = Country.Code "TN"
    , countryMaking = Country.Code "ES"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    , makingWaste = Nothing
    , picking = Nothing
    , surfaceMass = Nothing
    , disabledSteps = []
    , disabledFading = Nothing
    }


pantalonCircuitEurope : Query
pantalonCircuitEurope =
    -- Pantalon circuit Europe
    { mass = Mass.kilograms 0.45
    , materials = [ { id = Material.Id "lin-filasse", share = Unit.ratio 1 } ]
    , product = Product.Id "pantalon"
    , countrySpinning = Nothing
    , countryFabric = Country.Code "TR"
    , countryDyeing = Country.Code "TR"
    , countryMaking = Country.Code "TR"
    , dyeingWeighting = Nothing
    , airTransportRatio = Nothing
    , quality = Nothing
    , reparability = Nothing
    , makingWaste = Nothing
    , picking = Nothing
    , surfaceMass = Nothing
    , disabledSteps = []
    , disabledFading = Nothing
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


materialInputCodec : List Process -> Codec MaterialInput
materialInputCodec processes =
    Codec.object MaterialInput
        |> Codec.field "material" .material (Material.codec processes)
        |> Codec.field "share" .share Unit.ratioCodec
        |> Codec.buildObject


inputsCodec : List Process -> Codec Inputs
inputsCodec processes =
    Codec.object Inputs
        |> Codec.field "mass" .mass (Codec.map Mass.kilograms Mass.inKilograms Codec.float)
        |> Codec.field "materials" .materials (Codec.list (materialInputCodec processes))
        |> Codec.field "product" .product (Product.codec processes)
        |> Codec.field "countryMaterial" .countryMaterial (Country.codec processes)
        |> Codec.field "countrySpinning" .countrySpinning (Country.codec processes)
        |> Codec.field "countryFabric" .countryFabric (Country.codec processes)
        |> Codec.field "countryDyeing" .countryDyeing (Country.codec processes)
        |> Codec.field "countryMaking" .countryMaking (Country.codec processes)
        |> Codec.field "countryDistribution" .countryDistribution (Country.codec processes)
        |> Codec.field "countryUse" .countryUse (Country.codec processes)
        |> Codec.field "countryEndOfLife" .countryEndOfLife (Country.codec processes)
        |> Codec.maybeField "dyeingWeighting" .dyeingWeighting Unit.ratioCodec
        |> Codec.maybeField "airTransportRatio" .airTransportRatio Unit.ratioCodec
        |> Codec.maybeField "quality" .quality Unit.qualityCodec
        |> Codec.maybeField "reparability" .reparability Unit.reparabilityCodec
        |> Codec.maybeField "makingWaste" .makingWaste Unit.ratioCodec
        |> Codec.maybeField "picking" .picking Unit.pickPerMeterCodec
        |> Codec.maybeField "surfaceMass" .surfaceMass Unit.surfaceMassCodec
        -- FIXME: make this an optional JSON key with a default of []
        |> Codec.field "disabledSteps" .disabledSteps (Codec.list Label.codeCodec)
        |> Codec.maybeField "disabledFading" .disabledFading Codec.bool
        |> Codec.buildObject


queryCodec : Codec Query
queryCodec =
    Codec.object Query
        |> Codec.field "mass" .mass (Codec.map Mass.kilograms Mass.inKilograms Codec.float)
        |> Codec.field "materials" .materials (Codec.list materialQueryCodec)
        |> Codec.field "product" .product Product.idCodec
        |> Codec.maybeField "countrySpinning" .countrySpinning Country.codeCodec
        |> Codec.field "countryFabric" .countryFabric Country.codeCodec
        |> Codec.field "countryDyeing" .countryDyeing Country.codeCodec
        |> Codec.field "countryMaking" .countryMaking Country.codeCodec
        |> Codec.maybeField "dyeingWeighting" .dyeingWeighting Unit.ratioCodec
        |> Codec.maybeField "airTransportRatio" .airTransportRatio Unit.ratioCodec
        |> Codec.maybeField "quality" .quality Unit.qualityCodec
        |> Codec.maybeField "reparability" .reparability Unit.reparabilityCodec
        |> Codec.maybeField "makingWaste" .makingWaste Unit.ratioCodec
        |> Codec.maybeField "picking" .picking Unit.pickPerMeterCodec
        |> Codec.maybeField "surfaceMass" .surfaceMass Unit.surfaceMassCodec
        -- FIXME: make this an optional JSON key with a default of []
        |> Codec.field "disabledSteps" .disabledSteps (Codec.list Label.codeCodec)
        |> Codec.maybeField "disabledFading" .disabledFading Codec.bool
        |> Codec.buildObject


materialQueryCodec : Codec MaterialQuery
materialQueryCodec =
    Codec.object MaterialQuery
        |> Codec.field "id" .id Material.idCodec
        |> Codec.field "share" .share Unit.ratioCodec
        |> Codec.buildObject


b64decode : String -> Result String Query
b64decode =
    Base64.decode
        >> Result.andThen
            (Codec.decodeString queryCodec
                >> Result.mapError Decode.errorToString
            )


b64encode : Query -> String
b64encode =
    Codec.encodeToString 0 queryCodec
        >> Base64.encode



-- Parser


parseBase64Query : Parser (Maybe Query -> a) a
parseBase64Query =
    Parser.custom "QUERY" <|
        b64decode
            >> Result.toMaybe
            >> Just

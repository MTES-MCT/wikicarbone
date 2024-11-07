module Server.Query exposing
    ( Errors
    , encodeErrors
    , parseTextileQuery
    )

import Data.Country as Country exposing (Country)
import Data.Env as Env
import Data.Scope as Scope exposing (Scope)
import Data.Split as Split exposing (Split)
import Data.Textile.DyeingMedium as DyeingMedium exposing (DyeingMedium)
import Data.Textile.Economics as Economics
import Data.Textile.Fabric as Fabric exposing (Fabric)
import Data.Textile.MakingComplexity as MakingComplexity exposing (MakingComplexity)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Material.Spinning as Spinning exposing (Spinning)
import Data.Textile.Printing as Printing exposing (Printing)
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Query as TextileQuery
import Data.Textile.Step.Label as Label exposing (Label)
import Data.Unit as Unit
import Dict exposing (Dict)
import Json.Encode as Encode
import Mass exposing (Mass)
import Quantity
import Regex
import Result.Extra as RE
import Static.Db exposing (Db)
import Url.Parser.Query as Query exposing (Parser)



-- Query String


type alias FieldName =
    String


type alias ErrorMessage =
    String


type alias Errors =
    Dict FieldName ErrorMessage


type alias ParseResult a =
    Result ( FieldName, String ) a


succeed : a -> Parser a
succeed =
    -- Kind of a hack: we don't have access to the Parser constructor, so
    -- we use Query.custom to wrap our thing in a Parser
    always >> Query.custom ""


countryParser : List Country -> Scope -> String -> Result String (Maybe Country.Code)
countryParser countries scope countryStr =
    if countryStr == "" then
        Ok Nothing

    else
        countries
            |> validateCountry countryStr scope
            |> Result.map Just


validateBool : String -> Result String Bool
validateBool str =
    case str of
        "true" ->
            Ok True

        "false" ->
            Ok False

        _ ->
            Err "La valeur ne peut être que true ou false."


validateCountry : String -> Scope -> List Country -> Result String Country.Code
validateCountry countryCode scope =
    Country.findByCode (Country.codeFromString countryCode)
        >> Result.andThen
            (\{ code, scopes } ->
                if List.member scope scopes then
                    Ok code

                else
                    "Le code pays "
                        ++ countryCode
                        ++ " n'est pas utilisable dans un contexte "
                        ++ Scope.toLabel scope
                        ++ "."
                        |> Err
            )


validatePhysicalDurability : String -> Result String Unit.PhysicalDurability
validatePhysicalDurability string =
    string
        |> String.toFloat
        |> Result.fromMaybe ("Durabilité invalide\u{202F}: " ++ string)
        |> Result.andThen
            (\durability ->
                let
                    minFloatDurability =
                        Unit.minDurability Unit.PhysicalDurability |> Unit.physicalDurabilityToFloat

                    maxFloatDurability =
                        Unit.maxDurability Unit.PhysicalDurability |> Unit.physicalDurabilityToFloat
                in
                if durability < minFloatDurability || durability > maxFloatDurability then
                    Err
                        ("La durabilité doit être comprise entre "
                            ++ String.fromFloat minFloatDurability
                            ++ " et "
                            ++ String.fromFloat maxFloatDurability
                            ++ "."
                        )

                else
                    Ok (Unit.PhysicalDurability durability)
            )


maybePriceParser : String -> Parser (ParseResult (Maybe Economics.Price))
maybePriceParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (String.toFloat
                    >> Maybe.map Economics.priceFromFloat
                    >> Result.fromMaybe "Ce prix est invalide"
                    >> Result.map Just
                    >> Result.mapError (\err -> ( key, err ))
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybePhysicalDurabilityParser : String -> Parser (ParseResult (Maybe Unit.PhysicalDurability))
maybePhysicalDurabilityParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (\durability ->
                    validatePhysicalDurability durability
                        |> Result.map Just
                        |> Result.mapError (\err -> ( key, err ))
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeBusiness : String -> Parser (ParseResult (Maybe Economics.Business))
maybeBusiness key =
    Query.string key
        |> Query.map
            (Maybe.map
                (Economics.businessFromString
                    >> Result.map Just
                    >> Result.mapError (\err -> ( key, err ))
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeIntParser : String -> Parser (ParseResult (Maybe Int))
maybeIntParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (String.toInt
                    >> Result.fromMaybe "Nombre entier invalide"
                    >> Result.map Just
                    >> Result.mapError (\err -> ( key, err ))
                )
                >> Maybe.withDefault (Ok Nothing)
            )


parseTextileQuery : Db -> Parser (Result Errors TextileQuery.Query)
parseTextileQuery { countries, textile } =
    succeed (Ok TextileQuery.Query)
        |> apply (maybeSplitParser "airTransportRatio")
        |> apply (maybeBusiness "business")
        |> apply (maybeTextileCountryParser "countryDyeing" countries)
        |> apply (maybeTextileCountryParser "countryFabric" countries)
        |> apply (maybeTextileCountryParser "countryMaking" countries)
        |> apply (maybeTextileCountryParser "countrySpinning" countries)
        |> apply (maybeDisabledStepsParser "disabledSteps")
        |> apply (maybeDyeingMedium "dyeingMedium")
        |> apply (maybeFabricParser "fabricProcess")
        |> apply (maybeBoolParser "fading")
        |> apply (maybeMakingComplexityParser "makingComplexity")
        |> apply (maybeMakingDeadStockParser "makingDeadStock")
        |> apply (maybeMakingWasteParser "makingWaste")
        |> apply (massParser Mass.kilograms "mass")
        |> apply (materialListParser "materials" textile.materials countries)
        |> apply (maybeIntParser "numberOfReferences")
        |> apply (maybePhysicalDurabilityParser "physicalDurability")
        |> apply (maybePriceParser "price")
        |> apply (maybePrinting "printing")
        |> apply (productParser "product" textile.products)
        |> apply (maybeSurfaceMassParser "surfaceMass")
        |> apply (maybeBoolParser "traceability")
        |> apply (boolParser { default = False } "upcycled")
        |> apply (maybeYarnSizeParser "yarnSize")


toErrors : ParseResult a -> Result Errors a
toErrors =
    Result.mapError
        (\( key, errorMessage ) ->
            Dict.singleton key errorMessage
        )


builder :
    Result (Dict comparable v) a
    -> Result (Dict comparable v) (a -> b)
    -> Result (Dict comparable v) b
builder result accumulator =
    case ( result, accumulator ) of
        ( Err a, Err b ) ->
            -- Merge the error dicts
            Err <| Dict.union a b

        ( Ok _, Err b ) ->
            -- No error in "result", but the accumulator is errored: return the errored accumulator
            Err b

        ( valueOrError, Ok fn ) ->
            Result.map fn valueOrError


apply : Parser (ParseResult a) -> Parser (Result Errors (a -> b)) -> Parser (Result Errors b)
apply argParser funcParser =
    -- Adapted from https://package.elm-lang.org/packages/elm/url/latest/Url-Parser-Query#map8
    -- with the full list of errors returned, instead of just the first one encountered.
    Query.map2
        builder
        (Query.map toErrors argParser)
        funcParser


floatParser : String -> Parser (Maybe Float)
floatParser key =
    Query.custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    String.toFloat str

                _ ->
                    Nothing


massParser : (Float -> Mass) -> String -> Parser (ParseResult Mass)
massParser toMass key =
    floatParser key
        |> Query.map (Result.fromMaybe ( key, "La masse est manquante." ))
        |> Query.map
            (Result.andThen
                (\float ->
                    if float < 0 then
                        Err ( key, "La masse doit être supérieure ou égale à zéro." )

                    else
                        Ok <| toMass float
                )
            )


productParser : String -> List Product -> Parser (ParseResult Product.Id)
productParser key products =
    Query.string key
        |> Query.map (Result.fromMaybe ( key, "Identifiant du type de produit manquant." ))
        |> Query.map
            (Result.andThen
                (\id ->
                    products
                        |> Product.findById (Product.Id id)
                        |> Result.map .id
                        |> Result.mapError (\err -> ( key, err ))
                )
            )


materialListParser : String -> List Material -> List Country -> Parser (ParseResult (List TextileQuery.MaterialQuery))
materialListParser key materials countries =
    Query.custom (key ++ "[]")
        (List.map (parseMaterial_ materials countries)
            >> RE.combine
            >> Result.andThen TextileQuery.validateMaterials
            >> Result.mapError (\err -> ( key, err ))
        )


parseMaterial_ : List Material -> List Country -> String -> Result String TextileQuery.MaterialQuery
parseMaterial_ materials countries string =
    case String.split ";" string of
        [ id, share, spinningString, countryCode ] ->
            materials
                |> Material.findById (Material.Id id)
                |> Result.andThen
                    (\material ->
                        Ok TextileQuery.MaterialQuery
                            |> RE.andMap (countryParser countries Scope.Textile countryCode)
                            |> RE.andMap (Ok material.id)
                            |> RE.andMap (parseSplit share)
                            |> RE.andMap (parseSpinning material spinningString)
                    )

        [ id, share, spinningString ] ->
            materials
                |> Material.findById (Material.Id id)
                |> Result.andThen
                    (\material ->
                        Ok TextileQuery.MaterialQuery
                            |> RE.andMap (Ok Nothing)
                            |> RE.andMap (Ok material.id)
                            |> RE.andMap (parseSplit share)
                            |> RE.andMap (parseSpinning material spinningString)
                    )

        [ id, share ] ->
            Ok TextileQuery.MaterialQuery
                |> RE.andMap (Ok Nothing)
                |> RE.andMap (parseMaterialId_ materials id)
                |> RE.andMap (parseSplit share)
                |> RE.andMap (Ok Nothing)

        [ "" ] ->
            Err <| "Format de matière vide."

        _ ->
            Err <| "Format de matière invalide : " ++ string ++ "."


parseMaterialId_ : List Material -> String -> Result String Material.Id
parseMaterialId_ materials string =
    materials
        |> Material.findById (Material.Id string)
        |> Result.map .id


parseSplit : String -> Result String Split
parseSplit string =
    string
        |> String.toFloat
        |> Result.fromMaybe ("Ratio invalide : " ++ string)
        |> Result.andThen Split.fromFloat


parseSpinning : Material -> String -> Result String (Maybe Spinning)
parseSpinning material spinningString =
    if spinningString == "" then
        Ok Nothing

    else
        let
            availableSpinningProcesses =
                Spinning.getAvailableProcesses material.origin
        in
        Spinning.fromString spinningString
            |> Result.andThen
                (\spinning ->
                    if List.member spinning availableSpinningProcesses then
                        Ok (Just spinning)

                    else
                        Err <| "Un procédé de filature/filage doit être choisi parmi (" ++ (availableSpinningProcesses |> List.map Spinning.toString |> String.join "|") ++ ") (ici: " ++ spinningString ++ ")"
                )
            |> Result.mapError
                (always <|
                    "Un procédé de filature/filage doit être choisi parmi ("
                        ++ (availableSpinningProcesses |> List.map Spinning.toString |> String.join "|")
                        ++ ") (ici: "
                        ++ spinningString
                        ++ ")"
                )


maybeTextileCountryParser : String -> List Country -> Parser (ParseResult (Maybe Country.Code))
maybeTextileCountryParser key countries =
    Query.string key
        |> Query.map
            (Maybe.map
                (\code ->
                    validateCountry code Scope.Textile countries
                        |> Result.map Just
                        |> Result.mapError (\err -> ( key, err ))
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeDyeingMedium : String -> Parser (ParseResult (Maybe DyeingMedium))
maybeDyeingMedium key =
    Query.string key
        |> Query.map
            (Maybe.map
                (\str ->
                    case DyeingMedium.fromString str of
                        Err err ->
                            Err ( key, err )

                        Ok dyeingMedium ->
                            Ok (Just dyeingMedium)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybePrinting : String -> Parser (ParseResult (Maybe Printing))
maybePrinting key =
    Query.string key
        |> Query.map
            (Maybe.map
                (\str ->
                    case Printing.fromStringParam str of
                        Err err ->
                            Err ( key, err )

                        Ok printing ->
                            Ok (Just printing)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeSplitParser : String -> Parser (ParseResult (Maybe Split))
maybeSplitParser key =
    floatParser key
        |> Query.map
            (Maybe.map
                (\float ->
                    if float < 0 || float > 1 then
                        Err ( key, "Un ratio doit être compris entre 0 et 1 inclus." )

                    else
                        Ok (Result.toMaybe (Split.fromFloat float))
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeMakingComplexityParser : String -> Parser (ParseResult (Maybe MakingComplexity))
maybeMakingComplexityParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (\str ->
                    case MakingComplexity.fromString str of
                        Err err ->
                            Err ( key, err )

                        Ok printing ->
                            Ok (Just printing)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeMakingWasteParser : String -> Parser (ParseResult (Maybe Split))
maybeMakingWasteParser key =
    floatParser key
        |> Query.map
            (Maybe.map
                (\float ->
                    if float < Split.toFloat Env.minMakingWasteRatio || float > Split.toFloat Env.maxMakingWasteRatio then
                        Err
                            ( key
                            , "Le taux de perte en confection doit être compris entre "
                                ++ Split.toFloatString Env.minMakingWasteRatio
                                ++ " et "
                                ++ Split.toFloatString Env.maxMakingWasteRatio
                                ++ "."
                            )

                    else
                        Ok (Split.fromFloat float |> Result.toMaybe)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeMakingDeadStockParser : String -> Parser (ParseResult (Maybe Split))
maybeMakingDeadStockParser key =
    floatParser key
        |> Query.map
            (Maybe.map
                (\float ->
                    if float < Split.toFloat Env.minMakingDeadStockRatio || float > Split.toFloat Env.maxMakingDeadStockRatio then
                        Err
                            ( key
                            , "Le taux de stocks dormants en confection doit être compris entre "
                                ++ Split.toFloatString Env.minMakingDeadStockRatio
                                ++ " et "
                                ++ Split.toFloatString Env.maxMakingDeadStockRatio
                                ++ "."
                            )

                    else
                        Ok (Split.fromFloat float |> Result.toMaybe)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


parseYarnSize : String -> Maybe Unit.YarnSize
parseYarnSize str =
    let
        withUnitRegex =
            -- Match either an int or a int and a unit `Nm` or `Dtex`
            Regex.fromString "(\\d+)(Nm|Dtex)"
                |> Maybe.withDefault Regex.never

        subMatches =
            -- If it matches, returns [ <the value>, <the unit> ]
            str
                |> Regex.find withUnitRegex
                |> List.map .submatches
    in
    case String.toFloat str of
        Just float ->
            Just <| Unit.yarnSizeKilometersPerKg float

        Nothing ->
            case subMatches of
                [ [ Just floatStr, Just "Nm" ] ] ->
                    String.toFloat floatStr
                        |> Maybe.map Unit.yarnSizeKilometersPerKg

                [ [ Just floatStr, Just "Dtex" ] ] ->
                    String.toFloat floatStr
                        |> Maybe.map Unit.yarnSizeGramsPer10km

                _ ->
                    Nothing


maybeYarnSizeParser : String -> Parser (ParseResult (Maybe Unit.YarnSize))
maybeYarnSizeParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (\str ->
                    case parseYarnSize str of
                        Just yarnSize ->
                            if (yarnSize |> Quantity.lessThan Unit.minYarnSize) || (yarnSize |> Quantity.greaterThan Unit.maxYarnSize) then
                                let
                                    format fn =
                                        fn >> floor >> String.fromInt
                                in
                                Err
                                    ( key
                                    , "Le titrage (yarnSize) doit être compris entre "
                                        ++ (Unit.minYarnSize |> format Unit.yarnSizeInKilometers)
                                        ++ " et "
                                        ++ (Unit.maxYarnSize |> format Unit.yarnSizeInKilometers)
                                        ++ " Nm (entre "
                                        -- The following two are reversed in Dtex because the unit is "reversed"
                                        ++ (Unit.maxYarnSize |> format Unit.yarnSizeInGrams)
                                        ++ " et "
                                        ++ (Unit.minYarnSize |> format Unit.yarnSizeInGrams)
                                        ++ " Dtex)"
                                    )

                            else
                                Ok (Just yarnSize)

                        Nothing ->
                            Err
                                ( key
                                , "Le format ne correspond pas au titrage (yarnSize) attendu : soit un entier simple (ie : `40`), ou avec l'unité `Nm` (ie : `40Nm`) ou `Dtex` (ie : `250Dtex`)"
                                )
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeSurfaceMassParser : String -> Parser (ParseResult (Maybe Unit.SurfaceMass))
maybeSurfaceMassParser key =
    Query.int key
        |> Query.map
            (Maybe.map
                (\int ->
                    let
                        surfaceMass =
                            Unit.gramsPerSquareMeter int
                    in
                    if
                        (surfaceMass |> Quantity.lessThan Unit.minSurfaceMass)
                            || (surfaceMass |> Quantity.greaterThan Unit.maxSurfaceMass)
                    then
                        Err
                            ( key
                            , "Le grammage (surfaceMass) doit être compris entre "
                                ++ String.fromInt (Unit.surfaceMassInGramsPerSquareMeters Unit.minSurfaceMass)
                                ++ " et "
                                ++ String.fromInt (Unit.surfaceMassInGramsPerSquareMeters Unit.maxSurfaceMass)
                                ++ " g/m²."
                            )

                    else
                        Ok (Just surfaceMass)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeFabricParser : String -> Parser (ParseResult (Maybe Fabric))
maybeFabricParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (\str ->
                    case Fabric.fromString str of
                        Err err ->
                            Err ( key, err )

                        Ok fabric ->
                            Ok (Just fabric)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


maybeDisabledStepsParser : String -> Parser (ParseResult (List Label))
maybeDisabledStepsParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (\str ->
                    str
                        |> String.split ","
                        |> List.map Label.fromCodeString
                        |> RE.combine
                        |> Result.mapError
                            (\err ->
                                ( key
                                , "Impossible d'interpréter la liste des étapes désactivées; " ++ err
                                )
                            )
                )
                >> Maybe.withDefault (Ok [])
            )


maybeBoolParser : String -> Parser (ParseResult (Maybe Bool))
maybeBoolParser key =
    Query.string key
        |> Query.map
            (Maybe.map
                (validateBool
                    >> Result.map Just
                    >> Result.mapError (Tuple.pair key)
                )
                >> Maybe.withDefault (Ok Nothing)
            )


boolParser : { default : Bool } -> String -> Parser (ParseResult Bool)
boolParser { default } key =
    Query.string key
        |> Query.map
            (Maybe.map (validateBool >> Result.mapError (Tuple.pair key))
                >> Maybe.withDefault (Ok default)
            )



-- Encoders


encodeErrors : Errors -> Encode.Value
encodeErrors errors =
    Encode.object
        [ ( "errors"
          , errors |> Encode.dict identity Encode.string
          )
        ]

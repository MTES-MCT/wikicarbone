module Data.Ecobalyse.Product exposing
    ( Product
    , ProductName
    , Products
    , WeightRatio
    , decodeProducts
    , empty
    , findByName
    , getImpact
    , getTotalImpact
    , getTotalWeight
    , getWeightRatio
    , isUnit
    , updateAmount
    )

import Data.Ecobalyse.Process as Process
    exposing
        ( Amount
        , Impacts
        , ImpactsForProcesses
        , Process
        , stringToProcessName
        )
import Data.Impact as Impact
import Data.Unit as Unit
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Result.Extra as RE


type alias Step =
    Dict String Process


trigramsToImpact : Dict.Dict String (Process.Impacts -> Float)
trigramsToImpact =
    Dict.fromList
        [ ( "acd", .acd )
        , ( "ozd", .ozd )
        , ( "cch", .cch )
        , ( "ccb", .ccb )
        , ( "ccf", .ccf )
        , ( "ccl", .ccl )
        , ( "fwe", .fwe )
        , ( "swe", .swe )
        , ( "tre", .tre )
        , ( "pco", .pco )
        , ( "pma", .pma )
        , ( "ior", .ior )
        , ( "fru", .fru )
        , ( "mru", .mru )
        , ( "ldu", .ldu )
        , ( "wtu", .wtu )
        , ( "etf", .etf )
        , ( "htc", .htc )
        , ( "htn", .htn )
        ]


type alias Product =
    { consumer : Step
    , supermarket : Step
    , distribution : Step
    , packaging : Step
    , plant : Step
    }


type alias ProductName =
    String


type alias Products =
    Dict ProductName Product


empty : Products
empty =
    Dict.empty


type alias Ingredient =
    ( String, Unit.Ratio )


type alias ProductDefinition =
    { consumer : List Ingredient
    , supermarket : List Ingredient
    , distribution : List Ingredient
    , packaging : List Ingredient
    , plant : List Ingredient
    }


type alias WeightRatio =
    { processName : String
    , weightRatio : Float
    }


insertProcess : String -> Amount -> Impacts -> Step -> Step
insertProcess processName amount impacts step =
    Dict.insert processName (Process amount impacts) step


stepFromIngredients : List Ingredient -> ImpactsForProcesses -> Result String Step
stepFromIngredients ingredients impactsForProcesses =
    ingredients
        |> List.foldl
            (\( processName, amount ) stepResult ->
                let
                    impactsResult : Result String Impacts
                    impactsResult =
                        Process.findByName (stringToProcessName processName) impactsForProcesses
                in
                Result.map2 (insertProcess processName amount) impactsResult stepResult
            )
            (Ok Dict.empty)


productFromDefinition : ImpactsForProcesses -> ProductDefinition -> Result String Product
productFromDefinition impactsForProcesses { consumer, supermarket, distribution, packaging, plant } =
    Ok Product
        |> RE.andMap (stepFromIngredients consumer impactsForProcesses)
        |> RE.andMap (stepFromIngredients supermarket impactsForProcesses)
        |> RE.andMap (stepFromIngredients distribution impactsForProcesses)
        |> RE.andMap (stepFromIngredients packaging impactsForProcesses)
        |> RE.andMap (stepFromIngredients plant impactsForProcesses)


updateAmount : Maybe WeightRatio -> String -> Amount -> Step -> Step
updateAmount maybeWeightRatio processName newAmount step =
    step
        |> Dict.update processName
            (Maybe.map
                (\process ->
                    { process | amount = newAmount }
                )
            )
        |> updateWeight maybeWeightRatio


updateWeight : Maybe WeightRatio -> Step -> Step
updateWeight maybeWeightRatio step =
    case maybeWeightRatio of
        Nothing ->
            step

        Just { processName, weightRatio } ->
            let
                updatedRawWeight =
                    getTotalWeight step

                updatedWeight =
                    updatedRawWeight
                        * weightRatio
                        |> Unit.Ratio
            in
            step
                |> Dict.update processName
                    (Maybe.map
                        (\process ->
                            { process | amount = updatedWeight }
                        )
                    )


findByName : String -> Products -> Result String Product
findByName name =
    Dict.get name
        >> Result.fromMaybe ("Produit introuvable par nom : " ++ name)


decodeAmount : Decoder Amount
decodeAmount =
    Decode.float
        |> Decode.map Unit.ratio


decodeIngredients : Decoder (List Ingredient)
decodeIngredients =
    Decode.dict decodeAmount
        |> Decode.map Dict.toList


decodeProductDefinition : Decoder ProductDefinition
decodeProductDefinition =
    Decode.succeed ProductDefinition
        |> Pipe.required "consumer" decodeIngredients
        |> Pipe.required "supermarket" decodeIngredients
        |> Pipe.required "distribution" decodeIngredients
        |> Pipe.required "packaging" decodeIngredients
        |> Pipe.required "plant" decodeIngredients


insertProduct : ProductName -> Product -> Products -> Products
insertProduct productName product products =
    Dict.insert productName product products


productsFromDefinitions : ImpactsForProcesses -> Dict ProductName ProductDefinition -> Result String Products
productsFromDefinitions impactsForProcesses definitions =
    definitions
        |> Dict.foldl
            (\productName productDefinition productsResult ->
                let
                    productResult : Result String Product
                    productResult =
                        productFromDefinition impactsForProcesses productDefinition
                in
                Result.map2 (insertProduct productName) productResult productsResult
            )
            (Ok Dict.empty)


decodeProducts : ImpactsForProcesses -> Decoder Products
decodeProducts impactsForProcesses =
    Decode.dict decodeProductDefinition
        |> Decode.andThen
            (\definitions ->
                definitions
                    |> productsFromDefinitions impactsForProcesses
                    |> (\result ->
                            case result of
                                Ok products ->
                                    Decode.succeed products

                                Err error ->
                                    Decode.fail error
                       )
            )



-- utilities


isUnit : String -> Bool
isUnit processName =
    String.endsWith "/ FR U" processName


getTotalImpact : Impact.Trigram -> List Impact.Definition -> Step -> Float
getTotalImpact trigram definitions step =
    step
        |> Dict.foldl
            (\_ { amount, impacts } total ->
                let
                    impact =
                        getImpact trigram definitions impacts
                in
                total + (Unit.ratioToFloat amount * impact)
            )
            0


getImpact : Impact.Trigram -> List Impact.Definition -> Process.Impacts -> Float
getImpact (Impact.Trigram trigram) definitions impacts =
    case Dict.get trigram trigramsToImpact of
        Just impactGetter ->
            impactGetter impacts

        Nothing ->
            if trigram == "pef" then
                -- PEF is a computed impact
                Dict.keys trigramsToImpact
                    -- Get all the impacts we have, and normalize/weigh them
                    |> List.map Impact.trg
                    |> List.map
                        (\trig ->
                            case Impact.getDefinition trig definitions of
                                Ok { pefData } ->
                                    case pefData of
                                        Just { normalization, weighting } ->
                                            getImpact trig definitions impacts
                                                |> Unit.impact
                                                |> Unit.impactPefScore normalization weighting
                                                |> Unit.impactToFloat

                                        Nothing ->
                                            0.0

                                Err _ ->
                                    0.0
                        )
                    |> List.foldl (+) 0.0

            else
                0.0


getTotalWeight : Step -> Float
getTotalWeight step =
    step
        |> Dict.foldl
            (\processName { amount } total ->
                if isUnit processName then
                    total

                else
                    total + Unit.ratioToFloat amount
            )
            0


getWeightRatio : Product -> Maybe WeightRatio
getWeightRatio product =
    -- TODO: HACK, we assume that the process "at plant" that is the heavier is the total
    -- "final" weight, versus the total weight of the raw ingredients. We only need this
    -- if there's some kind of process that "looses weight" in the process, and we assume this
    -- process should be named ".... / FR U" (eg "Cooking, industrial, 1kg of cooked product/ FR U")
    let
        maybeProcessName =
            getWeightLosingUnitProcessName product.plant

        totalIngredientsWeight =
            getTotalWeight product.plant
    in
    maybeProcessName
        |> Maybe.andThen
            (\processName ->
                product.plant
                    |> Dict.get processName
                    |> Maybe.map
                        (\process ->
                            { processName = processName
                            , weightRatio =
                                Unit.ratioToFloat process.amount
                                    / totalIngredientsWeight
                            }
                        )
            )


getWeightLosingUnitProcessName : Step -> Maybe String
getWeightLosingUnitProcessName step =
    step
        |> Dict.toList
        -- Only keep processes with names ending with "/ FR U"
        |> List.filter (Tuple.first >> String.endsWith "/ FR U")
        -- Sort by heavier to lighter
        |> List.sortBy (Tuple.second >> .amount >> Unit.ratioToFloat)
        |> List.reverse
        -- Only keep the process names
        |> List.map Tuple.first
        -- Take the heaviest
        |> List.head

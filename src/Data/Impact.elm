module Data.Impact exposing
    ( BonusImpacts
    , Impacts
    , addBonusImpacts
    , applyBonus
    , bonusesImpactAsChartEntries
    , decodeImpacts
    , defaultFoodTrigram
    , defaultTextileTrigram
    , encodeAggregatedScoreChartEntry
    , encodeBonusesImpacts
    , encodeImpacts
    , filterImpacts
    , getAggregatedScoreData
    , getImpact
    , grabImpactFloat
    , impactsFromDefinitons
    , invalid
    , mapImpacts
    , noBonusImpacts
    , noImpacts
    , parseTrigram
    , perKg
    , sumImpacts
    , toDict
    , toProtectionAreas
    , totalBonusesImpactAsChartEntry
    , updateImpact
    )

import Data.Impact.Definition as Definition exposing (Definition, Trigram)
import Data.Scope as Scope exposing (Scope)
import Data.Unit as Unit
import Dict.Any as AnyDict exposing (AnyDict)
import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Mass exposing (Mass)
import Quantity
import Url.Parser as Parser exposing (Parser)


type alias BonusImpacts =
    -- Note: these are always expressed in ecoscore (ecs) µPt
    { agroDiversity : Unit.Impact
    , agroEcology : Unit.Impact
    , animalWelfare : Unit.Impact
    , total : Unit.Impact
    }


addBonusImpacts : BonusImpacts -> BonusImpacts -> BonusImpacts
addBonusImpacts a b =
    { agroDiversity = Quantity.plus a.agroDiversity b.agroDiversity
    , agroEcology = Quantity.plus a.agroEcology b.agroEcology
    , animalWelfare = Quantity.plus a.animalWelfare b.animalWelfare
    , total = Quantity.plus a.total b.total
    }


applyBonus : Unit.Impact -> Impacts -> Impacts
applyBonus bonus impacts =
    let
        ecoScore =
            getImpact Definition.Ecs impacts
    in
    impacts
        |> insertWithoutAggregateComputation Definition.Ecs
            (Quantity.difference ecoScore bonus)


noBonusImpacts : BonusImpacts
noBonusImpacts =
    { agroDiversity = Unit.impact 0
    , agroEcology = Unit.impact 0
    , animalWelfare = Unit.impact 0
    , total = Unit.impact 0
    }


bonusesImpactAsChartEntries : BonusImpacts -> List { name : String, value : Float, color : String }
bonusesImpactAsChartEntries { agroDiversity, agroEcology, animalWelfare } =
    -- We want those bonuses to appear as negative values on the chart
    [ { name = "Bonus diversité agricole", value = -(Unit.impactToFloat agroDiversity), color = "#808080" }
    , { name = "Bonus infrastructures agro-écologiques", value = -(Unit.impactToFloat agroEcology), color = "#a0a0a0" }
    , { name = "Bonus conditions d'élevage", value = -(Unit.impactToFloat animalWelfare), color = "#c0c0c0" }
    ]


totalBonusesImpactAsChartEntry : BonusImpacts -> { name : String, value : Float, color : String }
totalBonusesImpactAsChartEntry { total } =
    -- We want those bonuses to appear as negative values on the chart
    { name = "Bonus écologique", value = -(Unit.impactToFloat total), color = "#808080" }


type alias ProtectionAreas =
    -- Protection Areas is basically scientific slang for subscores
    { climate : Unit.Impact -- Climat
    , biodiversity : Unit.Impact -- Biodiversité
    , resources : Unit.Impact -- Ressources
    , health : Unit.Impact -- Santé environnementale
    }


invalid : Scope -> Definition
invalid scope =
    { trigram = defaultTrigram scope
    , source = { label = "N/A", url = "https://example.com/" }
    , label = "Not applicable"
    , description = "Not applicable"
    , unit = "N/A"
    , decimals = 0
    , quality = Definition.GoodQuality
    , pefData = Nothing
    , ecoscoreData = Nothing
    , scopes = []
    }


defaultFoodTrigram : Trigram
defaultFoodTrigram =
    Definition.Ecs


defaultTextileTrigram : Trigram
defaultTextileTrigram =
    Definition.Pef


defaultTrigram : Scope -> Trigram
defaultTrigram scope =
    case scope of
        Scope.Food ->
            defaultFoodTrigram

        Scope.Textile ->
            defaultTextileTrigram


toProtectionAreas : Impacts -> ProtectionAreas
toProtectionAreas (Impacts impactsPerKgWithoutBonuses) =
    let
        pick trigrams =
            impactsPerKgWithoutBonuses
                |> AnyDict.filter (\t _ -> List.member t trigrams)
                |> Impacts
                |> computeAggregatedScore .ecoscoreData
    in
    { climate =
        pick
            [ Definition.Cch -- Climate change
            ]
    , biodiversity =
        pick
            [ Definition.Bvi -- Biodiversity impact
            , Definition.Acd -- Acidification
            , Definition.Tre -- Terrestrial eutrophication
            , Definition.Fwe -- Freshwater Eutrophication
            , Definition.Swe -- Marine eutrophication
            , Definition.EtfC -- Ecotoxicity: freshwater
            , Definition.Ldu -- Land use
            ]
    , health =
        pick
            [ Definition.Ozd -- Ozone depletion
            , Definition.Ior -- Ionising radiation
            , Definition.Pco -- Photochemical ozone formation
            , Definition.HtnC -- Human toxicity: non-carcinogenic
            , Definition.HtcC -- Human toxicity: carcinogenic
            , Definition.Pma -- Particulate matter
            ]
    , resources =
        pick
            [ Definition.Wtu -- Water use
            , Definition.Fru -- Fossile resource use
            , Definition.Mru -- Minerals and metal resource use
            ]
    }



-- Impact data & scores


type Impacts
    = Impacts (AnyDict String Trigram Unit.Impact)


noImpacts : Impacts
noImpacts =
    AnyDict.empty Definition.toString
        |> Impacts


impactsFromDefinitons : Impacts
impactsFromDefinitons =
    List.map (\trigram -> ( trigram, Quantity.zero )) Definition.trigrams
        |> AnyDict.fromList Definition.toString
        |> Impacts


insertWithoutAggregateComputation : Trigram -> Unit.Impact -> Impacts -> Impacts
insertWithoutAggregateComputation trigram impact (Impacts impacts) =
    AnyDict.insert trigram impact impacts
        |> Impacts


getImpact : Trigram -> Impacts -> Unit.Impact
getImpact trigram (Impacts impacts) =
    AnyDict.get trigram impacts
        |> Maybe.withDefault Quantity.zero


grabImpactFloat : Unit.Functional -> Duration -> Trigram -> { a | impacts : Impacts } -> Float
grabImpactFloat funit daysOfWear trigram { impacts } =
    impacts
        |> getImpact trigram
        |> Unit.inFunctionalUnit funit daysOfWear
        |> Unit.impactToFloat


filterImpacts : (Trigram -> Unit.Impact -> Bool) -> Impacts -> Impacts
filterImpacts fn (Impacts impacts) =
    AnyDict.filter fn impacts
        |> Impacts


mapImpacts : (Trigram -> Unit.Impact -> Unit.Impact) -> Impacts -> Impacts
mapImpacts fn (Impacts impacts) =
    AnyDict.map fn impacts
        |> Impacts


perKg : Mass -> Impacts -> Impacts
perKg totalMass =
    mapImpacts (\_ -> Quantity.divideBy (Mass.inKilograms totalMass))


sumImpacts : List Impacts -> Impacts
sumImpacts =
    List.foldl
        (\impacts ->
            mapImpacts
                (\trigram impact ->
                    Quantity.sum [ getImpact trigram impacts, impact ]
                )
        )
        impactsFromDefinitons


toDict : Impacts -> AnyDict.AnyDict String Trigram Unit.Impact
toDict (Impacts impacts) =
    impacts


updateImpact : Trigram -> Unit.Impact -> Impacts -> Impacts
updateImpact trigram value =
    insertWithoutAggregateComputation trigram value
        >> updateAggregatedScores


decodeImpacts : Decoder Impacts
decodeImpacts =
    AnyDict.decode_
        (\str _ ->
            Definition.toTrigram str
                |> Maybe.map Ok
                |> Maybe.withDefault (Err <| "Trigramme d'impact inconnu: " ++ str)
        )
        Definition.toString
        Unit.decodeImpact
        |> Decode.map Impacts
        -- Update the aggregated scores as soon as the impacts are decoded, then we never need to compute them again.
        |> Decode.map updateAggregatedScores


encodeBonusesImpacts : BonusImpacts -> Encode.Value
encodeBonusesImpacts bonuses =
    Encode.object
        [ ( "agroDiversity", Unit.impactToFloat bonuses.agroDiversity |> Encode.float )
        , ( "agroEcology", Unit.impactToFloat bonuses.agroEcology |> Encode.float )
        , ( "animalWelfare", Unit.impactToFloat bonuses.animalWelfare |> Encode.float )
        , ( "total", Unit.impactToFloat bonuses.total |> Encode.float )
        ]


encodeImpacts : Scope -> Impacts -> Encode.Value
encodeImpacts scope (Impacts impacts) =
    impacts
        |> AnyDict.filter
            (\trigram _ ->
                trigram
                    |> Definition.get
                    |> Maybe.map (.scopes >> List.member scope)
                    |> Maybe.withDefault False
            )
        |> AnyDict.encode Definition.toString Unit.encodeImpact


updateAggregatedScores : Impacts -> Impacts
updateAggregatedScores impacts =
    let
        aggregateScore getter trigram =
            impacts
                |> computeAggregatedScore getter
                |> insertWithoutAggregateComputation trigram
    in
    impacts
        |> aggregateScore .ecoscoreData Definition.Ecs
        |> aggregateScore .pefData Definition.Pef


getAggregatedScoreData :
    (Definition -> Maybe Definition.AggregatedScoreData)
    -> Impacts
    -> List { color : String, name : String, value : Float }
getAggregatedScoreData getter (Impacts impacts) =
    AnyDict.foldl
        (\trigram impact acc ->
            case Definition.get trigram of
                Just def ->
                    case getter def of
                        Just { normalization, weighting, color } ->
                            { name = def.label
                            , value =
                                impact
                                    |> Unit.impactAggregateScore normalization weighting
                                    |> Unit.impactToFloat
                            , color = color ++ "bb" -- pastelization through slight transparency
                            }
                                :: acc

                        Nothing ->
                            acc

                Nothing ->
                    acc
        )
        []
        impacts


encodeAggregatedScoreChartEntry : { name : String, value : Float, color : String } -> Encode.Value
encodeAggregatedScoreChartEntry entry =
    -- This is to be easily used with Highcharts.js in a Web Component
    Encode.object
        [ ( "name", Encode.string entry.name )
        , ( "y", Encode.float entry.value )
        , ( "color", Encode.string entry.color )
        ]


computeAggregatedScore : (Definition -> Maybe Definition.AggregatedScoreData) -> Impacts -> Unit.Impact
computeAggregatedScore getter (Impacts impacts) =
    impacts
        |> AnyDict.map
            (\trigram impact ->
                case Definition.get trigram |> Maybe.map getter of
                    Just (Just { normalization, weighting }) ->
                        impact
                            |> Unit.impactAggregateScore normalization weighting

                    _ ->
                        Quantity.zero
            )
        |> AnyDict.foldl (\_ -> Quantity.plus) Quantity.zero



-- Parser


parseTrigram : Scope -> Parser (Trigram -> a) a
parseTrigram scope =
    Parser.custom "TRIGRAM" <|
        \trigram ->
            Definition.toTrigram trigram
                |> Maybe.map Just
                |> Maybe.withDefault (Just <| defaultTrigram scope)

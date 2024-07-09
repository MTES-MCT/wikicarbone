module Data.Impact exposing
    ( ComplementsImpacts
    , Impacts
    , StepsImpacts
    , addComplementsImpacts
    , applyComplements
    , complementsImpactAsChartEntries
    , decodeImpacts
    , default
    , divideBy
    , divideComplementsImpactsBy
    , divideStepsImpactsBy
    , empty
    , encode
    , encodeAggregatedScoreChartEntry
    , encodeComplementsImpacts
    , encodeSingleImpact
    , getAggregatedScoreData
    , getImpact
    , getTotalComplementsImpacts
    , impactsWithComplements
    , mapComplementsImpacts
    , mapImpacts
    , maxEcotoxWeighting
    , minEcotoxWeighting
    , noComplementsImpacts
    , noStepsImpacts
    , parseTrigram
    , per100grams
    , perKg
    , setEcotoxWeighting
    , stepsColors
    , stepsImpactsAsChartEntries
    , sumEcosystemicImpacts
    , sumImpacts
    , toProtectionAreas
    , totalComplementsImpactAsChartEntry
    , updateAggregatedScores
    , updateImpact
    )

import Data.Color as Color
import Data.Impact.Definition as Definition exposing (Definition, Definitions, Trigram, Trigrams)
import Data.Unit as Unit
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Mass exposing (Mass)
import Quantity
import Url.Parser as Parser exposing (Parser)



-- Complements impacts


type alias ComplementsImpacts =
    -- Note: these are always expressed in ecoscore (ecs) Pts
    { -- Ecosystemic services impacts
      hedges : Unit.Impact
    , plotSize : Unit.Impact
    , cropDiversity : Unit.Impact
    , permanentPasture : Unit.Impact
    , livestockDensity : Unit.Impact

    -- Other impacts
    , microfibers : Unit.Impact
    , outOfEuropeEOL : Unit.Impact
    }


addComplementsImpacts : ComplementsImpacts -> ComplementsImpacts -> ComplementsImpacts
addComplementsImpacts a b =
    { -- Ecosystemic services impacts
      hedges = Quantity.plus a.hedges b.hedges
    , plotSize = Quantity.plus a.plotSize b.plotSize
    , cropDiversity = Quantity.plus a.cropDiversity b.cropDiversity
    , permanentPasture = Quantity.plus a.permanentPasture b.permanentPasture
    , livestockDensity = Quantity.plus a.livestockDensity b.livestockDensity

    -- Other impacts
    , microfibers = Quantity.plus a.microfibers b.microfibers
    , outOfEuropeEOL = Quantity.plus a.outOfEuropeEOL b.outOfEuropeEOL
    }


applyComplements : Unit.Impact -> Impacts -> Impacts
applyComplements complement impacts =
    let
        ecoScore =
            getImpact Definition.Ecs impacts
    in
    impacts
        |> insertWithoutAggregateComputation Definition.Ecs
            (Quantity.difference ecoScore complement)


divideComplementsImpactsBy : Float -> ComplementsImpacts -> ComplementsImpacts
divideComplementsImpactsBy n =
    mapComplementsImpacts (Quantity.divideBy n)


encodeComplementsImpacts : ComplementsImpacts -> Encode.Value
encodeComplementsImpacts complementsImpact =
    let
        negated =
            negateComplementsImpacts complementsImpact
    in
    Encode.object
        -- Ecosystemic services
        [ ( "hedges", Unit.encodeImpact negated.hedges )
        , ( "plotSize", Unit.encodeImpact negated.plotSize )
        , ( "cropDiversity", Unit.encodeImpact negated.cropDiversity )
        , ( "permanentPasture", Unit.encodeImpact negated.permanentPasture )
        , ( "livestockDensity", Unit.encodeImpact negated.livestockDensity )

        -- Textile complements
        , ( "microfibers", Unit.encodeImpact negated.microfibers )
        , ( "outOfEuropeEOL", Unit.encodeImpact negated.outOfEuropeEOL )
        ]


getTotalComplementsImpacts : ComplementsImpacts -> Unit.Impact
getTotalComplementsImpacts complementsImpacts =
    Quantity.sum
        [ complementsImpacts.hedges
        , complementsImpacts.plotSize
        , complementsImpacts.cropDiversity
        , complementsImpacts.permanentPasture
        , complementsImpacts.livestockDensity
        , complementsImpacts.microfibers
        , complementsImpacts.outOfEuropeEOL
        ]


mapComplementsImpacts : (Unit.Impact -> Unit.Impact) -> ComplementsImpacts -> ComplementsImpacts
mapComplementsImpacts fn ci =
    { hedges = fn ci.hedges
    , plotSize = fn ci.plotSize
    , cropDiversity = fn ci.cropDiversity
    , permanentPasture = fn ci.permanentPasture
    , livestockDensity = fn ci.livestockDensity
    , microfibers = fn ci.microfibers
    , outOfEuropeEOL = fn ci.outOfEuropeEOL
    }


negateComplementsImpacts : ComplementsImpacts -> ComplementsImpacts
negateComplementsImpacts =
    mapComplementsImpacts (Unit.impactToFloat >> negate >> Unit.impact)


noComplementsImpacts : ComplementsImpacts
noComplementsImpacts =
    { hedges = Unit.impact 0
    , plotSize = Unit.impact 0
    , cropDiversity = Unit.impact 0
    , permanentPasture = Unit.impact 0
    , livestockDensity = Unit.impact 0
    , microfibers = Unit.impact 0
    , outOfEuropeEOL = Unit.impact 0
    }


impactsWithComplements : ComplementsImpacts -> Impacts -> Impacts
impactsWithComplements complementsImpacts impacts =
    let
        complementsImpact =
            getTotalComplementsImpacts complementsImpacts

        ecsWithComplements =
            getImpact Definition.Ecs impacts
                -- Reminder: substracting a malus — a.k.a negative complement — adds to the total impact
                |> Quantity.minus complementsImpact
    in
    impacts
        |> insertWithoutAggregateComputation Definition.Ecs ecsWithComplements


sumEcosystemicImpacts : ComplementsImpacts -> Unit.Impact
sumEcosystemicImpacts c =
    Quantity.sum
        [ c.hedges
        , c.plotSize
        , c.cropDiversity
        , c.permanentPasture
        , c.livestockDensity
        ]


complementsImpactAsChartEntries : ComplementsImpacts -> List { name : String, value : Float, color : String }
complementsImpactAsChartEntries c =
    -- Notes:
    -- - We want those complements/bonuses to appear as negative values on the chart
    -- - We want to sum ecosystemic service components impacts to only have a single entry in the charts
    [ { name = "Services écosystémiques", value = -(Unit.impactToFloat (sumEcosystemicImpacts c)), color = "#606060" }
    , { name = "Complément microfibres", value = -(Unit.impactToFloat c.microfibers), color = "#c0c0c0" }
    , { name = "Complément export hors-Europe", value = -(Unit.impactToFloat c.outOfEuropeEOL), color = "#e0e0e0" }
    ]


totalComplementsImpactAsChartEntry : ComplementsImpacts -> { name : String, value : Float, color : String }
totalComplementsImpactAsChartEntry complementsImpacts =
    -- We want bonuses to appear as negative values on the chart, maluses as positive ones
    { name = "Compléments"
    , value = -(Unit.impactToFloat (getTotalComplementsImpacts complementsImpacts))
    , color = "#808080"
    }



-- Steps impacts


type alias Steps a =
    { materials : a
    , transform : a
    , packaging : a
    , transports : a
    , distribution : a
    , usage : a
    , endOfLife : a
    }


type alias StepsImpacts =
    Steps (Maybe Unit.Impact)


mapSteps : (a -> a) -> Steps a -> Steps a
mapSteps fn steps =
    { materials = fn steps.materials
    , transform = fn steps.transform
    , packaging = fn steps.packaging
    , transports = fn steps.transports
    , distribution = fn steps.distribution
    , usage = fn steps.usage
    , endOfLife = fn steps.endOfLife
    }


noStepsImpacts : StepsImpacts
noStepsImpacts =
    { materials = Nothing
    , transform = Nothing
    , packaging = Nothing
    , transports = Nothing
    , distribution = Nothing
    , usage = Nothing
    , endOfLife = Nothing
    }


divideStepsImpactsBy : Float -> StepsImpacts -> StepsImpacts
divideStepsImpactsBy n =
    mapSteps (Maybe.map (Quantity.divideBy n))


type alias StepsColors =
    Steps String


stepsColors : StepsColors
stepsColors =
    { materials = Color.purple
    , transform = Color.pink
    , packaging = Color.blue
    , transports = Color.green
    , distribution = Color.red
    , usage = Color.yellow
    , endOfLife = Color.turquoise
    }


stepsImpactsAsChartEntries : StepsImpacts -> List { name : String, value : Float, color : String }
stepsImpactsAsChartEntries stepsImpacts =
    [ ( "Matières premières", stepsImpacts.materials, stepsColors.materials )
    , ( "Transformation", stepsImpacts.transform, stepsColors.transform )
    , ( "Emballage", stepsImpacts.packaging, stepsColors.packaging )
    , ( "Transports", stepsImpacts.transports, stepsColors.transports )
    , ( "Distribution", stepsImpacts.distribution, stepsColors.distribution )
    , ( "Utilisation", stepsImpacts.usage, stepsColors.usage )
    , ( "Fin de vie", stepsImpacts.endOfLife, stepsColors.endOfLife )
    ]
        |> List.map
            (\( label, maybeValue, color ) ->
                { name = label
                , color = color
                , value =
                    -- All categories MUST be filled in order to allow comparing Food and Textile simulations
                    -- So, when we don't have a value for a given step, we fallback to zero
                    maybeValue
                        |> Maybe.map Unit.impactToFloat
                        |> Maybe.withDefault 0
                }
            )



-- Protection areas


type alias ProtectionAreas =
    -- Protection Areas is basically scientific slang for subscores
    { climate : Unit.Impact -- Climat
    , biodiversity : Unit.Impact -- Biodiversité
    , resources : Unit.Impact -- Ressources
    , health : Unit.Impact -- Santé environnementale
    }


toProtectionAreas : Definitions -> Impacts -> ProtectionAreas
toProtectionAreas definitions (Impacts impactsPerKgWithoutComplements) =
    let
        pick trigrams =
            impactsPerKgWithoutComplements
                |> Definition.filter (\t -> List.member t trigrams) (always Quantity.zero)
                |> Impacts
                |> computeAggregatedScore definitions .ecoscoreData
    in
    { climate =
        pick
            [ Definition.Cch -- Climate change
            ]
    , biodiversity =
        pick
            [ Definition.Acd -- Acidification
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
    = Impacts (Trigrams Unit.Impact)


default : Definition.Trigram
default =
    Definition.Ecs


divideBy : Float -> Impacts -> Impacts
divideBy n =
    mapImpacts (\_ -> Quantity.divideBy n)


empty : Impacts
empty =
    Impacts (Definition.init Quantity.zero)


insertWithoutAggregateComputation : Trigram -> Unit.Impact -> Impacts -> Impacts
insertWithoutAggregateComputation trigram impact (Impacts impacts) =
    Definition.update trigram (always impact) impacts
        |> Impacts


getImpact : Trigram -> Impacts -> Unit.Impact
getImpact trigram (Impacts impacts) =
    Definition.get trigram impacts


mapImpacts : (Trigram -> Unit.Impact -> Unit.Impact) -> Impacts -> Impacts
mapImpacts fn (Impacts impacts) =
    Definition.map fn impacts
        |> Impacts


per100grams : Mass -> Impacts -> Impacts
per100grams totalMass =
    perKg totalMass >> mapImpacts (\_ -> Quantity.divideBy 10)


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
        empty


updateImpact : Definitions -> Trigram -> Unit.Impact -> Impacts -> Impacts
updateImpact definitions trigram value =
    insertWithoutAggregateComputation trigram value
        >> updateAggregatedScores definitions


decodeImpacts : Decoder Impacts
decodeImpacts =
    Definition.decodeBase (always Unit.decodeImpact)
        |> Decode.map Impacts


encode : Impacts -> Encode.Value
encode (Impacts impacts) =
    impacts
        |> Definition.encodeBase Unit.encodeImpact


encodeSingleImpact : Impacts -> Trigram -> Encode.Value
encodeSingleImpact (Impacts impacts) trigram =
    Encode.object
        [ ( Definition.toString trigram, Unit.encodeImpact (Definition.get trigram impacts) )
        ]


updateAggregatedScores : Definitions -> Impacts -> Impacts
updateAggregatedScores definitions impacts =
    let
        aggregateScore getter trigram =
            impacts
                |> computeAggregatedScore definitions getter
                |> insertWithoutAggregateComputation trigram
    in
    impacts
        |> aggregateScore .ecoscoreData Definition.Ecs
        |> aggregateScore .pefData Definition.Pef


getAggregatedScoreData :
    Definitions
    -> (Definition -> Maybe Definition.AggregatedScoreData)
    -> Impacts
    -> List { color : String, name : String, value : Float }
getAggregatedScoreData definitions getter (Impacts impacts) =
    Definition.foldl
        (\trigram impact acc ->
            let
                def =
                    Definition.get trigram definitions
            in
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


computeAggregatedScore : Definitions -> (Definition -> Maybe Definition.AggregatedScoreData) -> Impacts -> Unit.Impact
computeAggregatedScore definitions getter (Impacts impacts) =
    impacts
        |> Definition.map
            (\trigram impact ->
                Definition.get trigram definitions
                    |> getter
                    |> Maybe.map
                        (\{ normalization, weighting } ->
                            impact
                                |> Unit.impactAggregateScore normalization weighting
                        )
                    |> Maybe.withDefault Quantity.zero
            )
        |> Definition.foldl (\_ -> Quantity.plus) Quantity.zero


minEcotoxWeighting : Unit.Ratio
minEcotoxWeighting =
    Unit.ratio 0


maxEcotoxWeighting : Unit.Ratio
maxEcotoxWeighting =
    Unit.ratio 0.25


{-| Set the ecotoxicity weighting (EtfC) then redistribute other Ecoscore weightings
accordingly. The methodology and formulas are described in this card:
<https://www.notion.so/Rendre-param-trable-la-pond-ration-de-l-cotox-894d42e217c6448a883346203dff8db4>
FIXME: ensure the card contents are moved to the public documentation eventually
-}
setEcotoxWeighting : Unit.Ratio -> Definitions -> Definitions
setEcotoxWeighting (Unit.Ratio weighting) definitions =
    let
        defsToUpdate =
            [ Definition.Acd
            , Definition.Fru
            , Definition.Fwe
            , Definition.Ior
            , Definition.Ldu
            , Definition.Mru
            , Definition.Ozd
            , Definition.Pco
            , Definition.Pma
            , Definition.Swe
            , Definition.Tre
            , Definition.Wtu
            ]

        cleanWeighting =
            weighting
                |> clamp (Unit.ratioToFloat minEcotoxWeighting) (Unit.ratioToFloat maxEcotoxWeighting)
    in
    definitions
        -- Start with updating EtfC with the provided ratio
        |> Definition.update Definition.EtfC
            (\({ ecoscoreData } as definition) ->
                { definition
                    | ecoscoreData =
                        ecoscoreData
                            |> Maybe.map (\data -> { data | weighting = Unit.ratio cleanWeighting })
                }
            )
        -- Then redistribute the other weightings accordingly
        |> Definition.map
            (\trg def ->
                if List.member trg defsToUpdate then
                    let
                        pefWeighting =
                            def.pefData
                                |> Maybe.map .weighting
                                |> Maybe.withDefault (Unit.ratio 0)
                                |> Unit.ratioToFloat
                    in
                    { def
                        | ecoscoreData =
                            def.ecoscoreData
                                |> Maybe.map
                                    (\ecoscoreData ->
                                        { ecoscoreData
                                          -- = (PEF weighting for this trigram) * (78.94% - custom weighting) / 73.05%
                                            | weighting = Unit.ratio (pefWeighting * (0.7894 - cleanWeighting) / 0.7305)
                                        }
                                    )
                    }

                else
                    def
            )



-- Parser


parseTrigram : Parser (Trigram -> a) a
parseTrigram =
    Parser.custom "TRIGRAM" <|
        \trigram ->
            Definition.toTrigram trigram
                |> Result.toMaybe

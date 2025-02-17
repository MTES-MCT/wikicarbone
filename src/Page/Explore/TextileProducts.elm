module Page.Explore.TextileProducts exposing (table)

import Data.Dataset as Dataset
import Data.Env as Env
import Data.Scope exposing (Scope)
import Data.Split as Split
import Data.Textile.Economics as Economics
import Data.Textile.Fabric as Fabric
import Data.Textile.Formula as Formula
import Data.Textile.LifeCycle as LifeCycle
import Data.Textile.MakingComplexity as MakingComplexity
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Query as TextileQuery
import Data.Textile.Simulator as Simulator
import Data.Textile.Step.Label as Label
import Data.Unit as Unit
import Duration
import Energy
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Explore.Common as Common
import Page.Explore.Table as Table exposing (Table)
import Quantity
import Route
import Static.Db exposing (Db)
import Views.Format as Format
import Volume


withTitle : String -> Html msg
withTitle str =
    span [ title str ] [ text str ]


table : Db -> { detailed : Bool, scope : Scope } -> Table Product String msg
table db { detailed, scope } =
    { filename = "products"
    , toId = .id >> Product.idToString
    , toRoute = .id >> Just >> Dataset.TextileProducts >> Route.Explore scope
    , legend =
        [ ul [ class "list-unstyled text-muted p-2 m-0" ]
            [ li [] [ strong [] [ text "*" ], text " Modifié au changement de catégorie de produit" ]
            , li [] [ strong [] [ text "**" ], text " Modifié au changement de catégorie, car dépendant d’autres paramètres" ]
            ]
        ]
    , columns =
        [ { label = "Identifiant"
          , toValue = Table.StringValue <| .id >> Product.idToString
          , toCell =
                \product ->
                    if detailed then
                        code [] [ text (Product.idToString product.id) ]

                    else
                        a [ Route.href (Route.Explore scope (Dataset.TextileProducts (Just product.id))) ]
                            [ code [] [ text (Product.idToString product.id) ] ]
          }
        , { label = "Produit(s) concerné(s)"
          , toValue = Table.StringValue .name
          , toCell = .name >> text
          }
        , { label = "Titrage*"
          , toValue = Table.FloatValue <| .yarnSize >> Unit.yarnSizeInKilometers
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ product.yarnSize |> Format.yarnSize ]
          }
        , { label = "Grammage*"
          , toValue = Table.IntValue <| .surfaceMass >> Unit.surfaceMassInGramsPerSquareMeters
          , toCell =
                \{ surfaceMass } ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.surfaceMass surfaceMass
                        ]
          }
        , let
            computeThreadDensity { surfaceMass, yarnSize } =
                Formula.computeThreadDensity surfaceMass yarnSize
          in
          { label = "Densité de fils**"
          , toValue = Table.FloatValue <| computeThreadDensity >> Unit.threadDensityToFloat
          , toCell = computeThreadDensity >> Format.threadDensity
          }
        , { label = "Volume*"
          , toValue = Table.FloatValue <| .endOfLife >> .volume >> Volume.inCubicMeters
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.m3 product.endOfLife.volume ]
          }
        , { label = "Étoffe*"
          , toValue = Table.StringValue (.fabric >> Fabric.toLabel)
          , toCell = .fabric >> Fabric.toLabel >> text
          }
        , let
            picking product surfaceMass ys =
                let
                    outputMass =
                        TextileQuery.default
                            |> TextileQuery.updateProduct product
                            |> Simulator.compute db
                            |> Result.map (.lifeCycle >> LifeCycle.getStepProp Label.Fabric .outputMass Quantity.zero)
                            |> Result.withDefault Quantity.zero

                    outputSurface =
                        Unit.surfaceMassToSurface surfaceMass outputMass

                    threadDensity =
                        Formula.computeThreadDensity surfaceMass ys
                in
                outputSurface
                    |> Formula.computePicking threadDensity
          in
          { label = "Duites.m**"
          , toValue =
                Table.FloatValue <|
                    \({ surfaceMass, yarnSize } as product) ->
                        picking product surfaceMass yarnSize
                            |> Unit.pickPerMeterToFloat
          , toCell =
                \({ surfaceMass, yarnSize } as product) ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ picking product surfaceMass yarnSize
                            |> Format.picking
                        ]
          }
        , { label = "Stocks dormants"
          , toValue = Table.FloatValue (Split.toPercent Env.defaultDeadStock |> always)
          , toCell =
                div [ classList [ ( "text-center", not detailed ) ] ]
                    [ Format.splitAsPercentage 0 Env.defaultDeadStock ]
                    |> always
          }
        , { label = "Confection (complexité)*"
          , toValue = Table.StringValue <| .making >> .complexity >> MakingComplexity.toLabel
          , toCell = .making >> .complexity >> MakingComplexity.toLabel >> text
          }
        , { label = "Confection (# minutes)*"
          , toValue = Table.FloatValue <| Product.getMakingDurationInMinutes >> Duration.inMinutes
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Product.getMakingDurationInMinutes product |> Format.minutes ]
          }
        , { label = "Confection (taux de perte)*"
          , toValue = Table.FloatValue <| .making >> .pcrWaste >> Split.toPercent
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.splitAsPercentage 2 product.making.pcrWaste ]
          }
        , { label = "Nombre de jours porté*"
          , toValue = Table.FloatValue <| .use >> .daysOfWear >> Duration.inDays
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.days product.use.daysOfWear ]
          }
        , { label = "Utilisations avant lavage*"
          , toValue = Table.IntValue <| .use >> .wearsPerCycle
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ text <| String.fromInt product.use.wearsPerCycle ]
          }
        , { label = "Cycles d'entretien (par défaut)**"
          , toValue = Table.IntValue <| .use >> .defaultNbCycles
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ text <| String.fromInt product.use.defaultNbCycles ]
          }
        , { label = "Repassage*"

          -- Note: Much better expressing electricity consumption in kWh than in MJ
          , toValue = Table.FloatValue <| .use >> .ironingElec >> Energy.inKilowattHours
          , toCell = .use >> .ironingElec >> Format.kilowattHours
          }
        , { label = "Procédé d'utilisation hors-repassage**"
          , toValue = Table.StringValue <| .use >> .nonIroningProcess >> .name
          , toCell = .use >> .nonIroningProcess >> .name >> withTitle
          }
        , { label = "Séchage électrique*"
          , toValue = Table.FloatValue <| .use >> .ratioDryer >> Split.toPercent
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.splitAsPercentage 0 product.use.ratioDryer ]
          }
        , { label = "Repassage (part)*"
          , toValue = Table.FloatValue <| .use >> .ratioIroning >> Split.toPercent
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.splitAsPercentage 0 product.use.ratioIroning ]
          }
        , { label = "Repassage (temps)*"
          , toValue = Table.FloatValue <| .use >> .timeIroning >> Duration.inHours
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.hours product.use.timeIroning ]
          }
        , { label = "Prix par défaut"
          , toValue = Table.FloatValue <| .economics >> .price >> Economics.priceToFloat
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.priceInEUR product.economics.price ]
          }
        , { label = "Coût de réparation par défaut*"
          , toValue = Table.FloatValue <| .economics >> .repairCost >> Economics.priceToFloat
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ Format.priceInEUR product.economics.repairCost ]
          }
        , { label = "Type d'entreprise"
          , toValue = Table.StringValue <| .economics >> .business >> Economics.businessToLabel
          , toCell = .economics >> .business >> Economics.businessToLabel >> text
          }
        , { label = "Nombre de références"
          , toValue = Table.IntValue <| .economics >> .numberOfReferences
          , toCell =
                \product ->
                    div [ classList [ ( "text-center", not detailed ) ] ]
                        [ product.economics.numberOfReferences |> String.fromInt |> text
                        , text " références"
                        ]
          }
        , { label = "Traçabilité affichée\u{00A0}?"
          , toValue = Table.StringValue <| .economics >> .traceability >> Common.boolText
          , toCell = .economics >> .traceability >> Common.boolText >> text
          }
        ]
    }

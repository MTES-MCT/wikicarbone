module Views.Component exposing (editorView)

import Autocomplete exposing (Autocomplete)
import Data.AutocompleteSelector as AutocompleteSelector
import Data.Component as Component exposing (Component, ComponentItem, ExpandedProcessItem)
import Data.Dataset as Dataset
import Data.Impact.Definition exposing (Definition)
import Data.Process as Process
import Data.Scope exposing (Scope)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import List.Extra as LE
import Route
import Views.Alert as Alert
import Views.Format as Format
import Views.Icon as Icon
import Views.Link as Link


type alias Config db msg =
    { addLabel : String
    , allowExpandDetails : Bool
    , db : Component.DataContainer db
    , detailed : List Component.Id
    , impact : Definition
    , items : List ComponentItem
    , noOp : msg
    , openSelectModal : Autocomplete Component -> msg
    , removeItem : Component.Id -> msg
    , results : Component.Results
    , scope : Scope
    , setDetailed : List Component.Id -> msg
    , title : String
    , updateItem : ComponentItem -> msg
    }


addButton : Config db msg -> Html msg
addButton { addLabel, db, items, openSelectModal } =
    let
        availableComponents =
            db.components
                |> Component.available (List.map .id items)

        -- FIXME: this should rather be initiated in page update
        autocompleteState =
            AutocompleteSelector.init .name availableComponents
    in
    button
        [ class "btn btn-outline-primary w-100"
        , class "d-flex justify-content-center align-items-center"
        , class "gap-1 w-100"
        , id "add-new-element"
        , disabled <| List.length availableComponents == 0
        , onClick <| openSelectModal autocompleteState
        ]
        [ i [ class "icon icon-plus" ] []
        , text addLabel
        ]


componentView :
    Config db msg
    -> ( Component.Quantity, Component, List ExpandedProcessItem )
    -> Component.Results
    -> List (Html msg)
componentView config ( quantity, component, processAmounts ) itemResults =
    let
        collapsed =
            config.detailed
                |> List.member component.id
                |> not
    in
    List.concat
        [ [ tr []
                [ th [ class "ps-3 align-middle", scope "col" ]
                    [ if config.allowExpandDetails then
                        button
                            [ class "btn btn-link text-dark text-decoration-none font-monospace fs-5  p-0 m-0"
                            , onClick <|
                                config.setDetailed <|
                                    if collapsed && not (List.member component.id config.detailed) then
                                        LE.unique <| component.id :: config.detailed

                                    else
                                        List.filter ((/=) component.id) config.detailed
                            ]
                            [ if collapsed then
                                text "▶"

                              else
                                text "▼"
                            ]

                      else
                        text ""
                    ]
                , td [ class "ps-0 align-middle" ]
                    [ quantity |> quantityInput config component.id ]
                , td [ class "align-middle text-truncate w-100 fw-bold", colspan 2 ]
                    [ text component.name ]
                , td [ class "text-end align-middle text-nowrap" ]
                    [ Component.extractMass itemResults
                        |> Format.kg
                    ]
                , td [ class "text-end align-middle text-nowrap" ]
                    [ Component.extractImpacts itemResults
                        |> Format.formatImpact config.impact
                    ]
                , td [ class "pe-3 align-middle text-nowrap" ]
                    [ button
                        [ class "btn btn-outline-secondary"
                        , onClick (config.removeItem component.id)
                        ]
                        [ Icon.trash ]
                    ]
                ]
          , if not collapsed then
                tr [ class "fs-7 text-muted" ]
                    [ th [] []
                    , th [ class "text-end", scope "col" ] [ text "Quantité" ]
                    , th [ scope "col" ] [ text "Procédé" ]
                    , th [ scope "col" ] [ text "Densité" ]
                    , th [ scope "col" ] [ text "Masse" ]
                    , th [ scope "col" ] [ text "Impact" ]
                    , th [ scope "col" ] [ text "" ]
                    ]

            else
                text ""
          ]
        , if not collapsed then
            Component.extractItems itemResults
                |> List.map2 (processView config.impact) processAmounts

          else
            []
        ]


editorView : Config db msg -> Html msg
editorView ({ db, items, results, scope, title } as config) =
    div [ class "card shadow-sm mb-3" ]
        [ div [ class "card-header d-flex align-items-center justify-content-between" ]
            [ h2 [ class "h5 mb-0" ]
                [ text title
                , Link.smallPillExternal
                    [ Route.href (Route.Explore scope (Dataset.Components scope Nothing))
                    , Attr.title "Explorer"
                    , attribute "aria-label" "Explorer"
                    ]
                    [ Icon.search ]
                ]
            , results
                |> Component.extractImpacts
                |> Format.formatImpact config.impact
            ]
        , if List.isEmpty items then
            div [ class "card-body" ] [ text "Aucun élément." ]

          else
            case Component.expandComponentItems db items of
                Err error ->
                    Alert.simple
                        { close = Nothing
                        , content = [ text error ]
                        , level = Alert.Danger
                        , title = Just "Erreur"
                        }

                Ok expandedComponentItems ->
                    div [ class "table-responsive" ]
                        [ table [ class "table mb-0" ]
                            [ thead []
                                [ tr [ class "fs-7 text-muted" ]
                                    [ th [] []
                                    , th [ class "ps-0", Attr.scope "col" ] [ text "Quantité" ]
                                    , th [ Attr.scope "col", colspan 2 ] [ text "Composant" ]
                                    , th [ Attr.scope "col" ] [ text "Masse" ]
                                    , th [ Attr.scope "col" ] [ text "Impact" ]
                                    , th [ Attr.scope "col" ] []
                                    ]
                                ]
                            , Component.extractItems results
                                |> List.map2 (componentView config) expandedComponentItems
                                |> List.concat
                                |> tbody []
                            ]
                        ]
        , addButton config
        ]


processView : Definition -> ExpandedProcessItem -> Component.Results -> Html msg
processView selectedImpact { amount, material } itemResults =
    tr [ class "fs-7" ]
        [ td [] []
        , td [ class "text-end text-nowrap" ]
            [ Format.amount material amount ]
        , td [ class "align-middle text-truncate w-100" ]
            [ text <| Process.getDisplayName material ]
        , td [ class "align-middle text-end text-nowrap" ]
            [ Format.density material ]
        , td [ class "text-end align-middle text-nowrap" ]
            [ Format.kg <| Component.extractMass itemResults ]
        , td [ class "text-end align-middle text-nowrap" ]
            [ Component.extractImpacts itemResults
                |> Format.formatImpact selectedImpact
            ]
        , td [ class "pe-3 align-middle text-nowrap" ]
            []
        ]


quantityInput : Config db msg -> Component.Id -> Component.Quantity -> Html msg
quantityInput config id quantity =
    div [ class "input-group", style "min-width" "90px", style "max-width" "120px" ]
        [ input
            [ type_ "number"
            , class "form-control text-end"
            , quantity |> Component.quantityToInt |> String.fromInt |> value
            , step "1"
            , Attr.min "1"
            , onInput <|
                \str ->
                    String.toInt str
                        |> Maybe.andThen
                            (\int ->
                                if int > 0 then
                                    Just int

                                else
                                    Nothing
                            )
                        |> Maybe.map
                            (\nonNullInt ->
                                config.updateItem
                                    { id = id
                                    , quantity = Component.quantityFromInt nonNullInt
                                    }
                            )
                        |> Maybe.withDefault config.noOp
            ]
            []
        ]

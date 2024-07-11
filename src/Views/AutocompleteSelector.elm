module Views.AutocompleteSelector exposing (Config, view)

import Autocomplete exposing (Autocomplete)
import Autocomplete.View as AutocompleteView
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.Modal as ModalView


type alias Config element msg =
    { autocompleteState : Autocomplete element
    , closeModal : msg
    , footer : List (Html msg)
    , noOp : msg
    , onAutocomplete : Autocomplete.Msg element -> msg
    , onAutocompleteSelect : msg
    , placeholderText : String
    , title : String
    , toCategory : element -> String
    , toDescription : Maybe (element -> String)
    , toLabel : element -> String
    }


view : Config element msg -> Html msg
view ({ autocompleteState, closeModal, footer, noOp, onAutocomplete, onAutocompleteSelect, placeholderText, title } as config) =
    let
        { query, choices, selectedIndex } =
            Autocomplete.viewState autocompleteState

        { inputEvents, choiceEvents } =
            AutocompleteView.events
                { onSelect = onAutocompleteSelect
                , mapHtml = onAutocomplete
                }
    in
    ModalView.view
        { size = ModalView.Large
        , close = closeModal
        , noOp = noOp
        , title = title
        , subTitle = Nothing
        , formAction = Nothing
        , content =
            [ input
                (inputEvents
                    ++ [ type_ "search"
                       , id "element-search"
                       , class "form-control"
                       , autocomplete False
                       , attribute "role" "combobox"
                       , attribute "aria-autocomplete" "list"
                       , attribute "aria-owns" "element-autocomplete-choices"
                       , placeholder placeholderText
                       , value query
                       ]
                )
                []
            , choices
                |> List.indexedMap (renderChoice config choiceEvents selectedIndex)
                |> div [ class "ElementAutocomplete", id "element-autocomplete-choices" ]
            ]
        , footer = footer
        }


renderChoice : Config element msg -> (Int -> List (Attribute msg)) -> Maybe Int -> Int -> element -> Html msg
renderChoice { toCategory, toDescription, toLabel } events selectedIndex index element =
    let
        selected =
            Autocomplete.isSelected selectedIndex index
    in
    button
        (events index
            ++ [ class "AutocompleteChoice w-100"
               , class "btn btn-outline-primary border-0 border-bottom text-start no-outline"
               , classList [ ( "btn-primary selected", selected ) ]
               , attribute "role" "option"
               , attribute "aria-selected"
                    (if selected then
                        "true"

                     else
                        "false"
                    )
               ]
        )
        [ div [ class "d-flex justify-content-between align-items-center gap-1" ]
            [ span [ class "text-nowrap" ] [ text <| toLabel element ]
            , span [ class "text-muted fs-8 text-truncate" ]
                [ text <| toCategory element ]
            ]
        , case toDescription of
            Just fn ->
                small [ class "text-muted fs-8 text-truncate" ]
                    [ text (fn element) ]

            Nothing ->
                text ""
        ]

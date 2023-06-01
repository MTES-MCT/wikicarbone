module Views.Table exposing
    ( percentageTable
    , responsiveDefault
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Views.Format as Format


responsiveDefault : List (Attribute msg) -> List (Html msg) -> Html msg
responsiveDefault attrs content =
    div [ class "DatasetTable table-responsive" ]
        [ table
            (class "table table-striped table-hover table-responsive mb-0"
                :: attrs
            )
            content
        ]


percentageTable : List ( String, Float ) -> Html msg
percentageTable data =
    let
        values =
            List.map Tuple.second data

        ( total, maximum ) =
            ( List.sum values
            , values |> List.maximum |> Maybe.withDefault 0
            )
    in
    if total == 0 || maximum == 0 then
        text ""

    else
        table [ class "table w-100 m-0" ]
            [ data
                |> List.map
                    (\( name, value ) ->
                        { name = name
                        , percent = value / total * 100
                        , width = value / maximum * 100
                        }
                    )
                |> List.map
                    (\{ name, percent, width } ->
                        tr []
                            [ th [ class "text-truncate fw-normal fs-8", style "max-width" "200px" ] [ text name ]
                            , td [ style "width" "200px", style "vertical-align" "middle" ]
                                [ div [ class "progress bg-white", style "width" "100%", style "height" "13px" ]
                                    [ div
                                        [ class "progress-bar bg-secondary"
                                        , style "width" (String.fromFloat width ++ "%")
                                        ]
                                        []
                                    ]
                                ]
                            , td [ class "text-end fs-8" ]
                                [ Format.percent percent
                                ]
                            ]
                    )
                |> tbody []
            ]

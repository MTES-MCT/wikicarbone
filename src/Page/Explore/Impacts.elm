module Page.Explore.Impacts exposing (..)

import Data.Impact as Impact exposing (Definition)
import Data.Unit as Unit
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.Format as Format
import Views.Table as Table


view : List Definition -> Html msg
view impacts =
    Table.responsiveDefault []
        [ thead []
            [ tr []
                [ th [] [ text "Trigramme" ]
                , th [] [ text "Nom" ]
                , th [] [ text "Unité" ]
                , th [] [ text "Coéf. normalisation PEF" ]
                , th [] [ text "Pondération PEF" ]
                ]
            ]
        , impacts
            |> List.map row
            |> tbody []
        ]


row : Definition -> Html msg
row def =
    tr []
        [ td [] [ code [] [ text (Impact.toString def.trigram) ] ]
        , td [] [ text def.label ]
        , td [] [ text def.unit ]
        , td []
            [ def.pefData
                |> Maybe.map (.normalization >> Unit.impactToFloat >> Format.formatRichFloat 2 def.unit)
                |> Maybe.withDefault (text "N/A")
            ]
        , td []
            [ def.pefData
                |> Maybe.map (.weighting >> Format.ratio)
                |> Maybe.withDefault (text "N/A")
            ]
        ]

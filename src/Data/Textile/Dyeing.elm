module Data.Textile.Dyeing exposing
    ( ProcessType(..)
    , decode
    , encode
    , fromString
    , toProcess
    , toString
    )

import Data.Process exposing (Process)
import Data.Textile.WellKnown exposing (WellKnown)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DE
import Json.Encode as Encode


type ProcessType
    = Average
    | Continuous
    | Discontinuous


decode : Decoder ProcessType
decode =
    Decode.string
        |> Decode.andThen (fromString >> DE.fromResult)


encode : ProcessType -> Encode.Value
encode =
    toString >> Encode.string


fromString : String -> Result String ProcessType
fromString string =
    case string of
        "average" ->
            Ok Average

        "continuous" ->
            Ok Continuous

        "discontinuous" ->
            Ok Discontinuous

        _ ->
            Err <| "Type de teinture inconnu: " ++ string


toProcess : WellKnown -> Maybe ProcessType -> Process
toProcess { dyeingProcessAverage, dyeingProcessContinuous, dyeingProcessDiscontinuous } processType =
    case processType of
        Just Continuous ->
            dyeingProcessContinuous

        Just Discontinuous ->
            dyeingProcessDiscontinuous

        _ ->
            dyeingProcessAverage


toString : ProcessType -> String
toString processType =
    case processType of
        Average ->
            "average"

        Continuous ->
            "continuous"

        Discontinuous ->
            "discontinuous"

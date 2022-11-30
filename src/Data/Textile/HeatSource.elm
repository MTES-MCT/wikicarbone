module Data.Textile.HeatSource exposing
    ( HeatSource(..)
    , decode
    , encode
    , fromString
    , toLabelWithZone
    , toString
    )

import Data.Zone as Zone exposing (Zone)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DE
import Json.Encode as Encode


type HeatSource
    = Coal
    | Gas
    | HeavyFuel
    | LightFuel


decode : Decoder HeatSource
decode =
    Decode.string
        |> Decode.andThen (fromString >> DE.fromResult)


encode : HeatSource -> Encode.Value
encode =
    toString >> Encode.string


fromString : String -> Result String HeatSource
fromString string =
    case string of
        "coal" ->
            Ok Coal

        "gas" ->
            Ok Gas

        "heavyfuel" ->
            Ok HeavyFuel

        "lightfuel" ->
            Ok LightFuel

        _ ->
            Err <| "Source de production de vapeur inconnue: " ++ string


toLabel : HeatSource -> String
toLabel source =
    case source of
        Coal ->
            "Charbon"

        Gas ->
            "Gaz naturel"

        HeavyFuel ->
            "Fioul lourd"

        LightFuel ->
            "Fioul léger"


toLabelWithZone : Zone -> HeatSource -> String
toLabelWithZone zone heatSource =
    let
        zoneLabel =
            case zone of
                Zone.Europe ->
                    " (Europe)"

                _ ->
                    " (hors Europe)"
    in
    toLabel heatSource ++ zoneLabel


toString : HeatSource -> String
toString source =
    case source of
        Coal ->
            "coal"

        Gas ->
            "gas"

        HeavyFuel ->
            "heavyfuel"

        LightFuel ->
            "lightfuel"

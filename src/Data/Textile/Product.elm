module Data.Textile.Product exposing
    ( Id(..)
    , Product
    , customDaysOfWear
    , decodeList
    , encode
    , encodeId
    , findById
    , getMakingDurationInMinutes
    , idToString
    )

import Data.Component as Component
import Data.Process as Process exposing (Process)
import Data.Split as Split exposing (Split)
import Data.Textile.Economics as Economics exposing (Economics)
import Data.Textile.Fabric as Fabric exposing (Fabric)
import Data.Textile.MakingComplexity as MakingComplexity exposing (MakingComplexity)
import Data.Unit as Unit
import Duration exposing (Duration)
import Energy exposing (Energy)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Json.Encode as Encode
import Volume exposing (Volume)


type alias MakingOptions =
    { complexity : MakingComplexity -- How complex is this making
    , pcrWaste : Split -- PCR product waste ratio
    }


type alias UseOptions =
    { daysOfWear : Duration -- Nombre de jour d'utilisation du vêtement (not used in computations)
    , defaultNbCycles : Int -- Nombre par défaut de cycles d'entretien (not used in computations)
    , ironingElec : Energy -- Quantitié d'éléctricité mobilisée pour repasser une pièce
    , nonIroningProcess : Process -- Procédé composite d'utilisation hors-repassage
    , ratioDryer : Split -- Ratio de séchage électrique (not used in computations)
    , ratioIroning : Split -- Ratio de repassage (not used in computations)
    , timeIroning : Duration -- Temps de repassage (not used in computations)
    , wearsPerCycle : Int -- Nombre de jours porté par cycle d'entretien
    }


type alias EndOfLifeOptions =
    { volume : Volume
    }


type alias Product =
    { economics : Economics
    , endOfLife : EndOfLifeOptions
    , fabric : Fabric
    , id : Id
    , making : MakingOptions
    , name : String
    , surfaceMass : Unit.SurfaceMass
    , trims : List Component.Item
    , use : UseOptions
    , yarnSize : Unit.YarnSize
    }


type Id
    = Id String


getMakingDurationInMinutes : Product -> Duration
getMakingDurationInMinutes =
    .making
        >> .complexity
        >> MakingComplexity.toDuration


findById : Id -> List Product -> Result String Product
findById id =
    List.filter (.id >> (==) id)
        >> List.head
        >> Result.fromMaybe ("Produit non trouvé id=" ++ idToString id ++ ".")


idToString : Id -> String
idToString (Id string) =
    string


decodeMakingOptions : Decoder MakingOptions
decodeMakingOptions =
    Decode.succeed MakingOptions
        |> Pipe.required "complexity" MakingComplexity.decode
        |> Pipe.required "pcrWaste" Split.decodeFloat


decodeUseOptions : List Process -> Decoder UseOptions
decodeUseOptions processes =
    Decode.succeed UseOptions
        |> Pipe.required "daysOfWear" (Decode.map Duration.days Decode.float)
        |> Pipe.required "defaultNbCycles" Decode.int
        |> Pipe.required "ironingElecInMJ" (Decode.map Energy.megajoules Decode.float)
        |> Pipe.required "nonIroningProcessUuid" (Process.decodeFromId processes)
        |> Pipe.required "ratioDryer" Split.decodeFloat
        |> Pipe.required "ratioIroning" Split.decodeFloat
        |> Pipe.required "timeIroning" (Decode.map Duration.hours Decode.float)
        |> Pipe.required "wearsPerCycle" Decode.int


decodeEndOfLifeOptions : Decoder EndOfLifeOptions
decodeEndOfLifeOptions =
    Decode.succeed EndOfLifeOptions
        |> Pipe.required "volume" (Decode.map Volume.cubicMeters Decode.float)


decode : List Process -> Decoder Product
decode processes =
    Decode.succeed Product
        |> Pipe.required "economics" Economics.decode
        |> Pipe.required "endOfLife" decodeEndOfLifeOptions
        |> Pipe.required "fabric" Fabric.decode
        |> Pipe.required "id" (Decode.map Id Decode.string)
        |> Pipe.required "making" decodeMakingOptions
        |> Pipe.required "name" Decode.string
        |> Pipe.required "surfaceMass" Unit.decodeSurfaceMass
        |> Pipe.required "trims" (Decode.list Component.decodeItem)
        |> Pipe.required "use" (decodeUseOptions processes)
        |> Pipe.required "yarnSize" Unit.decodeYarnSize


decodeList : List Process -> Decoder (List Product)
decodeList processes =
    Decode.list (decode processes)


encodeMakingOptions : MakingOptions -> Encode.Value
encodeMakingOptions v =
    Encode.object
        [ ( "pcrWaste", Split.encodeFloat v.pcrWaste )
        , ( "complexity", Encode.string (MakingComplexity.toString v.complexity) )
        ]


encodeUseOptions : UseOptions -> Encode.Value
encodeUseOptions v =
    Encode.object
        [ ( "nonIroningProcessUuid", Process.encodeId v.nonIroningProcess.id )
        , ( "wearsPerCycle", Encode.int v.wearsPerCycle )
        , ( "defaultNbCycles", Encode.int v.defaultNbCycles )
        , ( "ratioDryer", Split.encodeFloat v.ratioDryer )
        , ( "ratioIroning", Split.encodeFloat v.ratioIroning )
        , ( "timeIroning", Encode.float (Duration.inHours v.timeIroning) )
        , ( "daysOfWear", Encode.float (Duration.inDays v.daysOfWear) )
        ]


encodeEndOfLifeOptions : EndOfLifeOptions -> Encode.Value
encodeEndOfLifeOptions v =
    Encode.object
        [ ( "volume", v.volume |> Volume.inCubicMeters |> Encode.float ) ]


encode : Product -> Encode.Value
encode v =
    Encode.object
        [ ( "id", encodeId v.id )
        , ( "name", Encode.string v.name )
        , ( "fabric", Fabric.encode v.fabric )
        , ( "making", encodeMakingOptions v.making )
        , ( "trims", Encode.list Component.encodeItem v.trims )
        , ( "use", encodeUseOptions v.use )
        , ( "endOfLife", encodeEndOfLifeOptions v.endOfLife )
        ]


encodeId : Id -> Encode.Value
encodeId =
    idToString >> Encode.string


{-| Computes the number of maintainance cycles.
-}
customDaysOfWear : { productOptions | daysOfWear : Duration, wearsPerCycle : Int } -> Int
customDaysOfWear { daysOfWear, wearsPerCycle } =
    Duration.inDays daysOfWear
        / toFloat (clamp 1 wearsPerCycle wearsPerCycle)
        |> round

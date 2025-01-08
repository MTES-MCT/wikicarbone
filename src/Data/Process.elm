module Data.Process exposing
    ( Id
    , Process
    , decodeFromId
    , decodeId
    , decodeList
    , encode
    , encodeId
    , findByAlias
    , findById
    , getDisplayName
    , getImpact
    , idFromString
    , idToString
    , listByCategory
    , sourceIdToString
    )

import Data.Common.DecodeUtils as DU
import Data.Impact as Impact exposing (Impacts)
import Data.Impact.Definition as Definition
import Data.Process.Category as Category exposing (Category)
import Data.Split as Split exposing (Split)
import Data.Unit as Unit
import Data.Uuid as Uuid exposing (Uuid)
import Energy exposing (Energy)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DE
import Json.Decode.Pipeline as Pipe
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra


type Id
    = Id Uuid


{-| A process is an entry from processes.json or processes\_impacts.json.
-}
type alias Process =
    { alias : Maybe String
    , categories : List Category
    , comment : String
    , density : Float
    , displayName : Maybe String
    , elec : Energy
    , heat : Energy
    , id : Id
    , impacts : Impacts
    , name : String
    , source : String
    , sourceId : Maybe SourceId
    , unit : String
    , waste : Split
    }


type SourceId
    = SourceId String


decodeFromId : List Process -> Decoder Process
decodeFromId processes =
    Uuid.decoder
        |> Decode.andThen (Id >> (\id -> findById id processes) >> DE.fromResult)


getImpact : Definition.Trigram -> Process -> Unit.Impact
getImpact trigram =
    .impacts >> Impact.getImpact trigram


sourceIdFromString : String -> SourceId
sourceIdFromString =
    SourceId


sourceIdToString : SourceId -> String
sourceIdToString (SourceId string) =
    string


decodeProcess : Decoder Impact.Impacts -> Decoder Process
decodeProcess impactsDecoder =
    Decode.succeed Process
        |> DU.strictOptional "alias" Decode.string
        |> Pipe.required "categories" Category.decodeList
        |> Pipe.required "comment" Decode.string
        |> Pipe.required "density" Decode.float
        |> DU.strictOptional "displayName" Decode.string
        |> Pipe.required "elec_MJ" (Decode.map Energy.megajoules Decode.float)
        |> Pipe.required "heat_MJ" (Decode.map Energy.megajoules Decode.float)
        |> Pipe.required "id" decodeId
        |> Pipe.required "impacts" impactsDecoder
        |> Pipe.required "name" Decode.string
        |> Pipe.required "source" Decode.string
        |> DU.strictOptional "sourceId" decodeSourceId
        |> Pipe.required "unit" Decode.string
        |> Pipe.required "waste" Split.decodeFloat


encode : Process -> Encode.Value
encode process =
    Encode.object
        [ ( "alias", EncodeExtra.maybe Encode.string process.alias )
        , ( "categories", Encode.list Category.encode process.categories )
        , ( "comment", Encode.string process.comment )
        , ( "density", Encode.float process.density )
        , ( "displayName", EncodeExtra.maybe Encode.string process.displayName )
        , ( "elec_MJ", Encode.float (Energy.inMegajoules process.elec) )
        , ( "heat_MJ", Encode.float (Energy.inMegajoules process.heat) )
        , ( "id", encodeId process.id )
        , ( "impacts", Impact.encode process.impacts )
        , ( "name", Encode.string process.name )
        , ( "source", Encode.string process.source )
        , ( "sourceId", EncodeExtra.maybe encodeSourceId process.sourceId )
        , ( "unit", Encode.string process.unit )
        , ( "waste", Split.encodeFloat process.waste )
        ]


decodeId : Decoder Id
decodeId =
    Decode.map Id Uuid.decoder


decodeSourceId : Decoder SourceId
decodeSourceId =
    Decode.string
        |> Decode.map sourceIdFromString


decodeList : Decoder Impact.Impacts -> Decoder (List Process)
decodeList =
    decodeProcess >> Decode.list


encodeId : Id -> Encode.Value
encodeId (Id uuid) =
    Uuid.encoder uuid


encodeSourceId : SourceId -> Encode.Value
encodeSourceId =
    sourceIdToString >> Encode.string


idFromString : String -> Maybe Id
idFromString str =
    Uuid.fromString str |> Maybe.map Id


idToString : Id -> String
idToString (Id uuid) =
    Uuid.toString uuid


findByAlias : String -> List Process -> Result String Process
findByAlias alias_ processes =
    processes
        |> List.filter (.alias >> (==) (Just alias_))
        |> List.head
        |> Result.fromMaybe ("Procédé introuvable par alias : " ++ alias_)


findById : Id -> List Process -> Result String Process
findById id processes =
    processes
        |> List.filter (.id >> (==) id)
        |> List.head
        |> Result.fromMaybe ("Procédé introuvable par id : " ++ idToString id)


getDisplayName : Process -> String
getDisplayName { displayName, name } =
    Maybe.withDefault name displayName


listByCategory : Category -> List Process -> List Process
listByCategory category =
    List.filter (.categories >> List.member category)
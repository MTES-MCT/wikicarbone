module Data.Bookmark exposing
    ( Bookmark
    , Query(..)
    , decode
    , encode
    , findByFoodQuery
    , findByTextileQuery
    , isFood
    , isTextile
    , sort
    , toId
    , toQueryDescription
    )

import Data.Food.Query as FoodQuery
import Data.Food.Recipe as Recipe
import Data.Scope as Scope exposing (Scope)
import Data.Textile.Inputs as TextileQuery
import Data.Textile.Simulator as TextileSimulator
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Static.Db exposing (Db)
import Time exposing (Posix)


type alias Bookmark =
    { name : String
    , created : Posix
    , query : Query
    }


type Query
    = Food FoodQuery.Query
    | Textile TextileSimulator.Simulator


decode : Db -> Decoder Bookmark
decode textileDb =
    Decode.map3 Bookmark
        (Decode.field "name" Decode.string)
        (Decode.field "created" (Decode.map Time.millisToPosix Decode.int))
        (Decode.field "query" (decodeQuery textileDb))


decodeQuery : Db -> Decoder Query
decodeQuery textileDb =
    Decode.oneOf
        [ Decode.map Food FoodQuery.decode
        , Decode.map Textile (TextileSimulator.decode textileDb)
        ]


encode : Bookmark -> Encode.Value
encode v =
    Encode.object
        [ ( "name", Encode.string v.name )
        , ( "created", Encode.int <| Time.posixToMillis v.created )
        , ( "query", encodeQuery v.query )
        ]


encodeQuery : Query -> Encode.Value
encodeQuery v =
    case v of
        Food query ->
            FoodQuery.encode query

        Textile query ->
            TextileSimulator.encode query


isFood : Bookmark -> Bool
isFood { query } =
    case query of
        Food _ ->
            True

        _ ->
            False


isTextile : Bookmark -> Bool
isTextile { query } =
    case query of
        Textile _ ->
            True

        _ ->
            False


findByFoodQuery : FoodQuery.Query -> List Bookmark -> Maybe Bookmark
findByFoodQuery foodQuery =
    List.filter (.query >> (==) (Food foodQuery))
        >> List.head


findByTextileQuery : TextileQuery.Query -> List Bookmark -> Maybe Bookmark
findByTextileQuery textileQuery bookmarks =
    bookmarks
        |> List.filter
            (\bookmark ->
                case bookmark.query of
                    Textile simulator ->
                        simulator
                            |> .inputs
                            |> TextileQuery.toQuery
                            |> (==) textileQuery

                    _ ->
                        False
            )
        |> List.head


scope : Bookmark -> Scope
scope bookmark =
    case bookmark.query of
        Food _ ->
            Scope.Food

        Textile _ ->
            Scope.Textile


sort : List Bookmark -> List Bookmark
sort =
    List.sortBy (.created >> Time.posixToMillis) >> List.reverse


toId : Bookmark -> String
toId bookmark =
    Scope.toString (scope bookmark) ++ ":" ++ bookmark.name


toQueryDescription : Db -> Bookmark -> String
toQueryDescription db bookmark =
    case bookmark.query of
        Food foodQuery ->
            foodQuery
                |> Recipe.fromQuery db
                |> Result.map Recipe.toString
                |> Result.withDefault bookmark.name

        Textile simulator ->
            simulator
                |> .inputs
                |> TextileQuery.toString

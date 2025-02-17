module Data.Bookmark exposing
    ( Bookmark
    , Query(..)
    , decode
    , encode
    , findByFoodQuery
    , findByObjectQuery
    , findByTextileQuery
    , isFood
    , isObject
    , isTextile
    , isVeli
    , sort
    , toId
    , toQueryDescription
    )

import Data.Food.Query as FoodQuery
import Data.Food.Recipe as Recipe
import Data.Object.Query as ObjectQuery
import Data.Scope as Scope exposing (Scope)
import Data.Textile.Inputs as Inputs
import Data.Textile.Query as TextileQuery
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as Encode
import Static.Db exposing (Db)
import Time exposing (Posix)


type alias Bookmark =
    { created : Posix
    , name : String
    , query : Query
    , subScope : Maybe Scope
    }


type Query
    = Food FoodQuery.Query
    | Object ObjectQuery.Query
    | Textile TextileQuery.Query
    | Veli ObjectQuery.Query


decode : Decoder Bookmark
decode =
    Decode.succeed Bookmark
        |> JDP.required "created" (Decode.map Time.millisToPosix Decode.int)
        |> JDP.required "name" Decode.string
        |> JDP.required "query" decodeQuery
        |> JDP.optional "subScope" (Decode.maybe Scope.decode) Nothing
        |> Decode.map
            (\bookmark ->
                case ( bookmark.query, bookmark.subScope ) of
                    ( Object q, Just Scope.Veli ) ->
                        { bookmark | query = Veli q }

                    _ ->
                        bookmark
            )


decodeQuery : Decoder Query
decodeQuery =
    Decode.oneOf
        [ Decode.map Food FoodQuery.decode
        , Decode.map Object ObjectQuery.decode
        , Decode.map Textile TextileQuery.decode
        ]


encode : Bookmark -> Encode.Value
encode v =
    Encode.object
        [ ( "created", Encode.int <| Time.posixToMillis v.created )
        , ( "name", Encode.string v.name )
        , ( "query", encodeQuery v.query )
        , ( "subScope"
          , case v.subScope of
                Just Scope.Object ->
                    Scope.encode Scope.Object

                Just Scope.Veli ->
                    Scope.encode Scope.Veli

                _ ->
                    Encode.null
          )
        ]


encodeQuery : Query -> Encode.Value
encodeQuery v =
    case v of
        Food query ->
            FoodQuery.encode query

        Object query ->
            ObjectQuery.encode query

        Textile query ->
            TextileQuery.encode query

        Veli query ->
            ObjectQuery.encode query


isFood : Bookmark -> Bool
isFood { query } =
    case query of
        Food _ ->
            True

        _ ->
            False


isObject : Bookmark -> Bool
isObject { query } =
    case query of
        Object _ ->
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


isVeli : Bookmark -> Bool
isVeli { query } =
    case query of
        Veli _ ->
            True

        _ ->
            False


findByQuery : Query -> List Bookmark -> Maybe Bookmark
findByQuery query =
    List.filter (.query >> (==) query)
        >> List.head


findByFoodQuery : FoodQuery.Query -> List Bookmark -> Maybe Bookmark
findByFoodQuery foodQuery =
    findByQuery (Food foodQuery)


findByObjectQuery : ObjectQuery.Query -> List Bookmark -> Maybe Bookmark
findByObjectQuery objectQuery =
    findByQuery (Object objectQuery)


findByTextileQuery : TextileQuery.Query -> List Bookmark -> Maybe Bookmark
findByTextileQuery textileQuery =
    findByQuery (Textile textileQuery)


scope : Bookmark -> Scope
scope bookmark =
    case bookmark.query of
        Food _ ->
            Scope.Food

        Object _ ->
            Scope.Object

        Textile _ ->
            Scope.Textile

        Veli _ ->
            Scope.Veli


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

        Object objectQuery ->
            objectQuery
                |> ObjectQuery.toString db.components db.processes
                |> Result.withDefault "N/A"

        Textile textileQuery ->
            textileQuery
                |> Inputs.fromQuery db
                |> Result.map (Inputs.toString db.textile.wellKnown)
                |> Result.withDefault bookmark.name

        Veli objectQuery ->
            objectQuery
                |> ObjectQuery.toString db.components db.processes
                |> Result.withDefault "N/A"

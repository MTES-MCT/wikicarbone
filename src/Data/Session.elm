module Data.Session exposing
    ( Auth(..)
    , Notification(..)
    , Session
    , Store
    , authenticated
    , checkComparedSimulations
    , closeNotification
    , deleteBookmark
    , deserializeStore
    , getUser
    , isAuthenticated
    , logout
    , notifyError
    , notifyInfo
    , saveBookmark
    , selectAllBookmarks
    , selectNoBookmarks
    , serializeStore
    , toggleComparedSimulation
    , updateFoodQuery
    , updateTextileQuery
    )

import Browser.Navigation as Nav
import Data.Bookmark as Bookmark exposing (Bookmark)
import Data.Food.Process as FoodProcess
import Data.Food.Query as FoodQuery
import Data.Impact as Impact
import Data.Textile.Process as TextileProcess
import Data.Textile.Query as TextileQuery
import Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as Encode
import Request.Version exposing (Version)
import Set exposing (Set)
import Static.Db as StaticDb exposing (Db)
import Static.Json exposing (RawJsonProcesses, rawJsonProcesses)


type alias Session =
    { db : Db
    , navKey : Nav.Key
    , clientUrl : String
    , enableFoodSection : Bool
    , store : Store
    , currentVersion : Version
    , matomo : { host : String, siteId : String }
    , notifications : List Notification
    , queries :
        { food : FoodQuery.Query
        , textile : TextileQuery.Query
        }
    }



-- Notifications


type Notification
    = GenericError String String
    | GenericInfo String String


closeNotification : Notification -> Session -> Session
closeNotification notification ({ notifications } as session) =
    { session | notifications = notifications |> List.filter ((/=) notification) }


notifyInfo : String -> String -> Session -> Session
notifyInfo title info ({ notifications } as session) =
    { session | notifications = notifications ++ [ GenericInfo title info ] }


notifyError : String -> String -> Session -> Session
notifyError title error ({ notifications } as session) =
    { session | notifications = notifications ++ [ GenericError title error ] }



-- Boomarks


deleteBookmark : Bookmark -> Session -> Session
deleteBookmark bookmark =
    updateStore
        (\store ->
            { store
                | bookmarks =
                    List.filter ((/=) bookmark) store.bookmarks
            }
        )


saveBookmark : Bookmark -> Session -> Session
saveBookmark bookmark =
    updateStore
        (\store ->
            { store
                | bookmarks =
                    bookmark :: store.bookmarks
            }
        )



-- Queries


updateFoodQuery : FoodQuery.Query -> Session -> Session
updateFoodQuery foodQuery ({ queries } as session) =
    { session | queries = { queries | food = foodQuery } }


updateTextileQuery : TextileQuery.Query -> Session -> Session
updateTextileQuery textileQuery ({ queries } as session) =
    { session | queries = { queries | textile = textileQuery } }



-- Comparator


checkComparedSimulations : Session -> Session
checkComparedSimulations =
    updateStore
        (\({ bookmarks, comparedSimulations } as store) ->
            { store
                | comparedSimulations =
                    if Set.size comparedSimulations == 0 then
                        -- Add max bookmarks to compared sims
                        bookmarks
                            |> Bookmark.sort
                            |> List.map Bookmark.toId
                            |> Set.fromList

                    else
                        -- Purge deleted bookmarks from compared sims
                        comparedSimulations
                            |> Set.filter
                                (\id ->
                                    bookmarks
                                        |> List.map Bookmark.toId
                                        |> List.member id
                                )
            }
        )


toggleComparedSimulation : Bookmark -> Bool -> Session -> Session
toggleComparedSimulation bookmark checked =
    updateStore
        (\store ->
            { store
                | comparedSimulations =
                    if checked then
                        Set.insert (Bookmark.toId bookmark) store.comparedSimulations

                    else
                        Set.remove (Bookmark.toId bookmark) store.comparedSimulations
            }
        )


selectAllBookmarks : Session -> Session
selectAllBookmarks =
    updateStore
        (\store ->
            { store
                | comparedSimulations =
                    store.bookmarks |> List.map Bookmark.toId |> Set.fromList
            }
        )


selectNoBookmarks : Session -> Session
selectNoBookmarks =
    updateStore (\store -> { store | comparedSimulations = Set.empty })



-- Store
--
-- A serializable data structure holding session information you want to share
-- across browser restarts, typically in localStorage.


type alias Store =
    { comparedSimulations : Set String
    , bookmarks : List Bookmark
    , auth : Auth
    }


type Auth
    = NotAuthenticated
    | Authenticated User (List TextileProcess.Process) (List FoodProcess.Process)


defaultStore : Store
defaultStore =
    { comparedSimulations = Set.empty
    , bookmarks = []
    , auth = NotAuthenticated
    }


decodeStore : Decoder Store
decodeStore =
    Decode.succeed Store
        |> JDP.optional "comparedSimulations" (Decode.map Set.fromList (Decode.list Decode.string)) Set.empty
        |> JDP.optional "bookmarks" (Decode.list Bookmark.decode) []
        |> JDP.optional "auth" decodeAuth NotAuthenticated


decodeAuth : Decoder Auth
decodeAuth =
    Decode.succeed Authenticated
        |> JDP.required "user" User.decode
        |> JDP.required "textileProcesses" (TextileProcess.decodeList Impact.decodeImpacts)
        |> JDP.required "foodProcesses" (FoodProcess.decodeList Impact.decodeImpacts)


encodeStore : Store -> Encode.Value
encodeStore store =
    Encode.object
        [ ( "comparedSimulations", store.comparedSimulations |> Set.toList |> Encode.list Encode.string )
        , ( "bookmarks", Encode.list Bookmark.encode store.bookmarks )
        , ( "auth", encodeAuth store.auth )
        ]


encodeAuth : Auth -> Encode.Value
encodeAuth auth =
    case auth of
        NotAuthenticated ->
            Encode.null

        Authenticated user textileProcesses foodProcesses ->
            Encode.object
                [ ( "user", User.encode user )
                , ( "textileProcesses", Encode.list TextileProcess.encode textileProcesses )
                , ( "foodProcesses", Encode.list FoodProcess.encode foodProcesses )
                ]


getUser : Session -> Maybe User
getUser { store } =
    case store.auth of
        Authenticated user _ _ ->
            Just user

        NotAuthenticated ->
            Nothing


deserializeStore : String -> Store
deserializeStore =
    Decode.decodeString decodeStore
        -- FIXME: this should return a `Result String Store` so we could inform
        -- users something went wrong while decoding their data (eg. so they can
        -- report the issue).
        -- Meanwhile, if you ever need to debug JSON decode errors from session
        -- store, uncomment these lines.
        -- >> (\res ->
        --         case res of
        --             Ok r ->
        --                 Ok r
        --             Err err ->
        --                 let
        --                     _ =
        --                         Debug.log "deserializeStore error" err
        --                 in
        --                 Err err
        --    )
        >> Result.withDefault defaultStore


serializeStore : Store -> String
serializeStore =
    encodeStore >> Encode.encode 0


updateStore : (Store -> Store) -> Session -> Session
updateStore update session =
    { session | store = update session.store }


authenticated : Session -> User -> RawJsonProcesses -> Session
authenticated ({ store } as session) user rawJsonProcesses =
    case StaticDb.db rawJsonProcesses of
        Ok db ->
            { session
                | db = db
                , store = { store | auth = Authenticated user db.textile.processes db.food.processes }
            }

        Err err ->
            session
                |> notifyError "Impossible de recharger la db avec les nouveaux procédés" err


logout : Session -> Session
logout ({ store } as session) =
    case StaticDb.db rawJsonProcesses of
        Ok db ->
            { session
                | store = { store | auth = NotAuthenticated }
                , db = db
            }

        Err err ->
            { session | store = { store | auth = NotAuthenticated } }
                |> notifyError "Impossible de recharger la db avec les procédés par défaut" err


isAuthenticated : Session -> Bool
isAuthenticated { store } =
    case store.auth of
        Authenticated _ _ _ ->
            True

        _ ->
            False

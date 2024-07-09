module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Example as Example
import Data.Food.Query as FoodQuery
import Data.Impact as Impact
import Data.Session as Session exposing (Session)
import Data.Textile.Query as TextileQuery
import Html
import Page.Api as Api
import Page.Auth as Auth
import Page.Changelog as Changelog
import Page.Editorial as Editorial
import Page.Explore as Explore
import Page.Food as FoodBuilder
import Page.Home as Home
import Page.Stats as Stats
import Page.Textile as TextileSimulator
import Ports
import RemoteData exposing (WebData)
import Request.Version
import Route
import Static.Db as StaticDb exposing (Db)
import Static.Json as StaticJson
import Url exposing (Url)
import Views.Page as Page


type alias Flags =
    { clientUrl : String
    , enableFoodSection : Bool
    , matomo : { host : String, siteId : String }
    , rawStore : String
    }


type Page
    = ApiPage Api.Model
    | AuthPage Auth.Model
    | LoadingPage
    | ChangelogPage Changelog.Model
    | EditorialPage Editorial.Model
    | ExplorePage Explore.Model
    | FoodBuilderPage FoodBuilder.Model
    | HomePage Home.Model
    | NotFoundPage
    | StatsPage Stats.Model
    | TextileSimulatorPage TextileSimulator.Model


type State
    = Loaded Session Page
    | Errored String


type alias Model =
    { state : State
    , mobileNavigationOpened : Bool

    -- Duplicate the nav key in the model so Parcel's hot module reloading finds it always in the same place.
    , navKey : Nav.Key
    }


type Msg
    = ApiMsg Api.Msg
    | AuthMsg Auth.Msg
    | ChangelogMsg Changelog.Msg
    | CloseMobileNavigation
    | CloseNotification Session.Notification
    | EditorialMsg Editorial.Msg
    | ExploreMsg Explore.Msg
    | FoodBuilderMsg FoodBuilder.Msg
    | HomeMsg Home.Msg
    | LoadUrl String
    | OpenMobileNavigation
    | ReloadPage
    | StatsMsg Stats.Msg
    | StoreChanged String
    | TextileSimulatorMsg TextileSimulator.Msg
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | VersionPoll
    | VersionReceived (WebData String)


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    setRoute url
        ( { state =
                case StaticDb.db StaticJson.rawJsonProcesses of
                    Ok db ->
                        Loaded (setupSession navKey flags db) LoadingPage

                    Err err ->
                        Errored err
          , mobileNavigationOpened = False
          , navKey = navKey
          }
        , Cmd.batch
            [ Ports.appStarted ()
            , Request.Version.loadVersion VersionReceived
            ]
        )


setupSession : Nav.Key -> Flags -> Db -> Session
setupSession navKey flags db =
    let
        store =
            Session.deserializeStore flags.rawStore
    in
    { db =
        case store.auth of
            Session.Authenticated _ textileProcesses foodProcesses ->
                db |> StaticDb.updateProcesses foodProcesses textileProcesses

            Session.NotAuthenticated ->
                db
    , clientUrl = flags.clientUrl
    , enableFoodSection = flags.enableFoodSection
    , navKey = navKey
    , store = store
    , currentVersion = Request.Version.Unknown
    , matomo = flags.matomo
    , notifications = []
    , queries =
        { food = FoodQuery.empty
        , textile =
            db.textile.examples
                |> Example.findByName "Tshirt coton (150g) - Majorant par défaut"
                |> Result.map .query
                |> Result.withDefault TextileQuery.default
        }
    }


setRoute : Url -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
setRoute url ( { state } as model, cmds ) =
    case state of
        Loaded session _ ->
            let
                -- TODO: factor this with `update` internal `toPage`
                toPage page subMsg ( subModel, newSession, subCmds ) =
                    let
                        storeCmd =
                            if session.store /= newSession.store then
                                newSession.store |> Session.serializeStore |> Ports.saveStore

                            else
                                Cmd.none
                    in
                    ( { model | state = Loaded newSession (page subModel) }
                    , Cmd.batch
                        [ cmds
                        , Cmd.map subMsg subCmds
                        , storeCmd
                        ]
                    )
            in
            case Route.fromUrl url of
                Nothing ->
                    ( { model | state = Loaded session NotFoundPage }, Cmd.none )

                Just Route.Home ->
                    Home.init session
                        |> toPage HomePage HomeMsg

                Just Route.Api ->
                    Api.init session
                        |> toPage ApiPage ApiMsg

                Just (Route.Auth data) ->
                    Auth.init session data
                        |> toPage AuthPage AuthMsg

                Just Route.Changelog ->
                    Changelog.init session
                        |> toPage ChangelogPage ChangelogMsg

                Just (Route.Editorial slug) ->
                    Editorial.init slug session
                        |> toPage EditorialPage EditorialMsg

                Just (Route.Explore scope dataset) ->
                    Explore.init scope dataset session
                        |> toPage ExplorePage ExploreMsg

                Just Route.FoodBuilderHome ->
                    FoodBuilder.init session Impact.default Nothing
                        |> toPage FoodBuilderPage FoodBuilderMsg

                Just (Route.FoodBuilder trigram maybeQuery) ->
                    FoodBuilder.init session trigram maybeQuery
                        |> toPage FoodBuilderPage FoodBuilderMsg

                Just (Route.FoodBuilderExample uuid) ->
                    FoodBuilder.initFromExample session uuid
                        |> toPage FoodBuilderPage FoodBuilderMsg

                Just Route.Stats ->
                    Stats.init session
                        |> toPage StatsPage StatsMsg

                Just Route.TextileSimulatorHome ->
                    TextileSimulator.init Impact.default Nothing session
                        |> toPage TextileSimulatorPage TextileSimulatorMsg

                Just (Route.TextileSimulator trigram maybeQuery) ->
                    TextileSimulator.init trigram maybeQuery session
                        |> toPage TextileSimulatorPage TextileSimulatorMsg

                Just (Route.TextileSimulatorExample uuid) ->
                    TextileSimulator.initFromExample session uuid
                        |> toPage TextileSimulatorPage TextileSimulatorMsg

        Errored _ ->
            -- FIXME: Static database decoding error, highly unlikely to ever happen
            ( model, cmds )


update : Msg -> Model -> ( Model, Cmd Msg )
update rawMsg ({ state } as model) =
    case ( state, rawMsg ) of
        ( Loaded session page, msg ) ->
            let
                -- TODO: factor this with `setRoute` internal `toPage`
                toPage toModel toMsg ( newModel, newSession, newCmd ) =
                    let
                        storeCmd =
                            if session.store /= newSession.store then
                                newSession.store |> Session.serializeStore |> Ports.saveStore

                            else
                                Cmd.none
                    in
                    ( { model | state = Loaded newSession (toModel newModel) }
                    , Cmd.map toMsg (Cmd.batch [ newCmd, storeCmd ])
                    )
            in
            case ( msg, page ) of
                -- Pages
                ( HomeMsg homeMsg, HomePage homeModel ) ->
                    Home.update session homeMsg homeModel
                        |> toPage HomePage HomeMsg

                ( ApiMsg apiMsg, ApiPage apiModel ) ->
                    Api.update session apiMsg apiModel
                        |> toPage ApiPage ApiMsg

                ( AuthMsg authMsg, AuthPage authModel ) ->
                    Auth.update session authMsg authModel
                        |> toPage AuthPage AuthMsg

                ( ChangelogMsg changelogMsg, ChangelogPage changelogModel ) ->
                    Changelog.update session changelogMsg changelogModel
                        |> toPage ChangelogPage ChangelogMsg

                ( EditorialMsg editorialMsg, EditorialPage editorialModel ) ->
                    Editorial.update session editorialMsg editorialModel
                        |> toPage EditorialPage EditorialMsg

                ( ExploreMsg examplesMsg, ExplorePage examplesModel ) ->
                    Explore.update session examplesMsg examplesModel
                        |> toPage ExplorePage ExploreMsg

                -- Food
                ( FoodBuilderMsg foodMsg, FoodBuilderPage foodModel ) ->
                    FoodBuilder.update session foodMsg foodModel
                        |> toPage FoodBuilderPage FoodBuilderMsg

                ( TextileSimulatorMsg counterMsg, TextileSimulatorPage counterModel ) ->
                    TextileSimulator.update session counterMsg counterModel
                        |> toPage TextileSimulatorPage TextileSimulatorMsg

                -- Stats
                ( StatsMsg statsMsg, StatsPage statsModel ) ->
                    Stats.update session statsMsg statsModel
                        |> toPage StatsPage StatsMsg

                -- Notifications
                ( CloseNotification notification, currentPage ) ->
                    ( { model
                        | state =
                            currentPage
                                |> Loaded (session |> Session.closeNotification notification)
                      }
                    , Cmd.none
                    )

                -- Store
                ( StoreChanged json, currentPage ) ->
                    ( { model
                        | state =
                            currentPage |> Loaded { session | store = Session.deserializeStore json }
                      }
                    , Cmd.none
                    )

                -- Mobile navigation menu
                ( CloseMobileNavigation, _ ) ->
                    ( { model | mobileNavigationOpened = False }, Cmd.none )

                ( OpenMobileNavigation, _ ) ->
                    ( { model | mobileNavigationOpened = True }, Cmd.none )

                -- Url
                ( LoadUrl url, _ ) ->
                    ( model, Nav.load url )

                ( ReloadPage, _ ) ->
                    ( model, Nav.reloadAndSkipCache )

                ( UrlChanged url, _ ) ->
                    ( { model | mobileNavigationOpened = False }, Cmd.none )
                        |> setRoute url

                ( UrlRequested (Browser.Internal url), _ ) ->
                    ( model, Nav.pushUrl session.navKey (Url.toString url) )

                ( UrlRequested (Browser.External href), _ ) ->
                    ( model, Nav.load href )

                -- Version check
                ( VersionReceived webData, currentPage ) ->
                    ( { model
                        | state =
                            currentPage
                                |> Loaded { session | currentVersion = Request.Version.updateVersion session.currentVersion webData }
                      }
                    , Cmd.none
                    )

                ( VersionPoll, _ ) ->
                    ( model, Request.Version.loadVersion VersionReceived )

                -- Catch-all
                ( _, NotFoundPage ) ->
                    ( { model | state = Loaded session NotFoundPage }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Errored _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { state } =
    Sub.batch
        [ Ports.storeChanged StoreChanged
        , Request.Version.pollVersion VersionPoll
        , case state of
            Loaded _ (HomePage subModel) ->
                Home.subscriptions subModel
                    |> Sub.map HomeMsg

            Loaded _ (ExplorePage subModel) ->
                Explore.subscriptions subModel
                    |> Sub.map ExploreMsg

            Loaded _ (FoodBuilderPage subModel) ->
                FoodBuilder.subscriptions subModel
                    |> Sub.map FoodBuilderMsg

            Loaded _ (TextileSimulatorPage subModel) ->
                TextileSimulator.subscriptions subModel
                    |> Sub.map TextileSimulatorMsg

            _ ->
                Sub.none
        ]


view : Model -> Document Msg
view { state, mobileNavigationOpened } =
    case state of
        Errored error ->
            { title = "Erreur lors du chargement…"
            , body =
                [ Html.p [] [ Html.text <| "Database couldn't be parsed: " ]
                , Html.pre [] [ Html.text error ]
                ]
            }

        Loaded session page ->
            let
                pageConfig =
                    Page.Config session
                        mobileNavigationOpened
                        CloseMobileNavigation
                        OpenMobileNavigation
                        LoadUrl
                        ReloadPage
                        CloseNotification

                mapMsg msg ( title, content ) =
                    ( title, content |> List.map (Html.map msg) )
            in
            case page of
                HomePage homeModel ->
                    Home.view session homeModel
                        |> mapMsg HomeMsg
                        |> Page.frame (pageConfig Page.Home)

                ApiPage examplesModel ->
                    Api.view session examplesModel
                        |> mapMsg ApiMsg
                        |> Page.frame (pageConfig Page.Api)

                AuthPage authModel ->
                    Auth.view session authModel
                        |> mapMsg AuthMsg
                        |> Page.frame (pageConfig Page.Auth)

                ChangelogPage changelogModel ->
                    Changelog.view session changelogModel
                        |> mapMsg ChangelogMsg
                        |> Page.frame (pageConfig Page.Changelog)

                EditorialPage editorialModel ->
                    Editorial.view session editorialModel
                        |> mapMsg EditorialMsg
                        |> Page.frame (pageConfig (Page.Editorial editorialModel.slug))

                ExplorePage examplesModel ->
                    Explore.view session examplesModel
                        |> mapMsg ExploreMsg
                        |> Page.frame (pageConfig Page.Explore)

                FoodBuilderPage foodModel ->
                    FoodBuilder.view session foodModel
                        |> mapMsg FoodBuilderMsg
                        |> Page.frame (pageConfig Page.FoodBuilder)

                TextileSimulatorPage simulatorModel ->
                    TextileSimulator.view session simulatorModel
                        |> mapMsg TextileSimulatorMsg
                        |> Page.frame (pageConfig Page.TextileSimulator)

                StatsPage statsModel ->
                    Stats.view session statsModel
                        |> mapMsg StatsMsg
                        |> Page.frame (pageConfig Page.Stats)

                NotFoundPage ->
                    ( "Page manquante", [ Page.notFound ] )
                        |> Page.frame (pageConfig Page.Other)

                LoadingPage ->
                    ( "Chargement…", [ Page.loading ] )
                        |> Page.frame (pageConfig Page.Other)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }

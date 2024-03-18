module Page.Explore exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Browser.Navigation as Nav
import Data.Country as Country exposing (Country)
import Data.Dataset as Dataset exposing (Dataset)
import Data.Example as Example exposing (Example)
import Data.Food.Ingredient as Ingredient exposing (Ingredient)
import Data.Food.Process as FoodProcess
import Data.Food.Query as FoodQuery
import Data.Github as Github
import Data.Impact.Definition as Definition exposing (Definition, Definitions)
import Data.Key as Key
import Data.Scope as Scope exposing (Scope)
import Data.Session as Session exposing (Session)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Process as Process
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Query as TextileQuery
import Data.Uuid exposing (Uuid)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Explore.Countries as ExploreCountries
import Page.Explore.FoodExamples as FoodExamples
import Page.Explore.FoodIngredients as FoodIngredients
import Page.Explore.FoodProcesses as FoodProcesses
import Page.Explore.Impacts as ExploreImpacts
import Page.Explore.Table as Table
import Page.Explore.TextileExamples as TextileExamples
import Page.Explore.TextileMaterials as TextileMaterials
import Page.Explore.TextileProcesses as TextileProcesses
import Page.Explore.TextileProducts as TextileProducts
import Ports
import RemoteData exposing (WebData)
import Request.Common
import Request.Github as GithubApi
import Route exposing (Route)
import Static.Db exposing (Db)
import Table as SortableTable
import Views.Alert as Alert
import Views.Container as Container
import Views.Modal as ModalView


type alias Model =
    { dataset : Dataset
    , pullRequest : WebData Github.PullRequest
    , scope : Scope
    , tableState : SortableTable.State
    }


type Msg
    = NoOp
    | CloseModal
    | SendFoodExamplesPr
    | SentFoodExamplesPr (WebData Github.PullRequest)
    | SendTextileExamplesPr
    | SentTextileExamplesPr (WebData Github.PullRequest)
    | OpenDetail Route
    | ScopeChange Scope
    | SetTableState SortableTable.State


init : Scope -> Dataset -> Session -> ( Model, Session, Cmd Msg )
init scope dataset session =
    let
        initialSort =
            case dataset of
                Dataset.Countries _ ->
                    "Nom"

                Dataset.Impacts _ ->
                    "Code"

                Dataset.FoodExamples _ ->
                    "Coût Environnemental"

                Dataset.FoodIngredients _ ->
                    "Identifiant"

                Dataset.FoodProcesses _ ->
                    "Nom"

                Dataset.TextileExamples _ ->
                    "Coût Environnemental"

                Dataset.TextileProducts _ ->
                    "Identifiant"

                Dataset.TextileMaterials _ ->
                    "Identifiant"

                Dataset.TextileProcesses _ ->
                    "Nom"
    in
    ( { dataset = dataset
      , pullRequest = RemoteData.NotAsked
      , scope = scope
      , tableState = SortableTable.initialSort initialSort
      }
    , session
    , Ports.scrollTo { x = 0, y = 0 }
    )


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            ( model, session, Cmd.none )

        SendFoodExamplesPr ->
            ( { model | pullRequest = RemoteData.Loading }
            , session
            , GithubApi.createFoodExamplesPR session SentFoodExamplesPr
            )

        SentFoodExamplesPr ((RemoteData.Failure error) as state) ->
            ( { model | pullRequest = state }
            , session
                |> Session.notifyError "Erreur serveur" (Request.Common.errorToString error)
            , Cmd.none
            )

        SentFoodExamplesPr ((RemoteData.Success { html_url }) as state) ->
            ( { model | pullRequest = state }
            , session
            , Nav.load html_url
            )

        SentFoodExamplesPr state ->
            ( { model | pullRequest = state }, session, Cmd.none )

        SendTextileExamplesPr ->
            ( { model | pullRequest = RemoteData.Loading }
            , session
            , GithubApi.createTextileExamplesPR session SentTextileExamplesPr
            )

        SentTextileExamplesPr ((RemoteData.Failure error) as state) ->
            ( { model | pullRequest = state }
            , session
                |> Session.notifyError "Erreur serveur" (Request.Common.errorToString error)
            , Cmd.none
            )

        SentTextileExamplesPr ((RemoteData.Success { html_url }) as state) ->
            ( { model | pullRequest = state }
            , session
            , Nav.load html_url
            )

        SentTextileExamplesPr state ->
            ( { model | pullRequest = state }, session, Cmd.none )

        CloseModal ->
            ( model
            , session
            , model.dataset
                |> Dataset.reset
                |> Route.Explore model.scope
                |> Route.toString
                |> Nav.pushUrl session.navKey
            )

        OpenDetail route ->
            ( model
            , session
            , route
                |> Route.toString
                |> Nav.pushUrl session.navKey
            )

        ScopeChange scope ->
            ( { model | scope = scope }
            , session
            , (case model.dataset of
                -- Try selecting the most appropriate tab when switching scope.
                Dataset.Countries _ ->
                    Dataset.Countries Nothing

                Dataset.Impacts _ ->
                    Dataset.Impacts Nothing

                _ ->
                    case scope of
                        Scope.Food ->
                            Dataset.FoodExamples Nothing

                        Scope.Textile ->
                            Dataset.TextileExamples Nothing
              )
                |> Route.Explore scope
                |> Route.toString
                |> Nav.pushUrl session.navKey
            )

        SetTableState tableState ->
            ( { model | tableState = tableState }
            , session
            , Cmd.none
            )


datasetsMenuView : Model -> Html Msg
datasetsMenuView { scope, dataset } =
    Dataset.datasets scope
        |> List.map
            (\ds ->
                a
                    [ class "TabsTab nav-link"
                    , classList [ ( "active", Dataset.same ds dataset ) ]
                    , Route.href (Route.Explore scope ds)
                    ]
                    [ text (Dataset.label ds) ]
            )
        |> nav
            [ class "Tabs nav nav-tabs d-flex justify-content-end align-items-center gap-0 gap-sm-2"
            ]


scopesMenuView : Model -> Html Msg
scopesMenuView model =
    [ Scope.Food, Scope.Textile ]
        |> List.map
            (\scope ->
                label []
                    [ input
                        [ class "form-check-input ms-1 ms-sm-3 me-1"
                        , type_ "radio"
                        , classList [ ( "active", model.scope == scope ) ]
                        , checked <| model.scope == scope
                        , onCheck (always (ScopeChange scope))
                        ]
                        []
                    , text (Scope.toLabel scope)
                    ]
            )
        |> (::) (strong [ class "d-block d-sm-inline" ] [ text "Secteur d'activité" ])
        |> nav
            []


detailsModal : Html Msg -> Html Msg
detailsModal content =
    ModalView.view
        { size = ModalView.Large
        , close = CloseModal
        , noOp = NoOp
        , title = "Détail de l'enregistrement"
        , subTitle = Nothing
        , formAction = Nothing
        , content = [ content ]
        , footer = []
        }


alert : String -> Html Msg
alert error =
    div [ class "p-3 pb-0" ]
        [ Alert.simple
            { level = Alert.Danger
            , content = [ text error ]
            , title = Just "Erreur"
            , close = Nothing
            }
        ]


countriesExplorer :
    Db
    -> Table.Config Country Msg
    -> SortableTable.State
    -> Scope
    -> Maybe Country.Code
    -> List (Html Msg)
countriesExplorer { distances, countries } tableConfig tableState scope maybeCode =
    [ countries
        |> List.filter (.scopes >> List.member scope)
        |> Table.viewList OpenDetail tableConfig tableState scope (ExploreCountries.table distances countries)
    , case maybeCode of
        Just code ->
            detailsModal
                (case Country.findByCode code countries of
                    Ok country ->
                        country
                            |> Table.viewDetails scope (ExploreCountries.table distances countries)

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


impactsExplorer :
    Definitions
    -> Table.Config Definition Msg
    -> SortableTable.State
    -> Scope
    -> Maybe Definition.Trigram
    -> List (Html Msg)
impactsExplorer definitions tableConfig tableState scope maybeTrigram =
    [ Definition.toList definitions
        |> List.sortBy (.trigram >> Definition.toString)
        |> Table.viewList OpenDetail tableConfig tableState scope ExploreImpacts.table
    , maybeTrigram
        |> Maybe.map (\trigram -> Definition.get trigram definitions)
        |> Maybe.map (Table.viewDetails scope ExploreImpacts.table)
        |> Maybe.map detailsModal
        |> Maybe.withDefault (text "")
    ]


foodExamplesExplorer :
    Db
    -> Table.Config (Example FoodQuery.Query) Msg
    -> SortableTable.State
    -> Maybe Uuid
    -> List (Html Msg)
foodExamplesExplorer db tableConfig tableState maybeId =
    [ db.food.examples
        |> List.filter (.query >> (/=) FoodQuery.empty)
        |> List.sortBy .name
        |> Table.viewList OpenDetail tableConfig tableState Scope.Food (FoodExamples.table db)
    , case maybeId of
        Just id ->
            detailsModal
                (case Example.findByUuid id db.food.examples of
                    Ok example ->
                        example
                            |> Table.viewDetails Scope.Food (FoodExamples.table db)

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


foodIngredientsExplorer :
    Db
    -> Table.Config Ingredient Msg
    -> SortableTable.State
    -> Maybe Ingredient.Id
    -> List (Html Msg)
foodIngredientsExplorer { food } tableConfig tableState maybeId =
    [ food.ingredients
        |> List.sortBy .name
        |> Table.viewList OpenDetail tableConfig tableState Scope.Food (FoodIngredients.table food)
    , case maybeId of
        Just id ->
            detailsModal
                (case Ingredient.findByID id food.ingredients of
                    Ok ingredient ->
                        ingredient
                            |> Table.viewDetails Scope.Food (FoodIngredients.table food)

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


foodProcessesExplorer :
    Db
    -> Table.Config FoodProcess.Process Msg
    -> SortableTable.State
    -> Maybe FoodProcess.Identifier
    -> List (Html Msg)
foodProcessesExplorer { food } tableConfig tableState maybeId =
    [ food.processes
        |> List.sortBy (.name >> FoodProcess.nameToString)
        |> Table.viewList OpenDetail tableConfig tableState Scope.Food (FoodProcesses.table food)
    , case maybeId of
        Just id ->
            detailsModal
                (case FoodProcess.findByIdentifier id food.processes of
                    Ok process ->
                        process
                            |> Table.viewDetails Scope.Food (FoodProcesses.table food)

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


textileExamplesExplorer :
    Db
    -> Table.Config (Example TextileQuery.Query) Msg
    -> SortableTable.State
    -> Maybe Uuid
    -> List (Html Msg)
textileExamplesExplorer db tableConfig tableState maybeId =
    [ db.textile.examples
        |> List.sortBy .name
        |> Table.viewList OpenDetail tableConfig tableState Scope.Textile (TextileExamples.table db)
    , case maybeId of
        Just id ->
            detailsModal
                (case Example.findByUuid id db.textile.examples of
                    Ok example ->
                        example
                            |> Table.viewDetails Scope.Food (TextileExamples.table db)

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


textileProductsExplorer :
    Db
    -> Table.Config Product Msg
    -> SortableTable.State
    -> Maybe Product.Id
    -> List (Html Msg)
textileProductsExplorer db tableConfig tableState maybeId =
    [ db.textile.products
        |> Table.viewList OpenDetail tableConfig tableState Scope.Textile (TextileProducts.table db)
    , case maybeId of
        Just id ->
            detailsModal
                (case Product.findById id db.textile.products of
                    Ok product ->
                        Table.viewDetails Scope.Textile (TextileProducts.table db) product

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


textileMaterialsExplorer :
    Db
    -> Table.Config Material Msg
    -> SortableTable.State
    -> Maybe Material.Id
    -> List (Html Msg)
textileMaterialsExplorer db tableConfig tableState maybeId =
    [ db.textile.materials
        |> Table.viewList OpenDetail tableConfig tableState Scope.Textile (TextileMaterials.table db)
    , case maybeId of
        Just id ->
            detailsModal
                (case Material.findById id db.textile.materials of
                    Ok material ->
                        material
                            |> Table.viewDetails Scope.Textile (TextileMaterials.table db)

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


textileProcessesExplorer :
    Db
    -> Table.Config Process.Process Msg
    -> SortableTable.State
    -> Maybe Process.Uuid
    -> List (Html Msg)
textileProcessesExplorer { textile } tableConfig tableState maybeId =
    [ textile.processes
        |> Table.viewList OpenDetail tableConfig tableState Scope.Textile TextileProcesses.table
    , case maybeId of
        Just id ->
            detailsModal
                (case Process.findByUuid id textile.processes of
                    Ok process ->
                        process
                            |> Table.viewDetails Scope.Textile TextileProcesses.table

                    Err error ->
                        alert error
                )

        Nothing ->
            text ""
    ]


explore : Session -> Model -> List (Html Msg)
explore { db, initialDb } ({ scope, dataset, tableState } as model) =
    let
        defaultCustomizations =
            SortableTable.defaultCustomizations

        tableConfig =
            { toId = always "" -- Placeholder
            , toMsg = SetTableState
            , columns = []
            , customizations =
                { defaultCustomizations
                    | tableAttrs = [ class "table table-striped table-hover table-responsive mb-0 view-list cursor-pointer" ]
                }
            }
    in
    case dataset of
        Dataset.Countries maybeCode ->
            countriesExplorer db tableConfig tableState scope maybeCode

        Dataset.Impacts maybeTrigram ->
            impactsExplorer db.definitions tableConfig tableState scope maybeTrigram

        Dataset.FoodExamples maybeId ->
            [ div [] (foodExamplesExplorer db tableConfig tableState maybeId)
            , if db /= initialDb then
                pullRequestForm SendFoodExamplesPr model

              else
                text ""
            ]

        Dataset.FoodIngredients maybeId ->
            foodIngredientsExplorer db tableConfig tableState maybeId

        Dataset.FoodProcesses maybeId ->
            foodProcessesExplorer db tableConfig tableState maybeId

        Dataset.TextileExamples maybeId ->
            [ div [] (textileExamplesExplorer db tableConfig tableState maybeId)
            , if db /= initialDb then
                pullRequestForm SendTextileExamplesPr model

              else
                text ""
            ]

        Dataset.TextileMaterials maybeId ->
            textileMaterialsExplorer db tableConfig tableState maybeId

        Dataset.TextileProducts maybeId ->
            textileProductsExplorer db tableConfig tableState maybeId

        Dataset.TextileProcesses maybeId ->
            textileProcessesExplorer db tableConfig tableState maybeId


pullRequestButton : Msg -> WebData Github.PullRequest -> Html Msg
pullRequestButton event pullRequest =
    let
        ( loading, btnClass ) =
            case pullRequest of
                RemoteData.Loading ->
                    ( True, "btn-primary" )

                RemoteData.NotAsked ->
                    ( False, "btn-primary" )

                RemoteData.Failure _ ->
                    ( False, "btn-danger" )

                RemoteData.Success _ ->
                    ( False, "btn-success" )
    in
    div [ class "text-center mt-3" ]
        [ button [ class <| "btn " ++ btnClass, onClick event, disabled loading ]
            [ span [ classList [ ( "spinner-border spinner-border-sm", loading ) ] ] []
            , text "\u{00A0}Proposer mes changements"
            ]
        ]


pullRequestForm : Msg -> Model -> Html Msg
pullRequestForm sendEvent model =
    div [ class "row justify-content-center" ]
        [ div [ class "offset-lg-3" ] []
        , div [ class "col-lg-6" ]
            [ div [ class "card mt-3 m-auto mt-4 mb-3 shadow-sm" ]
                [ div [ class "card-body" ]
                    [ div [ class "d-flex flex-column gap-2" ]
                        [ text "Vous avez mis à jour certains exemples, vous pouvez proposer ces modifications à la communauté."
                        , div [ class "row" ]
                            [ div [ class "col-sm-6" ]
                                [ label [ for "nom" ] [ text "Nom" ]
                                , input
                                    [ type_ "text"
                                    , id "nom"
                                    , class "form-control"
                                    ]
                                    []
                                ]
                            , div [ class "col-sm-6" ]
                                [ label [ for "email" ] [ text "Email" ]
                                , input
                                    [ type_ "email"
                                    , id "email"
                                    , class "form-control"
                                    ]
                                    []
                                ]
                            ]
                        , div []
                            [ label [ for "description" ] [ text "Description" ]
                            , textarea
                                [ id "description"
                                , class "form-control"
                                ]
                                []
                            ]
                        , pullRequestButton sendEvent model.pullRequest
                        ]
                    ]
                ]
            ]
        , div [ class "offset-lg-3" ] []
        ]


view : Session -> Model -> ( String, List (Html Msg) )
view session model =
    ( Dataset.label model.dataset ++ " | Explorer "
    , [ Container.centered [ class "pb-3" ]
            [ div []
                [ h1 [] [ text "Explorateur" ]
                , div [ class "row d-flex align-items-stretch mt-5 mx-0" ]
                    [ div [ class "col-12 col-lg-5 d-flex align-items-center pb-2 pb-lg-0 mb-4 mb-lg-0 border-bottom ps-0 ms-0" ] [ scopesMenuView model ]
                    , div [ class "col-12 col-lg-7 pe-0 me-0" ] [ datasetsMenuView model ]
                    ]
                ]
            , explore session model
                |> div [ class "mt-3" ]
            ]
      ]
    )


subscriptions : Model -> Sub Msg
subscriptions { dataset } =
    if Dataset.isDetailed dataset then
        Browser.Events.onKeyDown (Key.escape CloseModal)

    else
        Sub.none

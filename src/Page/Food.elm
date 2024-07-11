module Page.Food exposing
    ( Model
    , Msg(..)
    , init
    , initFromExample
    , subscriptions
    , update
    , view
    )

import Autocomplete exposing (Autocomplete)
import Browser.Dom as Dom
import Browser.Events as BE
import Browser.Navigation as Navigation
import Data.AutocompleteSelector as AutocompleteSelector
import Data.Bookmark as Bookmark exposing (Bookmark)
import Data.Dataset as Dataset
import Data.Example as Example
import Data.Food.EcosystemicServices as EcosystemicServices
import Data.Food.Ingredient as Ingredient exposing (Ingredient)
import Data.Food.Ingredient.Category as IngredientCategory
import Data.Food.Origin as Origin
import Data.Food.Preparation as Preparation
import Data.Food.Process as Process exposing (Process)
import Data.Food.Query as Query exposing (Query)
import Data.Food.Recipe as Recipe exposing (Recipe)
import Data.Food.Retail as Retail
import Data.Gitbook as Gitbook
import Data.Impact as Impact
import Data.Impact.Definition as Definition exposing (Definition)
import Data.Key as Key
import Data.Scope as Scope
import Data.Session as Session exposing (Session)
import Data.Unit as Unit
import Data.Uuid exposing (Uuid)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Length
import Mass exposing (Mass)
import Ports
import Quantity
import Route
import Static.Db as Db exposing (Db)
import Task
import Time exposing (Posix)
import Views.Alert as Alert
import Views.AutocompleteSelector as AutocompleteSelectorView
import Views.BaseElement as BaseElement
import Views.Bookmark as BookmarkView
import Views.Button as Button
import Views.Comparator as ComparatorView
import Views.ComplementsDetails as ComplementsDetails
import Views.Component.DownArrow as DownArrow
import Views.Component.MassInput as MassInput
import Views.Component.StepsBorder as StepsBorder
import Views.Container as Container
import Views.Example as ExampleView
import Views.Format as Format
import Views.Icon as Icon
import Views.ImpactTabs as ImpactTabs
import Views.Link as Link
import Views.Modal as ModalView
import Views.Sidebar as SidebarView
import Views.Transport as TransportView


type alias Model =
    { impact : Definition
    , initialQuery : Query
    , bookmarkName : String
    , bookmarkTab : BookmarkView.ActiveTab
    , comparisonType : ComparatorView.ComparisonType
    , modal : Modal
    , activeImpactsTab : ImpactTabs.Tab
    }


type Modal
    = NoModal
    | ComparatorModal
    | AddIngredientModal (Maybe Recipe.RecipeIngredient) (Autocomplete Ingredient)
    | SelectExampleModal (Autocomplete Query)


type Msg
    = AddIngredient Ingredient
    | AddPackaging
    | AddPreparation
    | AddTransform
    | AddDistribution
    | CopyToClipBoard String
    | DeleteBookmark Bookmark
    | DeleteIngredient Ingredient.Id
    | DeletePackaging Process.Identifier
    | DeletePreparation Preparation.Id
    | LoadQuery Query
    | NoOp
    | OnAutocompleteExample (Autocomplete.Msg Query)
    | OnAutocompleteIngredient (Autocomplete.Msg Ingredient)
    | OnAutocompleteSelect
    | OnStepClick String
    | OpenComparator
    | Reset
    | ResetTransform
    | ResetDistribution
    | SaveBookmark
    | SaveBookmarkWithTime String Bookmark.Query Posix
    | SelectAllBookmarks
    | SelectNoBookmarks
    | SetModal Modal
    | SwitchBookmarksTab BookmarkView.ActiveTab
    | SwitchComparisonType ComparatorView.ComparisonType
    | SwitchImpact (Result String Definition.Trigram)
    | SwitchImpactsTab ImpactTabs.Tab
    | ToggleComparedSimulation Bookmark Bool
    | UpdateBookmarkName String
    | UpdateEcotoxWeighting (Maybe Unit.Ratio)
    | UpdateIngredient Query.IngredientQuery Query.IngredientQuery
    | UpdatePackaging Process.Identifier Query.ProcessQuery
    | UpdatePreparation Preparation.Id Preparation.Id
    | UpdateTransform Query.ProcessQuery
    | UpdateDistribution String


init : Session -> Definition.Trigram -> Maybe Query -> ( Model, Session, Cmd Msg )
init session trigram maybeQuery =
    let
        impact =
            Definition.get trigram session.db.definitions

        query =
            maybeQuery
                |> Maybe.withDefault session.queries.food
    in
    ( { impact = impact
      , initialQuery = query
      , bookmarkName = query |> findExistingBookmarkName session
      , bookmarkTab = BookmarkView.SaveTab
      , comparisonType =
            if Session.isAuthenticated session then
                ComparatorView.Subscores

            else
                ComparatorView.Steps
      , modal = NoModal
      , activeImpactsTab = ImpactTabs.StepImpactsTab
      }
    , session |> Session.updateFoodQuery query
    , case maybeQuery of
        Nothing ->
            Ports.scrollTo { x = 0, y = 0 }

        Just _ ->
            Cmd.none
    )


initFromExample : Session -> Uuid -> ( Model, Session, Cmd Msg )
initFromExample session uuid =
    let
        example =
            session.db.food.examples
                |> Example.findByUuid uuid

        query =
            example
                |> Result.map .query
                |> Result.withDefault Query.empty
    in
    ( { impact = session.db.definitions |> Definition.get Definition.Ecs
      , initialQuery = query
      , bookmarkName = query |> findExistingBookmarkName session
      , bookmarkTab = BookmarkView.SaveTab
      , comparisonType = ComparatorView.Subscores
      , modal = NoModal
      , activeImpactsTab = ImpactTabs.StepImpactsTab
      }
    , session |> Session.updateFoodQuery query
    , Ports.scrollTo { x = 0, y = 0 }
    )


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update ({ db, queries } as session) msg model =
    let
        query =
            queries.food

        maybeUpdateQuery : (a -> Query) -> Maybe a -> ( Model, Session, Cmd Msg )
        maybeUpdateQuery toQuery maybeThing =
            maybeThing
                |> Maybe.map (\thing -> updateQuery (toQuery thing) ( model, session, Cmd.none ))
                |> Maybe.withDefault ( model, session, Cmd.none )
    in
    case msg of
        AddIngredient ingredient ->
            update session (SetModal NoModal) model
                |> updateQuery (query |> Query.addIngredient (Recipe.ingredientQueryFromIngredient ingredient))

        AddPackaging ->
            let
                firstPackaging =
                    db.food.processes
                        |> Recipe.availablePackagings (List.map .code query.packaging)
                        |> List.sortBy Process.getDisplayName
                        |> List.head
                        |> Maybe.map Recipe.processQueryFromProcess
            in
            firstPackaging
                |> maybeUpdateQuery (\packaging -> Query.addPackaging packaging query)

        AddPreparation ->
            let
                firstPreparation =
                    Preparation.all
                        |> Preparation.unused query.preparation
                        |> List.head
            in
            firstPreparation
                |> maybeUpdateQuery (\{ id } -> Query.addPreparation id query)

        AddTransform ->
            let
                defaultMass =
                    query.ingredients |> List.map .mass |> Quantity.sum

                firstTransform =
                    db.food.processes
                        |> Process.listByCategory Process.Transform
                        |> List.sortBy Process.getDisplayName
                        |> List.head
                        |> Maybe.map
                            (Recipe.processQueryFromProcess
                                >> (\processQuery -> { processQuery | mass = defaultMass })
                            )
            in
            firstTransform
                |> maybeUpdateQuery (\transform -> Query.setTransform transform query)

        AddDistribution ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.setDistribution Retail.ambient query)

        CopyToClipBoard shareableLink ->
            ( model, session, Ports.copyToClipboard shareableLink )

        DeleteBookmark bookmark ->
            updateQuery query
                ( model
                , session |> Session.deleteBookmark bookmark
                , Cmd.none
                )

        DeleteIngredient ingredientId ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.deleteIngredient ingredientId query)

        DeletePackaging code ->
            ( model, session, Cmd.none )
                |> updateQuery (Recipe.deletePackaging code query)

        DeletePreparation id ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.deletePreparation id query)

        LoadQuery queryToLoad ->
            update session (SetModal NoModal) { model | initialQuery = queryToLoad }
                |> updateQuery queryToLoad

        NoOp ->
            ( model, session, Cmd.none )

        OnAutocompleteExample autocompleteMsg ->
            case model.modal of
                SelectExampleModal autocompleteState ->
                    let
                        ( newAutocompleteState, autoCompleteCmd ) =
                            Autocomplete.update autocompleteMsg autocompleteState
                    in
                    ( { model | modal = SelectExampleModal newAutocompleteState }
                    , session
                    , Cmd.map OnAutocompleteExample autoCompleteCmd
                    )

                _ ->
                    ( model, session, Cmd.none )

        OnAutocompleteIngredient autocompleteMsg ->
            case model.modal of
                AddIngredientModal maybeOldIngredient autocompleteState ->
                    let
                        ( newAutocompleteState, autoCompleteCmd ) =
                            Autocomplete.update autocompleteMsg autocompleteState
                    in
                    ( { model | modal = AddIngredientModal maybeOldIngredient newAutocompleteState }
                    , session
                    , Cmd.map OnAutocompleteIngredient autoCompleteCmd
                    )

                _ ->
                    ( model, session, Cmd.none )

        OnAutocompleteSelect ->
            case model.modal of
                AddIngredientModal maybeOldRecipeIngredient autocompleteState ->
                    updateIngredient query model session maybeOldRecipeIngredient autocompleteState

                SelectExampleModal autocompleteState ->
                    ( model, session, Cmd.none )
                        |> selectExample autocompleteState

                _ ->
                    ( model, session, Cmd.none )

        OnStepClick stepId ->
            ( model
            , session
            , Ports.scrollIntoView stepId
            )

        OpenComparator ->
            ( { model | modal = ComparatorModal }
            , session |> Session.checkComparedSimulations
            , Cmd.none
            )

        Reset ->
            ( model, session, Cmd.none )
                |> updateQuery model.initialQuery

        ResetDistribution ->
            ( model, session, Cmd.none )
                |> updateQuery (Recipe.resetDistribution query)

        ResetTransform ->
            ( model, session, Cmd.none )
                |> updateQuery (Recipe.resetTransform query)

        SaveBookmark ->
            ( model
            , session
            , Time.now
                |> Task.perform
                    (SaveBookmarkWithTime model.bookmarkName
                        (Bookmark.Food query)
                    )
            )

        SaveBookmarkWithTime name foodQuery now ->
            ( model
            , session
                |> Session.saveBookmark
                    { name = String.trim name
                    , query = foodQuery
                    , created = now
                    }
            , Cmd.none
            )

        SelectAllBookmarks ->
            ( model, Session.selectAllBookmarks session, Cmd.none )

        SelectNoBookmarks ->
            ( model, Session.selectNoBookmarks session, Cmd.none )

        SetModal NoModal ->
            ( { model | modal = NoModal }
            , session
            , commandsForNoModal model.modal
            )

        SetModal ComparatorModal ->
            ( { model | modal = ComparatorModal }
            , session
            , Ports.addBodyClass "prevent-scrolling"
            )

        SetModal (AddIngredientModal maybeOldIngredient autocomplete) ->
            ( { model | modal = AddIngredientModal maybeOldIngredient autocomplete }
            , session
            , Cmd.batch
                [ Ports.addBodyClass "prevent-scrolling"
                , Dom.focus "element-search"
                    |> Task.attempt (always NoOp)
                ]
            )

        SetModal (SelectExampleModal autocomplete) ->
            ( { model | modal = SelectExampleModal autocomplete }
            , session
            , Cmd.batch
                [ Ports.addBodyClass "prevent-scrolling"
                , Dom.focus "element-search"
                    |> Task.attempt (always NoOp)
                ]
            )

        SwitchBookmarksTab bookmarkTab ->
            ( { model | bookmarkTab = bookmarkTab }
            , session
            , Cmd.none
            )

        SwitchComparisonType displayChoice ->
            ( { model | comparisonType = displayChoice }, session, Cmd.none )

        SwitchImpact (Ok impact) ->
            ( model
            , session
            , Just query
                |> Route.FoodBuilder impact
                |> Route.toString
                |> Navigation.pushUrl session.navKey
            )

        SwitchImpact (Err error) ->
            ( model
            , session |> Session.notifyError "Erreur de sélection d'impact: " error
            , Cmd.none
            )

        SwitchImpactsTab impactsTab ->
            ( { model | activeImpactsTab = impactsTab }
            , session
            , Cmd.none
            )

        ToggleComparedSimulation bookmark checked ->
            ( model
            , session |> Session.toggleComparedSimulation bookmark checked
            , Cmd.none
            )

        UpdateBookmarkName recipeName ->
            ( { model | bookmarkName = recipeName }, session, Cmd.none )

        UpdateDistribution newDistribution ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updateDistribution newDistribution query)

        UpdateEcotoxWeighting (Just ratio) ->
            ( model, { session | db = Db.updateEcotoxWeighting db ratio }, Cmd.none )
                -- triggers recompute
                |> updateQuery query

        UpdateEcotoxWeighting Nothing ->
            ( model, session, Cmd.none )

        UpdateIngredient oldIngredient newIngredient ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updateIngredient oldIngredient.id newIngredient query)

        UpdatePackaging code newPackaging ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updatePackaging code newPackaging query)

        UpdatePreparation oldId newId ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updatePreparation oldId newId query)

        UpdateTransform newTransform ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updateTransform newTransform query)


updateQuery : Query -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
updateQuery query ( model, session, msg ) =
    ( { model | bookmarkName = query |> findExistingBookmarkName session }
    , session |> Session.updateFoodQuery query
    , msg
    )


commandsForNoModal : Modal -> Cmd Msg
commandsForNoModal modal =
    case modal of
        AddIngredientModal maybeOldIngredient _ ->
            Cmd.batch
                [ Ports.removeBodyClass "prevent-scrolling"
                , Dom.focus
                    -- This whole "node to focus" management is happening as a fallback
                    -- if the modal was closed without choosing anything.
                    -- If anything has been chosen, then the focus will be done in `OnAutocompleteSelect`
                    -- and overload any focus being done here.
                    (maybeOldIngredient
                        |> Maybe.map (.ingredient >> .id >> Ingredient.idToString >> (++) "selector-")
                        |> Maybe.withDefault "add-new-element"
                    )
                    |> Task.attempt (always NoOp)
                ]

        SelectExampleModal _ ->
            Cmd.batch
                [ Ports.removeBodyClass "prevent-scrolling"
                , Dom.focus "selector-example"
                    |> Task.attempt (always NoOp)
                ]

        _ ->
            Ports.removeBodyClass "prevent-scrolling"


updateExistingIngredient : Query -> Model -> Session -> Recipe.RecipeIngredient -> Ingredient -> ( Model, Session, Cmd Msg )
updateExistingIngredient query model session oldRecipeIngredient newIngredient =
    -- Update an existing ingredient
    let
        ingredientQuery : Query.IngredientQuery
        ingredientQuery =
            { id = newIngredient.id
            , mass = oldRecipeIngredient.mass
            , country = Nothing
            , planeTransport = Ingredient.byPlaneByDefault newIngredient
            }
    in
    model
        |> update session (SetModal NoModal)
        |> updateQuery (Query.updateIngredient oldRecipeIngredient.ingredient.id ingredientQuery query)
        |> focusNode ("selector-" ++ Ingredient.idToString newIngredient.id)


updateIngredient : Query -> Model -> Session -> Maybe Recipe.RecipeIngredient -> Autocomplete Ingredient -> ( Model, Session, Cmd Msg )
updateIngredient query model session maybeOldRecipeIngredient autocompleteState =
    let
        maybeSelectedValue =
            Autocomplete.selectedValue autocompleteState
    in
    Maybe.map2
        (updateExistingIngredient query model session)
        maybeOldRecipeIngredient
        maybeSelectedValue
        |> Maybe.withDefault
            -- Add a new ingredient
            (model
                |> update session (SetModal NoModal)
                |> selectIngredient autocompleteState
                |> focusNode
                    (maybeSelectedValue
                        |> Maybe.map (\selectedValue -> "selector-" ++ Ingredient.idToString selectedValue.id)
                        |> Maybe.withDefault "add-new-element"
                    )
            )


focusNode : String -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
focusNode node ( model, session, commands ) =
    ( model
    , session
    , Cmd.batch
        [ commands
        , Dom.focus node
            |> Task.attempt (always NoOp)
        ]
    )


findExistingBookmarkName : Session -> Query -> String
findExistingBookmarkName { db, store } query =
    store.bookmarks
        |> Bookmark.findByFoodQuery query
        |> Maybe.map .name
        |> Maybe.withDefault
            (query
                |> Recipe.fromQuery db
                |> Result.map Recipe.toString
                |> Result.withDefault ""
            )


selectExample : Autocomplete Query -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
selectExample autocompleteState ( model, session, _ ) =
    let
        example =
            Autocomplete.selectedValue autocompleteState
                |> Maybe.withDefault Query.empty

        msg =
            LoadQuery example
    in
    update session msg model


selectIngredient : Autocomplete Ingredient -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
selectIngredient autocompleteState ( model, session, _ ) =
    let
        ingredient =
            Autocomplete.selectedValue autocompleteState
                |> Maybe.map Just
                |> Maybe.withDefault (List.head (Autocomplete.choices autocompleteState))

        msg =
            ingredient
                |> Maybe.map AddIngredient
                |> Maybe.withDefault NoOp
    in
    update session msg model



-- Views


type alias AddProcessConfig msg =
    { isDisabled : Bool
    , event : msg
    , kind : String
    }


addProcessFormView : AddProcessConfig Msg -> Html Msg
addProcessFormView { isDisabled, event, kind } =
    li [ class "list-group-item p-0" ]
        [ button
            [ class "btn btn-outline-primary"
            , class "d-flex justify-content-center align-items-center"
            , class "gap-1 w-100"
            , disabled isDisabled
            , onClick event
            ]
            [ i [ class "icon icon-plus" ] []
            , text <| "Ajouter " ++ kind
            ]
        ]


type alias UpdateProcessConfig =
    { processes : List Process
    , excluded : List Process.Identifier
    , processQuery : Query.ProcessQuery
    , impact : Html Msg
    , updateEvent : Query.ProcessQuery -> Msg
    , deleteEvent : Msg
    }


updateProcessFormView : UpdateProcessConfig -> Html Msg
updateProcessFormView { processes, excluded, processQuery, impact, updateEvent, deleteEvent } =
    li [ class "ElementFormWrapper list-group-item" ]
        [ span [ class "QuantityInputWrapper" ]
            [ MassInput.view
                { mass = processQuery.mass
                , onChange =
                    \maybeMass ->
                        case maybeMass of
                            Just mass ->
                                updateEvent { processQuery | mass = mass }

                            _ ->
                                NoOp
                , disabled = False
                }
            ]
        , processes
            |> List.sortBy (.name >> Process.nameToString)
            |> processSelectorView
                processQuery.code
                (\code -> updateEvent { processQuery | code = code })
                excluded
        , span [ class "text-end ImpactDisplay fs-7" ] [ impact ]
        , BaseElement.deleteItemButton { disabled = False } deleteEvent
        ]


type alias UpdateIngredientConfig =
    { excluded : List Ingredient.Id
    , recipeIngredient : Recipe.RecipeIngredient
    , impact : Impact.Impacts
    , selectedImpact : Definition
    , transportImpact : Html Msg
    }


createElementSelectorConfig : Db -> Query.IngredientQuery -> UpdateIngredientConfig -> BaseElement.Config Ingredient Mass Msg
createElementSelectorConfig db ingredientQuery { excluded, recipeIngredient, impact, selectedImpact } =
    let
        baseElement =
            { element = recipeIngredient.ingredient
            , quantity = recipeIngredient.mass
            , country = recipeIngredient.country
            }
    in
    { allowEmptyList = True
    , baseElement = baseElement
    , db =
        { elements = db.food.ingredients
        , countries =
            db.countries
                |> Scope.only Scope.Food
                |> List.sortBy .name
        , definitions = db.definitions
        }
    , defaultCountry = Origin.toLabel recipeIngredient.ingredient.defaultOrigin
    , delete = \element -> DeleteIngredient element.id
    , excluded =
        db.food.ingredients
            |> List.filter (\ingredient -> List.member ingredient.id excluded)
    , impact = impact
    , quantityView =
        \{ quantity, onChange } ->
            MassInput.view { disabled = False, mass = quantity, onChange = onChange }
    , selectedImpact = selectedImpact
    , selectElement =
        \_ autocompleteState ->
            SetModal (AddIngredientModal (Just recipeIngredient) autocompleteState)
    , toId = .id >> Ingredient.idToString
    , toString = .name
    , toTooltip = .default >> .name >> Process.nameToString
    , update =
        \_ newElement ->
            UpdateIngredient
                ingredientQuery
                { ingredientQuery
                    | id = newElement.element.id
                    , mass = newElement.quantity
                    , country = Maybe.map .code newElement.country
                }
    }


updateIngredientFormView : Db -> UpdateIngredientConfig -> Html Msg
updateIngredientFormView db ({ recipeIngredient, selectedImpact, transportImpact } as updateIngredientConfig) =
    let
        ingredientQuery : Query.IngredientQuery
        ingredientQuery =
            { id = recipeIngredient.ingredient.id
            , mass = recipeIngredient.mass
            , country = recipeIngredient.country |> Maybe.map .code
            , planeTransport = recipeIngredient.planeTransport
            }

        event =
            UpdateIngredient ingredientQuery

        config : BaseElement.Config Ingredient Mass Msg
        config =
            createElementSelectorConfig db ingredientQuery updateIngredientConfig
    in
    li [ class "ElementFormWrapper list-group-item" ]
        (BaseElement.view config
            ++ [ if selectedImpact.trigram == Definition.Ecs then
                    let
                        { ingredient } =
                            recipeIngredient

                        complementsImpacts =
                            recipeIngredient.mass
                                |> Recipe.computeIngredientComplementsImpacts ingredient.ecosystemicServices
                    in
                    [ { name = EcosystemicServices.labels.hedges
                      , computedImpact = complementsImpacts.hedges
                      }
                    , { name = EcosystemicServices.labels.plotSize
                      , computedImpact = complementsImpacts.plotSize
                      }
                    , { name = EcosystemicServices.labels.cropDiversity
                      , computedImpact = complementsImpacts.cropDiversity
                      }
                    , { name = EcosystemicServices.labels.permanentPasture
                      , computedImpact = complementsImpacts.permanentPasture
                      }
                    , { name = EcosystemicServices.labels.livestockDensity
                      , computedImpact = complementsImpacts.livestockDensity
                      }
                    ]
                        |> List.map
                            (\{ name, computedImpact } ->
                                div
                                    [ class "ElementComplement"
                                    , title name
                                    ]
                                    [ span [ class "ComplementName d-flex align-items-center text-nowrap text-muted" ]
                                        [ text name
                                        , Button.smallPillLink
                                            [ href (Gitbook.publicUrlFromPath Gitbook.FoodComplements)
                                            , target "_blank"
                                            ]
                                            [ Icon.question ]
                                        ]
                                    , div [ class "ComplementValue d-flex justify-content-end align-items-center text-muted" ]
                                        []
                                    , div [ class "ComplementImpact text-black-50 text-muted text-end" ]
                                        [ text "("
                                        , Format.complement computedImpact
                                        , text ")"
                                        ]
                                    ]
                            )
                        |> ComplementsDetails.view
                            { complementsImpacts = complementsImpacts
                            , label = "Services ecosystémiques"
                            }

                 else
                    text ""
               , displayTransportDistances db recipeIngredient ingredientQuery event
               , span
                    [ class "text-black-50 text-end ElementTransportImpact fs-8"
                    , title "Impact du transport pour cet ingrédient"
                    ]
                    [ text "(+ "
                    , transportImpact
                    , text ")"
                    ]
               ]
        )


displayTransportDistances : Db -> Recipe.RecipeIngredient -> Query.IngredientQuery -> (Query.IngredientQuery -> Msg) -> Html Msg
displayTransportDistances db ingredient ingredientQuery event =
    span [ class "text-muted d-flex fs-7 gap-3 justify-content-left ElementTransportDistances" ]
        (if ingredient.planeTransport /= Ingredient.PlaneNotApplicable then
            let
                isByPlane =
                    ingredientQuery.planeTransport == Ingredient.ByPlane

                { road, roadCooled, air, sea, seaCooled } =
                    ingredient
                        |> Recipe.computeIngredientTransport db

                needsCooling =
                    ingredient.ingredient.transportCooling /= Ingredient.NoCooling
            in
            [ div [ class "IngredientPlaneOrBoatSelector" ]
                [ label [ class "PlaneSelector" ]
                    [ input
                        [ type_ "radio"
                        , attribute "role" "switch"
                        , checked isByPlane
                        , onInput <| always (event { ingredientQuery | planeTransport = Ingredient.ByPlane })
                        ]
                        []
                    , Icon.plane
                    ]
                , label [ class "BoatSelector" ]
                    [ input
                        [ type_ "radio"
                        , attribute "role" "switch"
                        , checked <| not isByPlane
                        , onInput <| always (event { ingredientQuery | planeTransport = Ingredient.NoPlane })
                        ]
                        []
                    , if needsCooling then
                        Icon.boatCooled

                      else
                        Icon.boat
                    ]
                , if isByPlane then
                    span [ class "ps-1 align-items-center gap-1", title "Tranport aérien" ]
                        [ Format.km air ]

                  else if needsCooling then
                    span [ class "ps-1 align-items-center gap-1", title "Tranport maritime réfrigéré" ]
                        [ Format.km seaCooled ]

                  else
                    span [ class "ps-1 align-items-center gap-1", title "Tranport maritime" ]
                        [ Format.km sea ]
                ]
            , if road /= Length.kilometers 0 then
                TransportView.entry { onlyIcons = False, distance = road, icon = Icon.bus, label = "Transport routier" }

              else
                text ""
            , if roadCooled /= Length.kilometers 0 then
                TransportView.entry { onlyIcons = False, distance = roadCooled, icon = Icon.busCooled, label = "Transport routier réfrigéré" }

              else
                text ""
            ]

         else
            ingredient
                |> Recipe.computeIngredientTransport db
                |> TransportView.viewDetails
                    { fullWidth = False
                    , hideNoLength = True
                    , onlyIcons = False
                    , airTransportLabel = Nothing
                    , seaTransportLabel = Nothing
                    , roadTransportLabel = Nothing
                    }
        )


debugQueryView : Db -> Query -> Html Msg
debugQueryView db query =
    let
        debugView =
            text >> List.singleton >> pre []
    in
    details []
        [ summary [] [ text "Debug" ]
        , div [ class "row" ]
            [ div [ class "col-7" ]
                [ query
                    |> Query.serialize
                    |> debugView
                ]
            , div [ class "col-5" ]
                [ query
                    |> Recipe.compute db
                    |> Result.map (Tuple.second >> Recipe.encodeResults >> Encode.encode 2)
                    |> Result.withDefault "Error serializing the impacts"
                    |> debugView
                ]
            ]
        ]


errorView : String -> Html Msg
errorView error =
    Alert.simple
        { level = Alert.Danger
        , content = [ text error ]
        , title = Nothing
        , close = Nothing
        }


ingredientListView : Db -> Definition -> Recipe -> Recipe.Results -> List (Html Msg)
ingredientListView db selectedImpact recipe results =
    let
        availableIngredients =
            db.food.ingredients
                |> Recipe.availableIngredients (List.map (.ingredient >> .id) recipe.ingredients)
                |> List.sortBy .name

        autocompleteState =
            availableIngredients
                |> AutocompleteSelector.init (\{ name, default } -> name ++ " " ++ Process.nameToString default.name)
    in
    [ div
        [ class "card-header d-flex align-items-center justify-content-between"
        , StepsBorder.style Impact.stepsColors.materials
        ]
        [ h2
            [ class "h5 mb-0"
            , id "materials-step"
            ]
            [ text "Ingrédients"
            , Link.smallPillExternal
                [ Route.href (Route.Explore Scope.Food (Dataset.FoodIngredients Nothing))
                , title "Explorer"
                , attribute "aria-label" "Explorer"
                ]
                [ Icon.search ]
            ]
        , span []
            [ results.recipe.ingredientsTotal
                |> Format.formatImpact selectedImpact
            , Button.docsPillLink
                [ class "btn btn-secondary ms-2 py-1"
                , href (Gitbook.publicUrlFromPath Gitbook.FoodIngredients)
                , title "Documentation"
                , target "_blank"
                ]
                [ Icon.question ]
            ]
        ]
    , ul [ class "CardList list-group list-group-flush" ]
        ((if List.isEmpty recipe.ingredients then
            [ li [ class "list-group-item" ] [ text "Aucun ingrédient" ] ]

          else
            recipe.ingredients
                |> List.map
                    (\ingredient ->
                        updateIngredientFormView db
                            { excluded = recipe.ingredients |> List.map (.ingredient >> .id)
                            , recipeIngredient = ingredient
                            , impact =
                                results.recipe.ingredients
                                    |> List.filter (\( recipeIngredient, _ ) -> recipeIngredient == ingredient)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                    |> Maybe.withDefault Impact.empty
                            , selectedImpact = selectedImpact
                            , transportImpact =
                                ingredient
                                    |> Recipe.computeIngredientTransport db
                                    |> .impacts
                                    |> Format.formatImpact selectedImpact
                            }
                    )
         )
            ++ [ li [ class "list-group-item p-0" ]
                    [ button
                        [ class "btn btn-outline-primary"
                        , class "d-flex justify-content-center align-items-center"
                        , class " gap-1 w-100"
                        , id "add-new-element"
                        , disabled <| List.isEmpty availableIngredients
                        , onClick (SetModal (AddIngredientModal Nothing autocompleteState))
                        ]
                        [ i [ class "icon icon-plus" ] []
                        , text "Ajouter un ingrédient"
                        ]
                    ]
               ]
        )
    ]


packagingListView : Db -> Definition -> Recipe -> Recipe.Results -> List (Html Msg)
packagingListView db selectedImpact recipe results =
    let
        availablePackagings =
            db.food.processes
                |> Recipe.availablePackagings
                    (recipe.packaging
                        |> List.map (.process >> .identifier)
                    )
    in
    [ div
        [ class "card-header d-flex align-items-center justify-content-between"
        , StepsBorder.style Impact.stepsColors.packaging
        ]
        [ h2
            [ class "h5 mb-0"
            , id "packaging-step"
            ]
            [ text "Emballage" ]
        , span []
            [ results.packaging
                |> Format.formatImpact selectedImpact
            , Button.docsPillLink
                [ class "btn btn-secondary ms-2 py-1"
                , href (Gitbook.publicUrlFromPath Gitbook.FoodPackaging)
                , title "Documentation"
                , target "_blank"
                ]
                [ Icon.question ]
            ]
        ]
    , ul [ class "CardList list-group list-group-flush" ]
        ((if List.isEmpty recipe.packaging then
            [ li [ class "list-group-item" ] [ text "Aucun emballage" ] ]

          else
            recipe.packaging
                |> List.map
                    (\packaging ->
                        updateProcessFormView
                            { processes =
                                db.food.processes
                                    |> Process.listByCategory Process.Packaging
                            , excluded = recipe.packaging |> List.map (.process >> .identifier)
                            , processQuery = { code = packaging.process.identifier, mass = packaging.mass }
                            , impact =
                                packaging
                                    |> Recipe.computeProcessImpacts
                                    |> Format.formatImpact selectedImpact
                            , updateEvent = UpdatePackaging packaging.process.identifier
                            , deleteEvent = DeletePackaging packaging.process.identifier
                            }
                    )
         )
            ++ [ addProcessFormView
                    { isDisabled = availablePackagings == []
                    , event = AddPackaging
                    , kind = "un emballage"
                    }
               ]
        )
    ]


transportToTransformationView : Definition -> Recipe.Results -> Html Msg
transportToTransformationView selectedImpact results =
    DownArrow.view
        []
        [ div []
            [ text "Masse : "
            , Format.kg results.recipe.initialMass
            ]
        , div [ class "d-flex justify-content-between" ]
            [ div [ class "d-flex justify-content-between gap-3" ]
                (results.recipe.transports
                    |> TransportView.viewDetails
                        { fullWidth = False
                        , hideNoLength = True
                        , onlyIcons = True
                        , airTransportLabel = Nothing
                        , seaTransportLabel = Nothing
                        , roadTransportLabel = Nothing
                        }
                )
            , span []
                [ Format.formatImpact selectedImpact results.recipe.transports.impacts
                , Button.smallPillLink
                    [ href (Gitbook.publicUrlFromPath Gitbook.FoodTransport)
                    , target "_blank"
                    ]
                    [ Icon.question ]
                ]
            ]
        ]


transportToPackagingView : Recipe -> Recipe.Results -> Html Msg
transportToPackagingView recipe results =
    DownArrow.view
        []
        [ div []
            [ text "Masse comestible\u{00A0}: "
            , Format.kg results.recipe.edibleMass
            , Link.smallPillExternal
                [ href (Gitbook.publicUrlFromPath Gitbook.FoodInediblePart)
                , title "Accéder à la documentation"
                , attribute "aria-label" "Accéder à la documentation"
                ]
                [ Icon.question ]
            ]
        , case recipe.transform of
            Just transform ->
                div []
                    [ span
                        [ title <| "(" ++ Process.nameToString transform.process.name ++ ")" ]
                        [ text "Masse après transformation : " ]
                    , Recipe.getTransformedIngredientsMass recipe
                        |> Format.kg
                    , Link.smallPillExternal
                        [ href (Gitbook.publicUrlFromPath Gitbook.FoodRawToCookedRatio)
                        , title "Accéder à la documentation"
                        , attribute "aria-label" "Accéder à la documentation"
                        ]
                        [ Icon.question ]
                    ]

            Nothing ->
                text ""
        ]


transportToDistributionView : Definition -> Recipe -> Recipe.Results -> Html Msg
transportToDistributionView selectedImpact recipe results =
    DownArrow.view
        []
        [ div []
            [ text "Masse : "
            , Recipe.getTransformedIngredientsMass recipe
                |> Format.kg
            , text " + Emballage : "
            , Recipe.getPackagingMass recipe
                |> Format.kg
            ]
        , div [ class "d-flex justify-content-between" ]
            [ div []
                (results.distribution.transports
                    |> TransportView.viewDetails
                        { fullWidth = False
                        , hideNoLength = True
                        , onlyIcons = False
                        , airTransportLabel = Nothing
                        , seaTransportLabel = Nothing
                        , roadTransportLabel = Nothing
                        }
                )
            , span []
                [ Format.formatImpact selectedImpact results.distribution.transports.impacts
                , Button.smallPillLink
                    [ href (Gitbook.publicUrlFromPath Gitbook.FoodTransport)
                    , target "_blank"
                    ]
                    [ Icon.question ]
                ]
            ]
        ]


transportToConsumptionView : Recipe -> Html Msg
transportToConsumptionView recipe =
    DownArrow.view
        []
        [ text <| "Masse : "
        , Recipe.getTransformedIngredientsMass recipe
            |> Format.kg
        , text " + Emballage : "
        , Recipe.getPackagingMass recipe
            |> Format.kg
        ]


transportAfterConsumptionView : Recipe -> Recipe.Results -> Html Msg
transportAfterConsumptionView recipe result =
    DownArrow.view
        []
        [ text <| "Masse : "
        , Format.kg result.preparedMass
        , text " + Emballage : "
        , Recipe.getPackagingMass recipe
            |> Format.kg
        ]


distributionView : Definition -> Recipe -> Recipe.Results -> List (Html Msg)
distributionView selectedImpact recipe results =
    let
        impact =
            results.distribution.total
                |> Format.formatImpact selectedImpact
    in
    [ div
        [ class "card-header d-flex align-items-center justify-content-between"
        , StepsBorder.style Impact.stepsColors.distribution
        ]
        [ h2
            [ class "h5 mb-0"
            , id "distribution-step"
            ]
            [ text "Distribution" ]
        , span []
            [ results.distribution.total
                |> Format.formatImpact selectedImpact
            , Button.docsPillLink
                [ class "btn btn-secondary ms-2 py-1"
                , href (Gitbook.publicUrlFromPath Gitbook.FoodDistribution)
                , title "Documentation"
                , target "_blank"
                ]
                [ Icon.question ]
            ]
        ]
    , ul [ class "CardList list-group list-group-flush border-top-0 border-bottom-0" ]
        (case recipe.distribution of
            Just distribution ->
                [ li [ class "ElementFormWrapper list-group-item" ]
                    [ select
                        [ class "form-select form-select"
                        , onInput UpdateDistribution
                        ]
                        (Retail.all
                            |> List.map
                                (\distrib ->
                                    option
                                        [ selected (recipe.distribution == Just distrib)
                                        , value (Retail.toString distrib)
                                        ]
                                        [ text (Retail.toDisplay distrib) ]
                                )
                        )
                    , span [ class "text-end ImpactDisplay fs-7" ] [ impact ]
                    , BaseElement.deleteItemButton { disabled = False } ResetDistribution
                    ]
                , li
                    [ class "list-group-item fs-7 pt-2" ]
                    [ distribution
                        |> Retail.displayNeeds
                        |> text
                    ]
                ]

            Nothing ->
                [ addProcessFormView
                    { isDisabled = False
                    , event = AddDistribution
                    , kind = "un mode de distribution"
                    }
                ]
        )
    ]


consumptionView : Db -> Definition -> Recipe -> Recipe.Results -> List (Html Msg)
consumptionView db selectedImpact recipe results =
    [ div
        [ class "card-header d-flex align-items-center justify-content-between"
        , StepsBorder.style Impact.stepsColors.usage
        ]
        [ h2
            [ class "h5 mb-0"
            , id "usage-step"
            ]
            [ text "Consommation" ]
        , span []
            [ results.preparation
                |> Format.formatImpact selectedImpact
            , Button.docsPillLink
                [ class "btn btn-secondary ms-2 py-1"
                , href (Gitbook.publicUrlFromPath Gitbook.FoodUse)
                , title "Documentation"
                , target "_blank"
                ]
                [ Icon.question ]
            ]
        ]
    , ul [ class "CardList list-group list-group-flush" ]
        ((if List.isEmpty recipe.preparation then
            [ li [ class "list-group-item" ] [ text "Aucune préparation" ] ]

          else
            recipe.preparation
                |> List.map
                    (\usedPreparation ->
                        li [ class "list-group-item d-flex justify-content-between align-items-center gap-2 pb-3" ]
                            [ Preparation.all
                                |> List.sortBy .name
                                |> List.map
                                    (\{ id, name } ->
                                        option
                                            [ selected <| usedPreparation.id == id
                                            , value <| Preparation.idToString id
                                            , disabled <| List.member id (List.map .id recipe.preparation)
                                            ]
                                            [ text name ]
                                    )
                                |> select
                                    [ class "form-select form-select w-50"
                                    , onInput (Preparation.Id >> UpdatePreparation usedPreparation.id)
                                    ]
                            , span [ class "w-50 text-end" ]
                                [ usedPreparation
                                    |> Preparation.apply db.food.wellKnown results.recipe.transformedMass
                                    |> Format.formatImpact selectedImpact
                                ]
                            , BaseElement.deleteItemButton { disabled = False } (DeletePreparation usedPreparation.id)
                            ]
                    )
         )
            ++ [ addProcessFormView
                    { isDisabled = List.length recipe.preparation == 2
                    , event = AddPreparation
                    , kind = "une technique de préparation"
                    }
               ]
        )
    ]


mainView : Session -> Model -> Html Msg
mainView ({ db } as session) model =
    let
        computed =
            session.queries.food
                |> Recipe.compute db
    in
    div [ class "row gap-3 gap-lg-0" ]
        [ div [ class "col-lg-8 d-flex flex-column gap-3" ]
            [ ExampleView.view
                { currentQuery = session.queries.food
                , emptyQuery = Query.empty
                , examples = db.food.examples
                , helpUrl = Nothing
                , onOpen = SelectExampleModal >> SetModal
                , routes =
                    { explore = Route.Explore Scope.Food (Dataset.FoodExamples Nothing)
                    , load = Route.FoodBuilderExample
                    , scopeHome = Route.FoodBuilderHome
                    }
                }
            , case computed of
                Ok ( recipe, results ) ->
                    stepListView db session model recipe results

                Err error ->
                    errorView error
            , session.queries.food
                |> debugQueryView db
            ]
        , div [ class "col-lg-4 d-flex flex-column gap-3" ]
            [ case computed of
                Ok ( _, results ) ->
                    sidebarView session model results

                Err error ->
                    errorView error
            ]
        ]


processSelectorView :
    Process.Identifier
    -> (Process.Identifier -> msg)
    -> List Process.Identifier
    -> List Process
    -> Html msg
processSelectorView selectedCode event excluded processes =
    select
        [ class "form-select form-select"
        , onInput (Process.identifierFromString >> event)
        ]
        (processes
            |> List.sortBy (\process -> Process.getDisplayName process)
            |> List.map
                (\process ->
                    option
                        [ selected <| selectedCode == process.identifier
                        , value <| Process.identifierToString process.identifier
                        , disabled <| List.member process.identifier excluded
                        ]
                        [ text <| Process.getDisplayName process ]
                )
        )


sidebarView : Session -> Model -> Recipe.Results -> Html Msg
sidebarView session model results =
    SidebarView.view
        { session = session
        , scope = Scope.Food

        -- Impact selector
        , selectedImpact = model.impact
        , switchImpact = SwitchImpact

        -- Score
        , customScoreInfo = Nothing
        , productMass = results.preparedMass
        , totalImpacts = results.total

        -- Ecotox weighting customization
        , updateEcotoxWeighting = UpdateEcotoxWeighting

        -- Impacts tabs
        , impactTabsConfig =
            SwitchImpactsTab
                |> ImpactTabs.createConfig session model.impact model.activeImpactsTab OnStepClick
                |> ImpactTabs.forFood results

        -- Bookmarks
        , activeBookmarkTab = model.bookmarkTab
        , bookmarkName = model.bookmarkName
        , copyToClipBoard = CopyToClipBoard
        , compareBookmarks = OpenComparator
        , deleteBookmark = DeleteBookmark
        , saveBookmark = SaveBookmark
        , updateBookmarkName = UpdateBookmarkName
        , switchBookmarkTab = SwitchBookmarksTab
        }


stepListView : Db -> Session -> Model -> Recipe -> Recipe.Results -> Html Msg
stepListView db session { impact, initialQuery } recipe results =
    div []
        [ div [ class "card shadow-sm" ]
            (ingredientListView db impact recipe results)
        , transportToTransformationView impact results
        , div [ class "card shadow-sm" ]
            (transformView db impact recipe results)
        , transportToPackagingView recipe results
        , div [ class "card shadow-sm" ]
            (packagingListView db impact recipe results)
        , transportToDistributionView impact recipe results
        , div [ class "card shadow-sm" ]
            (distributionView impact recipe results)
        , transportToConsumptionView recipe
        , div [ class "card shadow-sm" ]
            (consumptionView db impact recipe results)
        , transportAfterConsumptionView recipe results
        , div [ class "d-flex align-items-center justify-content-between mt-3 mb-5" ]
            [ a [ Route.href Route.Home ]
                [ text "« Retour à l'accueil" ]
            , button
                [ class "btn btn-secondary"
                , onClick Reset
                , disabled (session.queries.food == initialQuery)
                ]
                [ text "Réinitialiser le produit" ]
            ]
        ]


transformView : Db -> Definition -> Recipe -> Recipe.Results -> List (Html Msg)
transformView db selectedImpact recipe results =
    let
        impact =
            results.recipe.transform
                |> Format.formatImpact selectedImpact
    in
    [ div
        [ class "card-header d-flex align-items-center justify-content-between"
        , StepsBorder.style Impact.stepsColors.transform
        ]
        [ h2
            [ class "h5 mb-0"
            , id "transform-step"
            ]
            [ text "Transformation" ]
        , span []
            [ impact
            , Button.docsPillLink
                [ class "btn btn-secondary ms-2 py-1"
                , href (Gitbook.publicUrlFromPath Gitbook.FoodTransformation)
                , title "Documentation"
                , target "_blank"
                ]
                [ Icon.question ]
            ]
        ]
    , ul [ class "CardList list-group list-group-flush border-top-0 border-bottom-0" ]
        [ case recipe.transform of
            Just transform ->
                updateProcessFormView
                    { processes =
                        db.food.processes
                            |> Process.listByCategory Process.Transform
                    , excluded = [ transform.process.identifier ]
                    , processQuery = { code = transform.process.identifier, mass = transform.mass }
                    , impact = impact
                    , updateEvent = UpdateTransform
                    , deleteEvent = ResetTransform
                    }

            Nothing ->
                addProcessFormView
                    { isDisabled = False
                    , event = AddTransform
                    , kind = "une transformation"
                    }
        ]
    ]


view : Session -> Model -> ( String, List (Html Msg) )
view session model =
    ( "Constructeur de recette"
    , [ Container.centered [ class "pb-3" ]
            [ mainView session model
            , case model.modal of
                NoModal ->
                    text ""

                ComparatorModal ->
                    ModalView.view
                        { size = ModalView.ExtraLarge
                        , close = SetModal NoModal
                        , noOp = NoOp
                        , title = "Comparateur de simulations sauvegardées"
                        , subTitle = Just "en score d'impact, par produit ⚠️\u{00A0}Attention, ces résultats sont provisoires"
                        , formAction = Nothing
                        , content =
                            [ ComparatorView.view
                                { session = session
                                , impact = model.impact
                                , comparisonType = model.comparisonType
                                , selectAll = SelectAllBookmarks
                                , selectNone = SelectNoBookmarks
                                , switchComparisonType = SwitchComparisonType
                                , toggle = ToggleComparedSimulation
                                }
                            ]
                        , footer = []
                        }

                AddIngredientModal _ autocompleteState ->
                    AutocompleteSelectorView.view
                        { autocompleteState = autocompleteState
                        , closeModal = SetModal NoModal
                        , footer = []
                        , noOp = NoOp
                        , onAutocomplete = OnAutocompleteIngredient
                        , onAutocompleteSelect = OnAutocompleteSelect
                        , placeholderText = "tapez ici le nom de la matière première pour la rechercher"
                        , title = "Sélectionnez un ingrédient"
                        , toCategory =
                            .categories
                                >> List.head
                                >> Maybe.map IngredientCategory.toLabel
                                >> Maybe.withDefault ""
                        , toDescription = Just (.default >> .name >> Process.nameToString)
                        , toLabel = .name
                        }

                SelectExampleModal autocompleteState ->
                    AutocompleteSelectorView.view
                        { autocompleteState = autocompleteState
                        , closeModal = SetModal NoModal
                        , footer = []
                        , noOp = NoOp
                        , onAutocomplete = OnAutocompleteExample
                        , onAutocompleteSelect = OnAutocompleteSelect
                        , placeholderText = "tapez ici le nom du produit pour le rechercher"
                        , title = "Sélectionnez un produit"
                        , toCategory = Example.toCategory session.db.food.examples
                        , toDescription = Nothing
                        , toLabel = Example.toName session.db.food.examples
                        }
            ]
      ]
    )


subscriptions : Model -> Sub Msg
subscriptions { modal } =
    case modal of
        NoModal ->
            Sub.none

        _ ->
            BE.onKeyDown (Key.escape (SetModal NoModal))

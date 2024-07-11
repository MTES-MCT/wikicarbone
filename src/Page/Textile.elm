module Page.Textile exposing
    ( Model
    , Msg(..)
    , init
    , initFromExample
    , subscriptions
    , update
    , view
    )

import Array
import Autocomplete exposing (Autocomplete)
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Data.AutocompleteSelector as AutocompleteSelector
import Data.Bookmark as Bookmark exposing (Bookmark)
import Data.Country as Country
import Data.Dataset as Dataset
import Data.Example as Example
import Data.Gitbook as Gitbook
import Data.Impact as Impact
import Data.Impact.Definition as Definition exposing (Definition)
import Data.Key as Key
import Data.Scope as Scope
import Data.Session as Session exposing (Session)
import Data.Split exposing (Split)
import Data.Textile.Db as TextileDb
import Data.Textile.DyeingMedium exposing (DyeingMedium)
import Data.Textile.Economics as Economics
import Data.Textile.Fabric exposing (Fabric)
import Data.Textile.Inputs as Inputs
import Data.Textile.LifeCycle as LifeCycle
import Data.Textile.MakingComplexity exposing (MakingComplexity)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Material.Origin as Origin
import Data.Textile.Material.Spinning exposing (Spinning)
import Data.Textile.Printing exposing (Printing)
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Query as Query exposing (MaterialQuery, Query)
import Data.Textile.Simulator as Simulator exposing (Simulator)
import Data.Textile.Step.Label exposing (Label)
import Data.Unit as Unit
import Data.Uuid exposing (Uuid)
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Mass
import Ports
import Route
import Static.Db as Db exposing (Db)
import Task
import Time exposing (Posix)
import Views.Alert as Alert
import Views.AutocompleteSelector as AutocompleteSelector
import Views.Bookmark as BookmarkView
import Views.Button as Button
import Views.CardTabs as CardTabs
import Views.Comparator as ComparatorView
import Views.Component.DownArrow as DownArrow
import Views.Container as Container
import Views.Example as ExampleView
import Views.Format as Format
import Views.Icon as Icon
import Views.ImpactTabs as ImpactTabs
import Views.Modal as ModalView
import Views.Sidebar as SidebarView
import Views.Textile.Step as StepView


type alias Model =
    { simulator : Result String Simulator
    , bookmarkName : String
    , bookmarkTab : BookmarkView.ActiveTab
    , comparisonType : ComparatorView.ComparisonType
    , initialQuery : Query
    , impact : Definition
    , modal : Modal
    , activeTab : Tab
    , activeImpactsTab : ImpactTabs.Tab
    }


type Modal
    = NoModal
    | ComparatorModal
    | AddMaterialModal (Maybe Inputs.MaterialInput) (Autocomplete Material)
    | ConfirmSwitchToRegulatoryModal
    | SelectExampleModal (Autocomplete Query)
    | SelectProductModal (Autocomplete Product)


type Tab
    = RegulatoryTab
    | AdvancedTab


type Msg
    = AddMaterial Material
    | ConfirmSwitchToRegulatory
    | CopyToClipBoard String
    | DeleteBookmark Bookmark
    | NoOp
    | OnAutocompleteExample (Autocomplete.Msg Query)
    | OnAutocompleteMaterial (Autocomplete.Msg Material)
    | OnAutocompleteProduct (Autocomplete.Msg Product)
    | OnAutocompleteSelect
    | OnStepClick String
    | OpenComparator
    | RemoveMaterial Material.Id
    | Reset
    | SaveBookmark
    | SaveBookmarkWithTime String Bookmark.Query Posix
    | SelectAllBookmarks
    | SelectNoBookmarks
    | SetModal Modal
    | SwitchBookmarksTab BookmarkView.ActiveTab
    | SwitchComparisonType ComparatorView.ComparisonType
    | SwitchImpact (Result String Definition.Trigram)
    | SwitchImpactsTab ImpactTabs.Tab
    | SwitchTab Tab
    | ToggleComparedSimulation Bookmark Bool
    | ToggleFading Bool
    | ToggleStep Label
    | UpdateAirTransportRatio (Maybe Split)
    | UpdateBookmarkName String
    | UpdateBusiness (Result String Economics.Business)
    | UpdateDyeingMedium DyeingMedium
    | UpdateEcotoxWeighting (Maybe Unit.Ratio)
    | UpdateFabricProcess Fabric
    | UpdateMakingComplexity MakingComplexity
    | UpdateMakingWaste (Maybe Split)
    | UpdateMakingDeadStock (Maybe Split)
    | UpdateMarketingDuration (Maybe Duration)
    | UpdateMassInput String
    | UpdateMaterial MaterialQuery MaterialQuery
    | UpdateMaterialSpinning Material Spinning
    | UpdateNumberOfReferences (Maybe Int)
    | UpdatePrice (Maybe Economics.Price)
    | UpdatePrinting (Maybe Printing)
    | UpdateStepCountry Label Country.Code
    | UpdateSurfaceMass (Maybe Unit.SurfaceMass)
    | UpdateTraceability Bool
    | UpdateYarnSize (Maybe Unit.YarnSize)


init : Definition.Trigram -> Maybe Query -> Session -> ( Model, Session, Cmd Msg )
init trigram maybeUrlQuery session =
    let
        initialQuery =
            -- If we received a serialized query from the URL, use it
            -- Otherwise, fallback to use session query
            maybeUrlQuery
                |> Maybe.withDefault session.queries.textile

        simulator =
            initialQuery
                |> Simulator.compute session.db
    in
    ( { simulator = simulator
      , bookmarkName = initialQuery |> suggestBookmarkName session
      , bookmarkTab = BookmarkView.SaveTab
      , comparisonType =
            if Session.isAuthenticated session then
                ComparatorView.Subscores

            else
                ComparatorView.Steps
      , initialQuery = initialQuery
      , impact = Definition.get trigram session.db.definitions
      , modal = NoModal
      , activeTab =
            if Query.isAdvancedQuery initialQuery then
                AdvancedTab

            else
                RegulatoryTab
      , activeImpactsTab = ImpactTabs.StepImpactsTab
      }
    , session
        |> Session.updateTextileQuery initialQuery
        |> (case simulator of
                Err error ->
                    Session.notifyError "Erreur de récupération des paramètres d'entrée" error

                Ok _ ->
                    identity
           )
    , case maybeUrlQuery of
        -- If we don't have an URL query, we may be coming from another app page, so we should
        -- reposition the viewport at the top.
        Nothing ->
            Ports.scrollTo { x = 0, y = 0 }

        -- If we do have an URL query, we either come from a bookmark, a saved simulation click or
        -- we're tweaking params for the current simulation: we shouldn't reposition the viewport.
        Just _ ->
            Cmd.none
    )


initFromExample : Session -> Uuid -> ( Model, Session, Cmd Msg )
initFromExample session uuid =
    let
        example =
            session.db.textile.examples
                |> Example.findByUuid uuid

        exampleQuery =
            example
                |> Result.map .query
                |> Result.withDefault session.queries.textile

        simulator =
            exampleQuery
                |> Simulator.compute session.db
    in
    ( { simulator = simulator
      , bookmarkName = exampleQuery |> suggestBookmarkName session
      , bookmarkTab = BookmarkView.SaveTab
      , comparisonType = ComparatorView.Subscores
      , initialQuery = exampleQuery
      , impact = Definition.get Definition.Ecs session.db.definitions
      , modal = NoModal
      , activeTab =
            if Query.isAdvancedQuery exampleQuery then
                AdvancedTab

            else
                RegulatoryTab
      , activeImpactsTab = ImpactTabs.StepImpactsTab
      }
    , session
        |> Session.updateTextileQuery exampleQuery
        |> (case simulator of
                Err error ->
                    Session.notifyError "Erreur de récupération des paramètres d'entrée" error

                Ok _ ->
                    identity
           )
    , Ports.scrollTo { x = 0, y = 0 }
    )


suggestBookmarkName : Session -> Query -> String
suggestBookmarkName { db, store } query =
    let
        -- Existing user bookmark?
        userBookmark =
            store.bookmarks
                |> Bookmark.findByTextileQuery query

        -- Matching product example name?
        exampleName =
            db.textile.examples
                |> Example.findByQuery query
                |> Result.toMaybe
    in
    case ( userBookmark, exampleName ) of
        ( Just { name }, _ ) ->
            name

        ( _, Just { name } ) ->
            name

        _ ->
            query
                |> Inputs.fromQuery db
                |> Result.map Inputs.toString
                |> Result.withDefault ""


updateQuery : Query -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
updateQuery query ( model, session, commands ) =
    ( { model
        | simulator = query |> Simulator.compute session.db
        , bookmarkName = query |> suggestBookmarkName session
      }
    , session |> Session.updateTextileQuery query
    , commands
    )


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update ({ db, queries, navKey } as session) msg model =
    let
        query =
            queries.textile
    in
    case ( msg, model.modal ) of
        ( AddMaterial material, _ ) ->
            update session (SetModal NoModal) model
                |> updateQuery (Query.addMaterial material query)

        ( ConfirmSwitchToRegulatory, _ ) ->
            ( { model | modal = NoModal, activeTab = RegulatoryTab }, session, Cmd.none )
                |> updateQuery (Query.regulatory query)

        ( CopyToClipBoard shareableLink, _ ) ->
            ( model, session, Ports.copyToClipboard shareableLink )

        ( DeleteBookmark bookmark, _ ) ->
            ( model
            , session |> Session.deleteBookmark bookmark
            , Cmd.none
            )

        ( NoOp, _ ) ->
            ( model, session, Cmd.none )

        ( OpenComparator, _ ) ->
            ( { model | modal = ComparatorModal }
            , session |> Session.checkComparedSimulations
            , Cmd.none
            )

        ( OnAutocompleteExample autocompleteMsg, SelectExampleModal autocompleteState ) ->
            let
                ( newAutocompleteState, autoCompleteCmd ) =
                    Autocomplete.update autocompleteMsg autocompleteState
            in
            ( { model | modal = SelectExampleModal newAutocompleteState }
            , session
            , Cmd.map OnAutocompleteExample autoCompleteCmd
            )

        ( OnAutocompleteExample _, _ ) ->
            ( model, session, Cmd.none )

        ( OnAutocompleteProduct autocompleteMsg, SelectProductModal autocompleteState ) ->
            let
                ( newAutocompleteState, autoCompleteCmd ) =
                    Autocomplete.update autocompleteMsg autocompleteState
            in
            ( { model | modal = SelectProductModal newAutocompleteState }
            , session
            , Cmd.map OnAutocompleteProduct autoCompleteCmd
            )

        ( OnAutocompleteProduct _, _ ) ->
            ( model, session, Cmd.none )

        ( OnAutocompleteMaterial autocompleteMsg, AddMaterialModal maybeOldMaterial autocompleteState ) ->
            let
                ( newAutocompleteState, autoCompleteCmd ) =
                    Autocomplete.update autocompleteMsg autocompleteState
            in
            ( { model | modal = AddMaterialModal maybeOldMaterial newAutocompleteState }
            , session
            , Cmd.map OnAutocompleteMaterial autoCompleteCmd
            )

        ( OnAutocompleteMaterial _, _ ) ->
            ( model, session, Cmd.none )

        ( OnAutocompleteSelect, AddMaterialModal maybeOldMaterial autocompleteState ) ->
            updateMaterial query model session maybeOldMaterial autocompleteState

        ( OnAutocompleteSelect, SelectExampleModal autocompleteState ) ->
            ( model, session, Cmd.none )
                |> selectExample autocompleteState

        ( OnAutocompleteSelect, SelectProductModal autocompleteState ) ->
            ( model, session, Cmd.none )
                |> selectProduct autocompleteState

        ( OnAutocompleteSelect, _ ) ->
            ( model, session, Cmd.none )

        ( OnStepClick stepId, _ ) ->
            ( model
            , session
            , Ports.scrollIntoView stepId
            )

        ( RemoveMaterial materialId, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.removeMaterial materialId query)

        ( Reset, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery model.initialQuery

        ( SaveBookmark, _ ) ->
            ( model
            , session
            , Time.now
                |> Task.perform
                    (SaveBookmarkWithTime model.bookmarkName
                        (Bookmark.Textile query)
                    )
            )

        ( SaveBookmarkWithTime name foodQuery now, _ ) ->
            ( model
            , session
                |> Session.saveBookmark
                    { name = String.trim name
                    , query = foodQuery
                    , created = now
                    }
            , Cmd.none
            )

        ( SelectAllBookmarks, _ ) ->
            ( model, Session.selectAllBookmarks session, Cmd.none )

        ( SelectNoBookmarks, _ ) ->
            ( model, Session.selectNoBookmarks session, Cmd.none )

        ( SetModal NoModal, _ ) ->
            ( { model | modal = NoModal }
            , session
            , commandsForNoModal model.modal
            )

        ( SetModal ComparatorModal, _ ) ->
            ( { model | modal = ComparatorModal }
            , session
            , Ports.addBodyClass "prevent-scrolling"
            )

        ( SetModal ConfirmSwitchToRegulatoryModal, _ ) ->
            ( { model | modal = ConfirmSwitchToRegulatoryModal }
            , session
            , Cmd.none
            )

        ( SetModal (AddMaterialModal maybeOldMaterial autocomplete), _ ) ->
            ( { model | modal = AddMaterialModal maybeOldMaterial autocomplete }
            , session
            , Cmd.batch
                [ Ports.addBodyClass "prevent-scrolling"
                , Dom.focus "element-search"
                    |> Task.attempt (always NoOp)
                ]
            )

        ( SetModal (SelectExampleModal autocomplete), _ ) ->
            ( { model | modal = SelectExampleModal autocomplete }
            , session
            , Ports.addBodyClass "prevent-scrolling"
            )

        ( SetModal (SelectProductModal autocomplete), _ ) ->
            ( { model | modal = SelectProductModal autocomplete }
            , session
            , Ports.addBodyClass "prevent-scrolling"
            )

        ( SwitchBookmarksTab bookmarkTab, _ ) ->
            ( { model | bookmarkTab = bookmarkTab }
            , session
            , Cmd.none
            )

        ( SwitchComparisonType displayChoice, _ ) ->
            ( { model | comparisonType = displayChoice }, session, Cmd.none )

        ( SwitchImpact (Ok trigram), _ ) ->
            ( model
            , session
            , Just query
                |> Route.TextileSimulator trigram
                |> Route.toString
                |> Navigation.pushUrl navKey
            )

        ( SwitchImpact (Err error), _ ) ->
            ( model
            , session |> Session.notifyError "Erreur de sélection d'impact: " error
            , Cmd.none
            )

        ( SwitchImpactsTab impactsTab, _ ) ->
            ( { model | activeImpactsTab = impactsTab }
            , session
            , Cmd.none
            )

        ( SwitchTab RegulatoryTab, _ ) ->
            if Query.isAdvancedQuery query then
                ( { model | modal = ConfirmSwitchToRegulatoryModal }, session, Cmd.none )

            else
                ( { model | activeTab = RegulatoryTab }, session, Cmd.none )

        ( SwitchTab AdvancedTab, _ ) ->
            ( { model | activeTab = AdvancedTab }, session, Cmd.none )

        ( ToggleComparedSimulation bookmark checked, _ ) ->
            ( model
            , session |> Session.toggleComparedSimulation bookmark checked
            , Cmd.none
            )

        ( ToggleFading fading, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | fading = Just fading }

        ( ToggleStep label, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.toggleStep label query)

        ( UpdateAirTransportRatio airTransportRatio, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | airTransportRatio = airTransportRatio }

        ( UpdateBookmarkName newName, _ ) ->
            ( { model | bookmarkName = newName }, session, Cmd.none )

        ( UpdateBusiness (Ok business), _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | business = Just business }

        ( UpdateBusiness (Err error), _ ) ->
            ( model, session |> Session.notifyError "Erreur de type d'entreprise" error, Cmd.none )

        ( UpdateDyeingMedium dyeingMedium, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | dyeingMedium = Just dyeingMedium }

        ( UpdateEcotoxWeighting (Just ratio), _ ) ->
            ( model, { session | db = Db.updateEcotoxWeighting db ratio }, Cmd.none )
                -- triggers recompute
                |> updateQuery query

        ( UpdateEcotoxWeighting Nothing, _ ) ->
            ( model, session, Cmd.none )

        ( UpdateFabricProcess fabricProcess, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | fabricProcess = Just fabricProcess }

        ( UpdateMakingComplexity makingComplexity, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | makingComplexity = Just makingComplexity }

        ( UpdateMakingWaste makingWaste, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | makingWaste = makingWaste }

        ( UpdateMakingDeadStock makingDeadStock, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | makingDeadStock = makingDeadStock }

        ( UpdateMarketingDuration marketingDuration, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | marketingDuration = marketingDuration }

        ( UpdateMassInput massInput, _ ) ->
            case massInput |> String.toFloat |> Maybe.map Mass.kilograms of
                Just mass ->
                    ( model, session, Cmd.none )
                        |> updateQuery { query | mass = mass }

                Nothing ->
                    ( model, session, Cmd.none )

        ( UpdateMaterial oldMaterial newMaterial, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updateMaterial oldMaterial.id newMaterial query)

        ( UpdateMaterialSpinning material spinning, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updateMaterialSpinning material spinning query)

        ( UpdateNumberOfReferences numberOfReferences, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | numberOfReferences = numberOfReferences }

        ( UpdatePrice price, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | price = price }

        ( UpdatePrinting printing, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | printing = printing }

        ( UpdateStepCountry label code, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery (Query.updateStepCountry label code query)

        ( UpdateSurfaceMass surfaceMass, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | surfaceMass = surfaceMass }

        ( UpdateTraceability traceability, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | traceability = Just traceability }

        ( UpdateYarnSize yarnSize, _ ) ->
            ( model, session, Cmd.none )
                |> updateQuery { query | yarnSize = yarnSize }


commandsForNoModal : Modal -> Cmd Msg
commandsForNoModal modal =
    case modal of
        AddMaterialModal maybeOldMaterial _ ->
            Cmd.batch
                [ Ports.removeBodyClass "prevent-scrolling"
                , Dom.focus
                    -- This whole "node to focus" management is happening as a fallback
                    -- if the modal was closed without choosing anything.
                    -- If anything has been chosen, then the focus will be done in `OnAutocompleteSelect`
                    -- and overload any focus being done here.
                    (maybeOldMaterial
                        |> Maybe.map (.material >> .id >> Material.idToString >> (++) "selector-")
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

        SelectProductModal _ ->
            Cmd.batch
                [ Ports.removeBodyClass "prevent-scrolling"
                , Dom.focus "selector-product"
                    |> Task.attempt (always NoOp)
                ]

        _ ->
            Ports.removeBodyClass "prevent-scrolling"


updateExistingMaterial : Query -> Model -> Session -> Inputs.MaterialInput -> Material -> ( Model, Session, Cmd Msg )
updateExistingMaterial query model session oldMaterial newMaterial =
    let
        materialQuery : MaterialQuery
        materialQuery =
            { id = newMaterial.id
            , share = oldMaterial.share
            , spinning = Nothing
            , country = Nothing
            }
    in
    model
        |> update session (SetModal NoModal)
        |> updateQuery (Query.updateMaterial oldMaterial.material.id materialQuery query)
        |> focusNode ("selector-" ++ Material.idToString newMaterial.id)


updateMaterial : Query -> Model -> Session -> Maybe Inputs.MaterialInput -> Autocomplete Material -> ( Model, Session, Cmd Msg )
updateMaterial query model session maybeOldMaterial autocompleteState =
    let
        maybeSelectedValue =
            Autocomplete.selectedValue autocompleteState
    in
    Maybe.map2
        (updateExistingMaterial query model session)
        maybeOldMaterial
        maybeSelectedValue
        |> Maybe.withDefault
            -- Add a new Material
            (model
                |> update session (SetModal NoModal)
                |> selectMaterial autocompleteState
                |> focusNode
                    (maybeSelectedValue
                        |> Maybe.map (\selectedValue -> "selector-" ++ Material.idToString selectedValue.id)
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


selectExample : Autocomplete Query -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
selectExample autocompleteState ( model, session, _ ) =
    let
        example =
            Autocomplete.selectedValue autocompleteState
                |> Maybe.withDefault Query.default
    in
    update session (SetModal NoModal) { model | initialQuery = example }
        |> updateQuery example


selectProduct : Autocomplete Product -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
selectProduct autocompleteState ( model, session, _ ) =
    case Autocomplete.selectedValue autocompleteState of
        Just product ->
            update session (SetModal NoModal) model
                |> updateQuery (Query.updateProduct product session.queries.textile)

        Nothing ->
            model
                |> update (session |> Session.notifyError "Erreur" "Aucun produit sélectionné") (SetModal NoModal)


selectMaterial : Autocomplete Material -> ( Model, Session, Cmd Msg ) -> ( Model, Session, Cmd Msg )
selectMaterial autocompleteState ( model, session, _ ) =
    let
        material =
            Autocomplete.selectedValue autocompleteState
                |> Maybe.map Just
                |> Maybe.withDefault (List.head (Autocomplete.choices autocompleteState))

        msg =
            material
                |> Maybe.map AddMaterial
                |> Maybe.withDefault NoOp
    in
    update session msg model


productCategoryField : TextileDb.Db -> Query -> Html Msg
productCategoryField { products } query =
    let
        autocompleteState =
            products
                |> List.sortBy .name
                |> AutocompleteSelector.init .name
    in
    div [ class "d-flex flex-column" ]
        [ label [ for "selector-product", class "form-label text-truncate" ]
            [ text "Catégorie" ]
        , button
            [ id "selector-product"
            , class "form-select ElementSelector text-start w-auto"
            , onClick (SetModal (SelectProductModal autocompleteState))
            ]
            [ products
                |> Product.findById query.product
                |> Result.map .name
                |> Result.withDefault ""
                |> text
            ]
        ]


numberOfReferencesField : Int -> Html Msg
numberOfReferencesField numberOfReferences =
    div [ class "row align-items-center g-2" ]
        [ label
            [ for "number-of-references"
            , class "col-sm-6 col-form-label text-truncate"
            ]
            [ text "Nombre de références" ]
        , div [ class "col-sm-6" ]
            [ input
                [ type_ "number"
                , id "number-of-references"
                , class "form-control"

                -- WARNING: be careful when reordering attributes: for obscure reasons,
                -- the `value` one MUST be set AFTER the `step` one.
                , Attr.min <| String.fromInt <| Economics.minNumberOfReferences
                , Attr.max <| String.fromInt <| Economics.maxNumberOfReferences
                , step "1"
                , value (String.fromInt numberOfReferences)
                , onInput (String.toInt >> UpdateNumberOfReferences)
                ]
                []
            ]
        ]


productPriceField : Economics.Price -> Html Msg
productPriceField productPrice =
    div [ class "row align-items-center g-2" ]
        [ label
            [ for "product-price"
            , class "col-sm-6 col-md-5 col-form-label text-truncate"
            ]
            [ text "Prix neuf" ]
        , div [ class "col-sm-6 col-md-7" ]
            [ div [ class "input-group" ]
                [ input
                    [ type_ "number"
                    , id "product-price"
                    , class "form-control"
                    , Attr.min <| String.fromFloat <| Economics.priceToFloat <| Economics.minPrice
                    , Attr.max <| String.fromFloat <| Economics.priceToFloat <| Economics.maxPrice
                    , productPrice |> Economics.priceToFloat |> String.fromFloat |> value
                    , onInput (String.toFloat >> Maybe.map Economics.priceFromFloat >> UpdatePrice)
                    ]
                    []
                , span [ class "input-group-text" ] [ text "€" ]
                ]
            ]
        ]


marketingDurationField : Duration -> Html Msg
marketingDurationField marketingDuration =
    div [ class "row align-items-center g-2" ]
        [ label
            [ for "marketing-duration"
            , class "col-sm-6 col-form-label text-truncate"
            ]
            [ text "Durée de commercialisation" ]
        , div [ class "col-sm-6" ]
            [ div [ class "input-group" ]
                [ input
                    [ type_ "number"
                    , id "marketing-duration"
                    , class "form-control"
                    , Attr.min <| String.fromFloat <| Duration.inDays <| Economics.minMarketingDuration
                    , Attr.max <| String.fromFloat <| Duration.inDays <| Economics.maxMarketingDuration

                    -- WARNING: be careful when reordering attributes: for obscure reasons,
                    -- the `value` one MUST be set AFTER the `step` one.
                    , step "1"
                    , marketingDuration |> Duration.inDays |> String.fromFloat |> value
                    , onInput (String.toInt >> Maybe.map (toFloat >> Duration.days) >> UpdateMarketingDuration)
                    ]
                    []
                , span [ class "input-group-text", title "jours" ] [ text "j." ]
                ]
            ]
        ]


businessField : Economics.Business -> Html Msg
businessField business =
    [ Economics.SmallBusiness
    , Economics.LargeBusinessWithoutServices
    , Economics.LargeBusinessWithServices
    ]
        |> List.map
            (\b ->
                option [ value (Economics.businessToString b), selected (business == b) ]
                    [ text (Economics.businessToLabel b) ]
            )
        |> select
            [ id "business"
            , class "form-select"
            , onInput (Economics.businessFromString >> UpdateBusiness)
            ]


traceabilityField : Bool -> Html Msg
traceabilityField traceability =
    div [ class "form-check align-items-center g-2 pt-2" ]
        [ input
            [ type_ "checkbox"
            , id "traceability"
            , class "form-check-input"
            , onCheck UpdateTraceability
            , checked traceability
            ]
            []
        , label [ for "traceability", class "form-check-label text-truncate" ]
            [ text "Traçabilité affichée" ]
        ]


massField : String -> Html Msg
massField massInput =
    div [ class "d-flex flex-column" ]
        [ label [ for "mass", class "form-label text-truncate" ]
            [ text "Masse du produit fini" ]
        , div
            [ class "input-group" ]
            [ input
                [ type_ "number"
                , class "form-control"
                , id "mass"
                , Attr.min "0.01"
                , step "0.01"
                , value massInput
                , onInput UpdateMassInput
                ]
                []
            , span [ class "input-group-text" ] [ text "kg" ]
            ]
        ]


lifeCycleStepsView : Db -> Model -> Simulator -> Html Msg
lifeCycleStepsView db { activeTab, impact } simulator =
    simulator.lifeCycle
        |> Array.indexedMap
            (\index current ->
                StepView.view
                    { db = db
                    , current = current
                    , daysOfWear = simulator.daysOfWear
                    , useNbCycles = simulator.useNbCycles
                    , index = index
                    , inputs = simulator.inputs
                    , next = LifeCycle.getNextEnabledStep current.label simulator.lifeCycle
                    , selectedImpact = impact
                    , showAdvancedFields = activeTab == AdvancedTab

                    -- Events
                    , addMaterialModal = AddMaterialModal
                    , deleteMaterial = .id >> RemoveMaterial
                    , setModal = SetModal
                    , toggleFading = ToggleFading
                    , toggleStep = ToggleStep
                    , updateCountry = UpdateStepCountry
                    , updateAirTransportRatio = UpdateAirTransportRatio
                    , updateDyeingMedium = UpdateDyeingMedium
                    , updateMaterial = UpdateMaterial
                    , updateMaterialSpinning = UpdateMaterialSpinning
                    , updateFabricProcess = UpdateFabricProcess
                    , updatePrinting = UpdatePrinting
                    , updateMakingComplexity = UpdateMakingComplexity
                    , updateMakingWaste = UpdateMakingWaste
                    , updateMakingDeadStock = UpdateMakingDeadStock
                    , updateSurfaceMass = UpdateSurfaceMass
                    , updateYarnSize = UpdateYarnSize
                    }
            )
        |> Array.toList
        |> List.concatMap
            (\{ step, transport } ->
                [ step
                , DownArrow.view [] [ transport ]
                ]
            )
        -- Drop the very last item, which is the last arrow showing the mass out of the end of life step
        -- which doesn't really make sense
        |> (List.reverse >> List.drop 1 >> List.reverse)
        |> div [ class "pt-1" ]


simulatorFormView : Session -> Model -> Simulator -> List (Html Msg)
simulatorFormView session model ({ inputs } as simulator) =
    [ div [ class "row align-items-start flex-md-columns g-2 mb-3" ]
        [ div [ class "col-md-8" ]
            [ inputs
                |> Inputs.toQuery
                |> productCategoryField session.db.textile
            ]
        , div [ class "col-md-4" ]
            [ inputs.mass
                |> Mass.inKilograms
                |> String.fromFloat
                |> massField
            ]
        ]
    , div [ class "card shadow-sm pb-2 mb-3" ]
        [ div [ class "card-header d-flex justify-content-between align-items-center" ]
            [ h2 [ class "h5 mb-1 text-truncate" ] [ text "Durabilité non-physique" ]
            , div [ class "d-flex align-items-center gap-2" ]
                [ span [ class "d-none d-sm-flex text-truncate" ] [ text "Coefficient de durabilité\u{00A0}:" ]
                , simulator.durability
                    |> Unit.durabilityToFloat
                    |> Format.formatFloat 2
                    |> text
                , Button.docsPillLink
                    [ class "bg-secondary"
                    , style "height" "24px"
                    , href (Gitbook.publicUrlFromPath Gitbook.TextileDurability)
                    , title "Documentation"
                    , target "_blank"
                    ]
                    [ Icon.question ]
                ]
            ]
        , div [ class "card-body pt-3 py-2 row g-3 align-items-start flex-md-columns" ]
            [ div [ class "col-md-8" ]
                [ inputs.numberOfReferences
                    |> Maybe.withDefault inputs.product.economics.numberOfReferences
                    |> numberOfReferencesField
                ]
            , div [ class "col-md-4" ]
                [ inputs.price
                    |> Maybe.withDefault inputs.product.economics.price
                    |> productPriceField
                ]
            ]
        , div [ class "card-body py-2 row g-3 align-items-start flex-md-columns" ]
            [ div [ class "col-md-8" ]
                [ inputs.marketingDuration
                    |> Maybe.withDefault inputs.product.economics.marketingDuration
                    |> marketingDurationField
                ]
            , div [ class "col-md-4" ]
                [ inputs.traceability
                    |> Maybe.withDefault inputs.product.economics.traceability
                    |> traceabilityField
                ]
            ]
        , div [ class "card-body py-2 row g-3 align-items-start flex-md-columns" ]
            [ div [ class "col-md-2" ] [ text "Entreprise" ]
            , div [ class "col-md-10" ]
                [ inputs.business
                    |> Maybe.withDefault inputs.product.economics.business
                    |> businessField
                ]
            ]
        , div [ class "card-body py-2 row g-3 align-items-start flex-md-columns" ]
            [ div [ class "col-md-2" ] [ text "Matières" ]
            , div [ class "col-md-10" ]
                [ div [ class "fw-bold" ]
                    [ Inputs.getMaterialsOriginShares inputs.materials
                        |> Economics.computeMaterialsOriginIndex
                        |> Tuple.second
                        |> text
                    ]
                , small [ class "text-muted fs-8 lh-sm" ]
                    [ text "Le type de matière retenu dépend de la composition du vêtement détaillée ci-dessous" ]
                ]
            ]
        ]
    , div []
        [ lifeCycleStepsView session.db model simulator
        , div [ class "d-flex align-items-center justify-content-between mt-3" ]
            [ a [ Route.href Route.Home ]
                [ text "« Retour à l'accueil" ]
            , button
                [ class "btn btn-secondary"
                , onClick Reset
                , disabled (session.queries.textile == model.initialQuery)
                ]
                [ text "Réinitialiser le produit" ]
            ]
        ]
    ]


simulatorView : Session -> Model -> Simulator -> Html Msg
simulatorView session model ({ inputs, impacts } as simulator) =
    let
        tabLabel help name =
            span [ class "d-flex justify-content-between align-items-center gap-1" ]
                [ span [ class "d-flex flex-fill justify-content-center" ] [ text name ]
                , span [ class "text-muted fs-8 cursor-help opacity-50", title help ] [ Icon.question ]
                ]
    in
    div [ class "row" ]
        [ div [ class "col-lg-8 bg-white" ]
            [ h1 [ class "visually-hidden" ] [ text "Simulateur " ]
            , div [ class "sticky-md-top bg-white pb-3" ]
                [ ExampleView.view
                    { currentQuery = session.queries.textile
                    , emptyQuery = Query.default
                    , examples = session.db.textile.examples
                    , helpUrl = Just Gitbook.TextileExamples
                    , onOpen = SelectExampleModal >> SetModal
                    , routes =
                        { explore = Route.Explore Scope.Textile (Dataset.TextileExamples Nothing)
                        , load = Route.TextileSimulatorExample
                        , scopeHome = Route.TextileSimulatorHome
                        }
                    }
                ]
            , div [ class "d-flex flex-column bg-white" ]
                [ CardTabs.view
                    { attrs = [ class "sticky-md-top", style "top" "50px" ]
                    , tabs =
                        [ { label =
                                "Mode règlementaire"
                                    |> tabLabel "N’affiche que les champs proposés dans le projet de cadre réglementaire"
                          , active = model.activeTab == RegulatoryTab
                          , onTabClick = SwitchTab RegulatoryTab
                          }
                        , { label =
                                "Mode avancé"
                                    |> tabLabel "Affiche des champs supplémentaires, hors cadre réglementaire"
                          , active = model.activeTab == AdvancedTab
                          , onTabClick = SwitchTab AdvancedTab
                          }
                        ]
                    , content =
                        [ simulator
                            |> simulatorFormView session model
                            |> div [ class "card-body p-2" ]
                        ]
                    }
                ]
            ]
        , div [ class "col-lg-4 bg-white" ]
            [ SidebarView.view
                { session = session
                , scope = Scope.Textile

                -- Impact selector
                , selectedImpact = model.impact
                , switchImpact = SwitchImpact

                -- Score
                , customScoreInfo =
                    Just
                        (div [ class "fs-8" ]
                            [ text "Pour 100g\u{00A0}:\u{00A0}"
                            , impacts
                                |> Impact.per100grams inputs.mass
                                |> Format.formatImpact model.impact
                            ]
                        )
                , productMass = inputs.mass
                , totalImpacts = impacts

                -- Ecotox weighting customization
                , updateEcotoxWeighting = UpdateEcotoxWeighting

                -- Impacts tabs
                , impactTabsConfig =
                    SwitchImpactsTab
                        |> ImpactTabs.createConfig session model.impact model.activeImpactsTab OnStepClick
                        |> ImpactTabs.forTextile session.db.definitions simulator

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
            ]
        ]


view : Session -> Model -> ( String, List (Html Msg) )
view session model =
    ( "Simulateur"
    , [ Container.centered [ class "Simulator pb-3" ]
            (case model.simulator of
                Ok simulator ->
                    [ simulatorView session model simulator
                    , case model.modal of
                        NoModal ->
                            text ""

                        ComparatorModal ->
                            ModalView.view
                                { size = ModalView.ExtraLarge
                                , close = SetModal NoModal
                                , noOp = NoOp
                                , title = "Comparateur de simulations sauvegardées"
                                , subTitle = Just "en score d'impact, par produit"
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

                        AddMaterialModal _ autocompleteState ->
                            AutocompleteSelector.view
                                { autocompleteState = autocompleteState
                                , closeModal = SetModal NoModal
                                , footer = []
                                , noOp = NoOp
                                , onAutocomplete = OnAutocompleteMaterial
                                , onAutocompleteSelect = OnAutocompleteSelect
                                , placeholderText = "tapez ici le nom de la matière première pour la rechercher"
                                , title = "Sélectionnez une matière première"
                                , toCategory = .origin >> Origin.toLabel
                                , toDescription = Nothing
                                , toLabel = .shortName
                                }

                        ConfirmSwitchToRegulatoryModal ->
                            ModalView.view
                                { size = ModalView.Standard
                                , close = SetModal NoModal
                                , noOp = NoOp
                                , title = "Avertissement"
                                , subTitle = Nothing
                                , formAction = Nothing
                                , content =
                                    [ div [ class "p-3" ]
                                        [ p []
                                            [ text "Basculer en mode règlementaire réinitialisera les valeurs renseignées pour les champs avancés." ]
                                        , p
                                            [ class "d-flex justify-content-center align-items-center gap-1" ]
                                            [ button
                                                [ class "btn btn-primary"
                                                , onClick ConfirmSwitchToRegulatory
                                                ]
                                                [ text "Confirmer" ]
                                            , text "ou"
                                            , button [ class "btn btn-link ps-0", onClick (SetModal NoModal) ]
                                                [ text "rester en mode avancé" ]
                                            ]
                                        ]
                                    ]
                                , footer = []
                                }

                        SelectExampleModal autocompleteState ->
                            AutocompleteSelector.view
                                { autocompleteState = autocompleteState
                                , closeModal = SetModal NoModal
                                , footer = []
                                , noOp = NoOp
                                , onAutocomplete = OnAutocompleteExample
                                , onAutocompleteSelect = OnAutocompleteSelect
                                , placeholderText = "tapez ici le nom du produit pour le rechercher"
                                , title = "Sélectionnez un produit"
                                , toCategory = Example.toCategory session.db.textile.examples
                                , toDescription = Nothing
                                , toLabel = Example.toName session.db.textile.examples
                                }

                        SelectProductModal autocompleteState ->
                            AutocompleteSelector.view
                                { autocompleteState = autocompleteState
                                , closeModal = SetModal NoModal
                                , footer =
                                    [ a
                                        [ class "d-flex justify-content-between gap-2 align-items-center btn btn-primary"
                                        , href "https://forms.gle/JY6QYMppqRTiCM6g8"
                                        , target "_blank"
                                        ]
                                        [ Icon.plus
                                        , text "Suggérer une nouvelle catégorie"
                                        ]
                                    ]
                                , noOp = NoOp
                                , onAutocomplete = OnAutocompleteProduct
                                , onAutocompleteSelect = OnAutocompleteSelect
                                , placeholderText = "tapez ici une catégorie pour la rechercher"
                                , title = "Sélectionnez une catégorie de produit"
                                , toCategory = always ""
                                , toDescription = Nothing
                                , toLabel = .name
                                }
                    ]

                Err error ->
                    [ Alert.simple
                        { level = Alert.Danger
                        , close = Nothing
                        , title = Just "Erreur"
                        , content = [ text error ]
                        }
                    ]
            )
      ]
    )


subscriptions : Model -> Sub Msg
subscriptions { modal } =
    case modal of
        NoModal ->
            Sub.none

        _ ->
            Browser.Events.onKeyDown (Key.escape (SetModal NoModal))

module Views.Page exposing
    ( ActivePage(..)
    , Config
    , frame
    , loading
    , notFound
    )

import Browser exposing (Document)
import Data.Db as Db
import Data.Impact as Impact
import Data.Session as Session exposing (Session)
import Data.Unit as Unit
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Simulator.ViewMode as ViewMode
import Request.Version
import Route
import Views.Alert as Alert
import Views.Container as Container
import Views.Icon as Icon
import Views.Link as Link
import Views.Spinner as Spinner


type ActivePage
    = Home
    | Changelog
    | Examples
    | Explore
    | Api
    | Simulator
    | Stats
    | Other


type MenuLink
    = Internal String Route.Route ActivePage
    | External String String
    | MailTo String String


type alias Config msg =
    { session : Session
    , mobileNavigationOpened : Bool
    , closeMobileNavigation : msg
    , openMobileNavigation : msg
    , loadUrl : String -> msg
    , reloadPage : msg
    , closeNotification : Session.Notification -> msg
    , activePage : ActivePage
    }


frame : Config msg -> ( String, List (Html msg) ) -> Document msg
frame config ( title, content ) =
    { title = title ++ " | wikicarbone"
    , body =
        [ stagingAlert config
        , newVersionAlert config
        , navbar config
        , if config.mobileNavigationOpened then
            mobileNavigation config

          else
            text ""
        , main_ [ class "bg-white" ]
            [ notificationListView config
            , div [ class "pt-2 pt-sm-5" ] content
            ]
        , pageFooter
        ]
    }


stagingAlert : Config msg -> Html msg
stagingAlert { session, loadUrl } =
    if String.contains "wikicarbone-pr" session.clientUrl then
        div [ class "StagingAlert d-block d-sm-flex justify-content-center align-items-center mt-3" ]
            [ text "Vous êtes sur un environnement de recette. "
            , button
                [ type_ "button"
                , class "btn btn-link"
                , onClick (loadUrl "https://wikicarbone.beta.gouv.fr/")
                ]
                [ text "Retourner vers l'environnement de production" ]
            ]

    else
        text ""


newVersionAlert : Config msg -> Html msg
newVersionAlert { session, reloadPage } =
    case session.currentVersion of
        Request.Version.NewerVersion ->
            div [ class "NewVersionAlert d-block align-items-center" ]
                [ text "Une nouvelle version de l'application est disponible."
                , button
                    [ type_ "button"
                    , onClick reloadPage
                    ]
                    [ text "Mettre à jour" ]
                ]

        _ ->
            text ""


headerMenuLinks : List MenuLink
headerMenuLinks =
    [ Internal "Accueil" Route.Home Home
    , Internal "Simulateur" (Route.Simulator Impact.defaultTrigram Unit.PerItem ViewMode.Simple Nothing) Simulator
    , Internal "Exemples" Route.Examples Examples
    , Internal "Explorateur" (Route.Explore (Db.Countries Nothing)) Explore
    , External "Documentation" "https://fabrique-numerique.gitbook.io/wikicarbone/"
    ]


footerMenuLinks : List MenuLink
footerMenuLinks =
    [ Internal "Accueil" Route.Home Home
    , Internal "Simulateur" (Route.Simulator Impact.defaultTrigram Unit.PerItem ViewMode.Simple Nothing) Simulator
    , Internal "Exemples" Route.Examples Examples
    , Internal "Api documentation" Route.Api Api
    , Internal "Changelog" Route.Changelog Changelog
    , Internal "Statistiques" Route.Stats Stats
    , External "Code source" "https://github.com/MTES-MCT/wikicarbone/"
    , External "Documentation" "https://fabrique-numerique.gitbook.io/wikicarbone/"
    , External "FAQ" "https://fabrique-numerique.gitbook.io/wikicarbone/faq"
    , MailTo "Contact" "wikicarbone@beta.gouv.fr"
    ]


navbar : Config msg -> Html msg
navbar { activePage, openMobileNavigation } =
    nav [ class "Header navbar navbar-expand-lg navbar-dark bg-dark shadow" ]
        [ Container.centered []
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ img
                    [ class "d-inline-block align-text-bottom invert me-2"
                    , alt ""
                    , src "img/logo.svg"
                    , height 26
                    ]
                    []
                , span [ class "fs-3" ] [ text "wikicarbone" ]
                ]
            , headerMenuLinks
                |> List.map (viewNavigationLink activePage)
                |> div
                    [ class "d-none d-sm-flex MainMenu navbar-nav justify-content-between flex-row"
                    , style "overflow" "auto"
                    ]
            , button
                [ type_ "button"
                , class "d-inline-block d-sm-none btn btn-dark m-0 p-0"
                , attribute "aria-label" "Ouvrir la navigation"
                , title "Ouvrir la navigation"
                , onClick openMobileNavigation
                ]
                [ Icon.verticalDots ]
            ]
        ]


viewNavigationLink : ActivePage -> MenuLink -> Html msg
viewNavigationLink activePage link =
    case link of
        Internal label route page ->
            Link.internal
                (class "nav-link pe-3"
                    :: classList [ ( "active", page == activePage ) ]
                    :: Route.href route
                    :: (if page == activePage then
                            [ attribute "aria-current" "page" ]

                        else
                            []
                       )
                )
                [ text label ]

        External label url ->
            Link.external [ class "nav-link link-external-muted pe-2", href url ]
                [ text label ]

        MailTo label email ->
            a [ class "nav-link", href <| "mailto:" ++ email ] [ text label ]


notificationListView : Config msg -> Html msg
notificationListView ({ session } as config) =
    session.notifications
        |> List.map (notificationView config)
        |> Container.centered [ class "bg-white pt-3" ]


notificationView : Config msg -> Session.Notification -> Html msg
notificationView { closeNotification } notification =
    -- TODO:
    -- - absolute positionning
    case notification of
        Session.HttpError error ->
            Alert.httpError error

        Session.GenericError title message ->
            Alert.simple
                { level = Alert.Danger
                , title = Just title
                , close = Just (closeNotification notification)
                , content = [ text message ]
                }


pageFooter : Html msg
pageFooter =
    footer
        [ class "bg-dark text-light py-5 fs-7" ]
        [ Container.centered []
            [ div [ class "row d-flex align-items-center" ]
                [ div [ class "col" ]
                    [ h3 [] [ text "wikicarbone" ]
                    , footerMenuLinks
                        |> List.map
                            (\link ->
                                case link of
                                    Internal label route _ ->
                                        Link.internal [ class "text-white text-decoration-none", Route.href route ]
                                            [ text label ]

                                    External label url ->
                                        Link.external [ class "text-white text-decoration-none", href url ]
                                            [ text label ]

                                    MailTo label email ->
                                        a [ class "text-white text-decoration-none link-email", href <| "mailto:" ++ email ]
                                            [ text label ]
                            )
                        |> List.map (List.singleton >> li [])
                        |> ul [ class "list-unstyled" ]
                    ]
                , Link.external
                    [ href "https://www.ecologique-solidaire.gouv.fr/"
                    , class "col text-center bg-white px-3 m-3 link-external-muted"
                    ]
                    [ img
                        [ src "img/logo_mte.svg"
                        , alt "Ministère de la transition écologique et solidaire"
                        , attribute "width" "200"
                        , attribute "height" "200"
                        ]
                        []
                    ]
                , Link.external
                    [ href "https://www.cohesion-territoires.gouv.fr/"
                    , class "col text-center bg-white px-3 m-3 link-external-muted"
                    ]
                    [ img
                        [ src "img/logo_mct.svg"
                        , alt "Ministère de la Cohésion des territoires et des Relations avec les collectivités territoriales"
                        , attribute "width" "200"
                        , attribute "height" "200"
                        ]
                        []
                    ]
                , Link.external
                    [ href "https://www.ecologique-solidaire.gouv.fr/fabrique-numerique"
                    , class "col text-center px-3 py-2 link-external-muted"
                    ]
                    [ img
                        [ src "img/logo-fabriquenumerique.svg"
                        , alt "La Fabrique Numérique"
                        , attribute "width" "200"
                        , attribute "height" "200"
                        ]
                        []
                    ]
                ]
            , div [ class "text-center pt-2" ]
                [ text "Un produit "
                , Link.external [ href "https://beta.gouv.fr/startups/wikicarbone.html", class "text-light" ]
                    [ img [ src "img/betagouv.svg", alt "beta.gouv.fr", style "width" "120px" ] [] ]
                ]
            ]
        ]


notFound : Html msg
notFound =
    Container.centered [ class "pb-5" ]
        [ h1 [ class "mb-3" ] [ text "Page non trouvée" ]
        , p [] [ text "La page que vous avez demandé n'existe pas." ]
        , a [ Route.href Route.Home ] [ text "Retour à l'accueil" ]
        ]


loading : Html msg
loading =
    Container.centered [ class "pb-5" ]
        [ Spinner.view
        ]


mobileNavigation : Config msg -> Html msg
mobileNavigation { activePage, closeMobileNavigation } =
    div []
        [ div
            [ class "offcanvas offcanvas-start show"
            , style "visibility" "visible"
            , id "navigation"
            , attribute "tabindex" "-1"
            , attribute "aria-labelledby" "navigationLabel"
            , attribute "arial-modal" "true"
            , attribute "role" "dialog"
            ]
            [ div [ class "offcanvas-header" ]
                [ h5 [ class "offcanvas-title", id "navigationLabel" ]
                    [ text "Navigation" ]
                , button
                    [ type_ "button"
                    , class "btn-close text-reset"
                    , attribute "aria-label" "Close"
                    , onClick closeMobileNavigation
                    ]
                    []
                ]
            , div [ class "offcanvas-body" ]
                [ footerMenuLinks
                    |> List.map (viewNavigationLink activePage)
                    |> div [ class "nav nav-pills flex-column" ]
                ]
            ]
        , div [ class "offcanvas-backdrop fade show" ] []
        ]

module Page.Auth exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Data.Env as Env
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Request.Auth as AuthRequest
import Request.Common as RequestCommon
import Route
import Static.Json exposing (RawJsonProcesses)
import Views.Alert as Alert
import Views.Container as Container
import Views.Markdown as Markdown


type alias Model =
    { authenticated : Bool
    , currentTab : Tab
    , formErrors : AuthRequest.Errors
    , user : User
    }


type Msg
    = AskForRegistration
    | Authenticated User (Result Http.Error RawJsonProcesses)
    | ChangeAction Tab
    | GotProfile (Result Http.Error User)
    | LoggedOut
    | Login
    | Logout
    | TokenEmailSent (Result Http.Error AuthRequest.AuthResponse)
    | UpdateForm Model


type Tab
    = RegistrationTab
    | AuthenticationTab


init : Session -> { authenticated : Bool } -> ( Model, Session, Cmd Msg )
init session data =
    ( emptyModel data
    , session
    , AuthRequest.user GotProfile
    )



-- Auth flow:
-- 1/ ask for login:
--    - ask for a connection link via email: should receive an email with a login link
--    - once the link in the email received is clicked, the backend will redirect to /#/auth/authenticated
--      - GET the user info (to make sure the user is connected)
--      - load the full processes with impacts
-- 2/ register:
--    - ask for registration with email, firstname, lastname, cgu (company): should receive en email with a validation link
--    - once the link in the email received is clicked, may not go through the login flow


emptyModel : { authenticated : Bool } -> Model
emptyModel { authenticated } =
    { user =
        { email = ""
        , firstname = ""
        , lastname = ""
        , company = ""
        , cgu = False
        , token = ""
        }
    , formErrors = Dict.empty
    , currentTab = RegistrationTab
    , authenticated = authenticated
    }


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update session msg model =
    case msg of
        AskForRegistration ->
            ( model
            , session
            , User.form model.user
                |> User.encodeForm
                |> AuthRequest.register TokenEmailSent
            )

        Authenticated user (Ok newProcessesJson) ->
            ( model
            , Session.authenticated session user newProcessesJson
            , Cmd.none
            )

        Authenticated _ (Err error) ->
            ( model
            , session
                |> Session.notifyError
                    "Impossible de charger les impacts lors de la connexion"
                    (RequestCommon.errorToString error)
            , Cmd.none
            )

        ChangeAction action ->
            ( { model | currentTab = action, formErrors = Dict.empty }
            , session
            , Cmd.none
            )

        GotProfile (Ok user) ->
            ( { model | user = user }
            , session
            , user.token
                |> AuthRequest.processes (Authenticated user)
            )

        GotProfile (Err err) ->
            ( { model | authenticated = False }
            , if model.authenticated then
                -- We're here following a click on a login link in an email. If we failed, notify the user.
                session
                    |> Session.notifyError "Erreur lors du login" (RequestCommon.errorToString err)

              else
                session
                    |> Session.logout
            , Cmd.none
            )

        LoggedOut ->
            ( { model | formErrors = Dict.empty }
            , session
            , Cmd.none
            )

        Login ->
            ( model
            , session
            , AuthRequest.login TokenEmailSent model.user.email
            )

        Logout ->
            ( model
            , Session.logout session
                |> Session.notifyInfo "Vous êtes désormais déconnecté" "Vous n'avez plus accès au détail des impacts."
            , AuthRequest.logout LoggedOut
            )

        TokenEmailSent (Ok (AuthRequest.SuccessResponse message)) ->
            ( model
            , session
                |> Session.notifyInfo "Authentification" ("Si vous êtes inscrit(e), un email vous a été envoyé avec un lien de connexion. " ++ message)
            , Cmd.none
            )

        TokenEmailSent (Ok (AuthRequest.ErrorResponse message errors)) ->
            ( { model | formErrors = errors }
            , session
                |> Session.notifyError "Erreur(s) rencontrée(s)" message
            , Cmd.none
            )

        TokenEmailSent (Err httpError) ->
            ( model
            , session
                |> Session.notifyError "Erreur lors de la connexion" (RequestCommon.errorToString httpError)
            , Cmd.none
            )

        UpdateForm newModel ->
            ( newModel
            , session
            , Cmd.none
            )


view : Session -> Model -> ( String, List (Html Msg) )
view session model =
    ( "Authentification"
    , [ Container.centered [ class "pb-5" ]
            [ div [ class "row" ]
                [ div [ class "col-lg-10 offset-lg-1 col-xl-8 offset-xl-2 d-flex flex-column gap-3" ]
                    [ h1 []
                        [ text <|
                            if Session.isAuthenticated session then
                                "Compte"

                            else
                                "Connexion / Inscription"
                        ]
                    , case Session.getUser session of
                        Just user ->
                            div []
                                [ if model.authenticated then
                                    Alert.simple
                                        { level = Alert.Success
                                        , close = Nothing
                                        , title = Nothing
                                        , content =
                                            [ div [ class "fs-7" ]
                                                [ """Vous avez maintenant accès au détail des impacts, à utiliser conformément aux
                                                [conditions d'utilisation des données]({url})."""
                                                    |> String.replace "{url}" Env.cguUrl
                                                    |> Markdown.simple []
                                                ]
                                            ]
                                        }

                                  else
                                    text ""
                                , viewAccount user
                                , div [ class "d-flex justify-content-center align-items-center gap-3" ]
                                    [ a [ Route.href Route.Home ]
                                        [ text "Retour à l'accueil" ]
                                    , button [ class "btn btn-primary my-3", onClick Logout ]
                                        [ text "Déconnexion" ]
                                    ]
                                ]

                        Nothing ->
                            div []
                                [ Alert.simple
                                    { level = Alert.Info
                                    , close = Nothing
                                    , title = Nothing
                                    , content =
                                        [ div [ class "fs-7" ]
                                            [ """Pour avoir accès au détail des impacts, il est nécessaire de s'enregistrer et d'approuver les [conditions d'utilisation]({url}), incluant notamment une utilisation strictement limitée aux produits textiles vendus sur le marché français."""
                                                |> String.replace "{url}" Env.cguUrl
                                                |> Markdown.simple []
                                            ]
                                        ]
                                    }
                                , viewLoginRegisterForm model
                                ]
                    ]
                ]
            ]
      ]
    )


viewAccount : User -> Html Msg
viewAccount user =
    div [ class "table-responsive border shadow-sm" ]
        [ table [ class "table table-striped mb-0" ]
            [ [ ( "Email", text user.email )
              , ( "Nom", text user.lastname )
              , ( "Prénom", text user.firstname )
              , ( "Organisation", text user.company )
              , ( "Jeton d'API"
                , div []
                    [ code [] [ text user.token ]
                    , br [] []
                    , small [ class "text-muted" ]
                        [ text "Nécessaire pour obtenir les impacts détaillés dans "
                        , a [ Route.href Route.Api ] [ text "l'API" ]
                        ]
                    ]
                )
              ]
                |> List.map
                    (\( label, htmlValue ) ->
                        tr []
                            [ th [] [ text <| label ++ " : " ]
                            , td [] [ htmlValue ]
                            ]
                    )
                |> tbody []
            ]
        ]


viewLoginRegisterForm : Model -> Html Msg
viewLoginRegisterForm model =
    div [ class "card shadow-sm px-0" ]
        [ div [ class "card-header px-0 pb-0 border-bottom-0" ]
            [ ul [ class "Tabs nav nav-tabs nav-fill justify-content-end gap-2 px-2" ]
                ([ ( "Inscription", RegistrationTab )
                 , ( "Connexion", AuthenticationTab )
                 ]
                    |> List.map
                        (\( label, action ) ->
                            li
                                [ class "TabsTab nav-item"
                                , classList [ ( "active", model.currentTab == action ) ]
                                ]
                                [ button
                                    [ class "nav-link no-outline border-top-0"
                                    , classList [ ( "active", model.currentTab == action ) ]
                                    , onClick (ChangeAction action)
                                    ]
                                    [ text label ]
                                ]
                        )
                )
            ]
        , div [ class "card-body" ]
            [ case model.currentTab of
                RegistrationTab ->
                    viewRegistrationForm model

                AuthenticationTab ->
                    viewLoginForm model
            ]
        ]


viewInput :
    { label : String
    , type_ : String
    , id : String
    , placeholder : String
    , required : Bool
    , value : String
    , onInput : String -> Msg
    }
    -> AuthRequest.Errors
    -> Html Msg
viewInput inputData formErrors =
    let
        error =
            getInputError inputData.id formErrors
    in
    div [ class "mb-3" ]
        [ label
            [ for inputData.id
            , class "form-label"
            ]
            [ text inputData.label ]
        , input
            [ type_ inputData.type_
            , class "form-control"
            , classList [ ( "is-invalid", error /= Nothing ) ]
            , id inputData.id
            , placeholder inputData.placeholder
            , required inputData.required
            , value inputData.value
            , onInput inputData.onInput
            ]
            []
        , div [ class "text-danger" ]
            [ error
                |> Maybe.withDefault ""
                |> text
            ]
        ]


viewLoginForm : Model -> Html Msg
viewLoginForm ({ user } as model) =
    Html.form [ onSubmit Login ]
        [ viewInput
            { label = "Adresse e-mail"
            , type_ = "email"
            , id = "email"
            , placeholder = "nom@example.com"
            , required = True
            , value = user.email
            , onInput = \email -> UpdateForm { model | user = { user | email = email } }
            }
            model.formErrors
        , button
            [ type_ "submit"
            , class "btn btn-primary mb-3"
            , disabled <| String.isEmpty user.email
            ]
            [ text "Connexion" ]
        ]


viewRegistrationForm : Model -> Html Msg
viewRegistrationForm ({ user } as model) =
    div []
        [ Html.form [ onSubmit AskForRegistration ]
            [ div [ class "row" ]
                [ div [ class "col-sm-6" ]
                    [ viewInput
                        { label = "Adresse e-mail"
                        , type_ = "text"
                        , id = "email"
                        , placeholder = "nom@example.com"
                        , required = True
                        , value = user.email
                        , onInput = \email -> UpdateForm { model | user = { user | email = email } }
                        }
                        model.formErrors
                    ]
                , div [ class "col-sm-6" ]
                    [ viewInput
                        { label = "Organisation"
                        , type_ = "text"
                        , id = "company"
                        , placeholder = "ACME SARL"
                        , required = False
                        , value = user.company
                        , onInput = \company -> UpdateForm { model | user = { user | company = company } }
                        }
                        model.formErrors
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-sm-6" ]
                    [ viewInput
                        { label = "Prénom"
                        , type_ = "text"
                        , id = "first_name"
                        , placeholder = "Joséphine"
                        , required = True
                        , value = user.firstname
                        , onInput = \firstname -> UpdateForm { model | user = { user | firstname = firstname } }
                        }
                        model.formErrors
                    ]
                , div [ class "col-sm-6" ]
                    [ viewInput
                        { label = "Nom"
                        , type_ = "text"
                        , id = "last_name"
                        , placeholder = "Durand"
                        , required = True
                        , value = user.lastname
                        , onInput = \lastname -> UpdateForm { model | user = { user | lastname = lastname } }
                        }
                        model.formErrors
                    ]
                ]
            , div []
                [ label
                    [ for "terms_of_use"
                    , class "form-check form-switch form-check-label pt-1"
                    ]
                    [ input
                        [ type_ "checkbox"
                        , class "form-check-input"
                        , classList [ ( "is-invalid", getInputError "terms_of_use" model.formErrors /= Nothing ) ]
                        , id "terms_of_use"
                        , required True
                        , checked user.cgu
                        , onCheck (\isChecked -> UpdateForm { model | user = { user | cgu = isChecked } })
                        ]
                        []
                    , div []
                        [ """Je m’engage à respecter les [conditions d'utilisation]({url})"""
                            |> String.replace "{url}" Env.cguUrl
                            |> Markdown.simple []
                        ]
                    , div [ class "text-danger" ]
                        [ getInputError "terms_of_use" model.formErrors
                            |> Maybe.withDefault ""
                            |> text
                        ]
                    ]
                , div [ class "d-none" ]
                    [ label [ for "nextInput", class "form-label" ] []
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "nextInput"
                        , required True
                        , value "/#/auth/authenticated"
                        , hidden True
                        ]
                        []
                    ]
                ]
            , div [ class "text-center mt-3" ]
                [ button
                    [ type_ "submit"
                    , class "btn btn-primary"
                    ]
                    [ text "Créer mon compte" ]
                ]
            ]
        ]


getInputError : String -> AuthRequest.Errors -> Maybe String
getInputError id formErrors =
    Dict.get id formErrors

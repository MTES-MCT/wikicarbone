module Page.Textile.Examples exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Data.Impact as Impact
import Data.Impact.Definition as Definition
import Data.Session as Session exposing (Session)
import Data.Textile.Inputs as Inputs
import Data.Textile.Simulator as Simulator
import Html exposing (..)
import Html.Attributes exposing (..)
import Ports
import Views.Container as Container
import Views.Impact as ImpactView
import Views.ImpactTabs as ImpactTabs
import Views.Textile.Summary as SummaryView


type alias Model =
    { impact : Definition.Trigram
    , activeImpactsTab : ImpactTabs.Tab
    }


type Msg
    = SwitchImpact (Result String Definition.Trigram)
    | SwitchImpactsTab ImpactTabs.Tab


init : Session -> ( Model, Session, Cmd Msg )
init session =
    ( { impact = Impact.default
      , activeImpactsTab = ImpactTabs.SubscoresTab
      }
    , session
    , Ports.scrollTo { x = 0, y = 0 }
    )


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update session msg model =
    case msg of
        SwitchImpact (Ok impact) ->
            ( { model | impact = impact }, session, Cmd.none )

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


viewExample : Session -> Model -> Definition.Trigram -> Inputs.Query -> Html Msg
viewExample session model impact query =
    query
        |> Simulator.compute session.textileDb
        |> SummaryView.view
            { session = session
            , impact = Definition.get impact session.textileDb.impactDefinitions
            , reusable = True
            , activeImpactsTab = model.activeImpactsTab
            , switchImpactsTab = SwitchImpactsTab
            }
        |> div [ class "col" ]


view : Session -> Model -> ( String, List (Html Msg) )
view session ({ impact } as model) =
    ( "Exemples"
    , [ Container.centered [ class "pb-3" ]
            [ div [ class "row" ]
                [ div [ class "col-md-7 mb-2" ]
                    [ h1 [] [ text "Exemples de simulation" ] ]
                , div [ class "col-md-5 mb-2 d-flex align-items-center" ]
                    [ ImpactView.selector
                        session.textileDb.impactDefinitions
                        { selectedImpact = impact
                        , switchImpact = SwitchImpact
                        }
                    ]
                ]
            , Definition.get impact session.textileDb.impactDefinitions
                |> ImpactView.viewDefinition
            , Inputs.presets
                |> List.map (viewExample session model impact)
                |> div [ class "row row-cols-1 row-cols-md-2 row-cols-xl-3 g-4" ]
            ]
      ]
    )

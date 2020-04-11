module Pages.Home exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Browser.Navigation as Nav
import Flags exposing (Flags)
import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (GameState, randomGame)
import Hanabi.MVC.API exposing (conn)
import Html exposing (Html, br, button, div, input, text)
import Html.Attributes exposing (disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Pages.Routes as Routes
import Random
import StateServer as SS


type alias Model =
    { flags : Flags
    , gameId : SS.Name
    , players : List String
    , working : Bool
    , navKey : Nav.Key
    }


type Msg
    = SetPlayers (List String)
    | SetGameId String
    | Join
    | LoadedGame (Result Http.Error (Maybe History))
    | Create
    | RandomGameGenerated GameState


init : Flags -> Nav.Key -> ( Model, Cmd Msg )
init flags key =
    ( { flags = flags
      , gameId = ""
      , players = []
      , working = False
      , navKey = key
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        connection =
            conn model.flags.stateServerRoot model.gameId
    in
    case msg of
        SetPlayers players ->
                ( { model | players = players }
                , Cmd.none
                )

        SetGameId gameId ->
                ( { model | gameId = gameId }
                , Cmd.none
                )

        Join ->
                ( model
                , SS.get LoadedGame connection
                )

        Create ->
                ( model
                , Random.generate RandomGameGenerated (randomGame model.players)
                )

        RandomGameGenerated g ->
                ( model
                , SS.create LoadedGame connection (Just { init = g, moves = [] })
                )

        LoadedGame (Ok _) ->
            ( model
            , Nav.pushUrl model.navKey <| Routes.toNavString <| Routes.PlayerSelect { gameId = model.gameId }
            )

        LoadedGame (Err _) ->
                ( { model | working = False }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div []
        [ text "Game id:"
        , input
            [ onInput SetGameId
            , value model.gameId
            , placeholder "someGameId"
            , disabled model.working
            ]
            []
        , button [ onClick Join, disabled model.working ] [ text "Join" ]
        , br [] []
        , text "or"
        , br [] []
        , text "Players: "
        , input
            [ onInput (String.split "," >> List.map String.trim >> SetPlayers)
            , value (model.players |> String.join ", ")
            , placeholder "Alice, Bob, Charlie"
            , disabled model.working
            ]
            []
        , button [ onClick Create, disabled model.working ] [ text "Create" ]
        ]

module Pages.Home exposing
    ( Model
    , Msg(..)
    , init
    , view
    , update
    )

import Http
import Html exposing (Html, div, button, text, input, br)
import Html.Attributes exposing (value, placeholder, style, disabled)
import Html.Events exposing (onClick, onInput)
import Url

import Hanabi.Core exposing (GameState, randomGame)
import Random
import Hanabi.Assistance exposing (History)
import Hanabi.MVC.API exposing (conn)
import StateServer as SS
import Flags exposing (Flags)
import Pages.Routes as Routes exposing (Escaped(..))

type alias Model =
    { flags : Flags
    , gameId : SS.Name
    , players : List String
    , working : Bool
    }

type Msg
    = SetPlayers (List String)
    | SetGameId String
    | Join
    | LoadedGame (Result Http.Error (Maybe History))
    | Create
    | RandomGameGenerated GameState

init : Flags -> (Model, Cmd Msg)
init flags =
    ( { flags = flags
      , gameId = ""
      , players = []
      , working = False
      }
    , Cmd.none
    )

update : Msg -> Model -> Escaped (Model, Cmd Msg)
update msg model =
    let
        connection = conn model.flags.stateServerRoot model.gameId
    in
    case msg of
        SetPlayers players ->
            Stay <|
            ( { model | players=players }
            , Cmd.none
            )
        SetGameId gameId ->
            Stay <|
            ( { model | gameId=gameId }
            , Cmd.none
            )
        Join ->
            Stay <|
            ( model
            , SS.get LoadedGame connection
            )
        Create ->
            Stay <|
            ( model
            , Random.generate RandomGameGenerated (randomGame model.players)
            )
        RandomGameGenerated g ->
            Stay <|
            ( model
            , SS.create LoadedGame connection (Just {init=g, moves=[]})
            )
        LoadedGame (Ok _) ->
            Escape (Routes.PlayerSelect {gameId=model.gameId})
        LoadedGame (Err err) ->
            Stay <|
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
            , placeholder "srpplayground"
            , disabled model.working
            ]
            []
        , button [ onClick Join, disabled model.working ] [text "Join"]
        , br [] [], text "or", br [] []
        , text "Players: "
        , input
            [ onInput (String.split "," >> List.map (String.trim) >> SetPlayers)
            , value (model.players |> String.join ", ")
            , placeholder "Alice, Bob, Charlie"
            , disabled model.working
            ]
            []
        , button [ onClick Create, disabled model.working ] [text "Create"]
        ]

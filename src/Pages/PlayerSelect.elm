module Pages.PlayerSelect exposing
    ( Model
    , Msg(..)
    , init
    , view
    , update
    )

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)

import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (Player)
import Hanabi.MVC.API exposing (conn)

import Pages.Routes as Routes exposing (Escaped(..), PlayerSelectPageFlags)

import StateServer as SS
import Flags exposing (Flags)
import Http

type alias Model =
    { flags : Flags
    , gameId : SS.Name
    , players : Maybe (List Player)
    }

type Msg
    = SetPlayer Player
    | LoadedGame (Result Http.Error History)


init : Flags -> PlayerSelectPageFlags -> (Model, Cmd Msg)
init flags pageFlags =
    ( { flags = flags
      , gameId = pageFlags.gameId
      , players = Nothing
      }
    , SS.get LoadedGame (conn flags.stateServerRoot pageFlags.gameId)
    )

update : Msg -> Model -> Escaped (Model, Cmd Msg)
update msg model =
    let
        connection = conn model.flags.stateServerRoot model.gameId
    in
    case msg of
        LoadedGame (Ok history) ->
            Stay <|
            ( { model | players = Just history.init.players }
            , Cmd.none
            )

        LoadedGame (Err e) ->
            Debug.todo "loading failed"

        SetPlayer player ->
            Escape (Routes.Play {gameId=model.gameId, player=player})



view : Model -> Html Msg
view model =
    case model.players of
        Nothing -> text "loading..."
        Just players ->
            div []
                (text "You are: " :: (players
                                      |> List.map (\p -> button [onClick <| SetPlayer p] [text p])
                                      |> List.intersperse (text " or ")))

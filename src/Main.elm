import Browser
import Dict
import Http
import Json.Encode as E

import Random
import Hanabi.Core exposing (randomGame)
import Hanabi.MVC.API exposing (postNewGame, pollForHistory, encodeHistory)
import Hanabi.MVC.Core exposing (Msg(..), Model(..), init)
import Hanabi.MVC.View exposing (view)

-------------------------------------------------------------------
-- MVC

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model, msg) of
        -- Creating
        (Creating state, SetPlayers s) -> (Creating {state | players = s}, Cmd.none)
        (Creating state, Create) ->
            let
                players = List.map String.trim <| String.split "," state.players
            in
                ( Creating {players=state.players}
                , Random.generate RandomGameGenerated (randomGame players)
                )
        (Creating state, RandomGameGenerated g) -> (model, postNewGame "TODO" g)
        (Creating state, NewGamePosted (Ok history)) ->
            (ChoosingPlayer {gameId = "TODO", history = history}, Cmd.none)
        (Creating state, NewGamePosted (Err err)) ->
            (Debug.log (Debug.toString err) model, Cmd.none)

        -- ChoosingPlayer
        (ChoosingPlayer state, SetPlayer p) ->
            ( Playing {gameId=state.gameId, player=p, history=state.history, freezeFrame=Nothing}
            , pollForHistory state.gameId state.history
            )

        -- Playing
        (Playing state, SetHistory result) ->
            let
                history = case Debug.log (Debug.toString result) result of
                    Ok h -> h
                    Err e -> Debug.log (Debug.toString ("Error fetching history", e)) state.history
            in
                ( Playing { state | history = history }
                , pollForHistory state.gameId history
                )
        (Playing state, SetFreezeFrame t) -> (Playing { state | freezeFrame = t }, Cmd.none)
        (Playing state, MakeMove move) ->
            let
                oldHistory = state.history
                newHistory = { oldHistory | moves = oldHistory.moves ++ [move] }
            in
                ( Playing { state | history = newHistory }
                , Http.post
                    { url = "/states/" ++ state.gameId
                    , body = Http.jsonBody (E.object [("old", encodeHistory oldHistory), ("new", encodeHistory newHistory)])
                    , expect = Http.expectWhatever (always MadeMove)
                    }
                )
        (Playing state, MadeMove) -> (Playing state, Cmd.none)

        (a, b) ->
            Debug.todo (Debug.toString ("unhandled message", a, b))

main = Browser.element {init=init, update=update, view=view, subscriptions=(always Sub.none)}

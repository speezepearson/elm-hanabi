import Browser
import Dict
import Random

import StateServer as SS
import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (randomGame)
import Hanabi.MVC.API exposing (conn)
import Hanabi.MVC.Core exposing (Msg(..), Model(..), init)
import Hanabi.MVC.View exposing (view)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model, msg) of
        -- Creating
        (Creating state, SetPlayers s) -> (Creating {state | players = s}, Cmd.none)
        (Creating state, SetGameId id) -> (Creating {state | gameId = id}, Cmd.none)
        (Creating state, Join) -> (Creating state, SS.get SetHistory (conn state.gameId))
        (Creating state, Create) ->
            let
                players = List.map String.trim <| String.split "," state.players
            in
                ( Creating state
                , Random.generate RandomGameGenerated (randomGame players)
                )
        (Creating state, RandomGameGenerated g) ->
            ( model
            , SS.create SetHistory (conn state.gameId) {init=g, moves=[]}
            )
        (Creating state, SetHistory (Ok history)) ->
            (ChoosingPlayer {conn = conn state.gameId, history = history}, Cmd.none) -- TODO race condition with HTTP vs input to gameId field
        (Creating state, SetHistory (Err err)) ->
            (Debug.log (Debug.toString err) model, Cmd.none)

        -- ChoosingPlayer
        (ChoosingPlayer state, SetPlayer p) ->
            ( Playing {conn=state.conn, player=p, history=state.history, freezeFrame=Nothing}
            , SS.poll SetHistory state.conn state.history
            )

        -- Playing
        (Playing state, SetHistory result) ->
            let
                history = case Debug.log (Debug.toString result) result of
                    Ok h -> h
                    Err e -> Debug.log (Debug.toString ("Error fetching history", e)) state.history
            in
                ( Playing { state | history = history }
                , SS.poll SetHistory state.conn history
                )
        (Playing state, SetFreezeFrame t) -> (Playing { state | freezeFrame = t }, Cmd.none)
        (Playing state, MakeMove move) ->
            let
                oldHistory = state.history
                newHistory = { oldHistory | moves = oldHistory.moves ++ [move] }
            in
                ( Playing { state | history = newHistory }
                , SS.update (always MadeMove) state.conn oldHistory newHistory
                )
        (Playing state, MadeMove) -> (Playing state, Cmd.none)

        (a, b) ->
            Debug.todo (Debug.toString ("unhandled message", a, b))

main = Browser.element {init=init, update=update, view=view, subscriptions=(always Sub.none)}

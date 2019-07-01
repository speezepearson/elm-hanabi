import Browser
import Dict
import Random

import StateServer as SS
import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (randomGame)
import Hanabi.MVC.API exposing (encodeHistory, historyDecoder)
import Hanabi.MVC.Core exposing (Msg(..), Model(..), init)
import Hanabi.MVC.View exposing (view)

conn : SS.Connection History
conn =
    { encode = encodeHistory
    , decoder= historyDecoder
    , name = "TODO"
    }

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
        (Creating state, RandomGameGenerated g) ->
            ( model
            , SS.create SetHistory conn {init=g, moves=[]}
            )
        (Creating state, SetHistory (Ok history)) ->
            (ChoosingPlayer {gameId = "TODO", history = history}, Cmd.none)
        (Creating state, SetHistory (Err err)) ->
            (Debug.log (Debug.toString err) model, Cmd.none)

        -- ChoosingPlayer
        (ChoosingPlayer state, SetPlayer p) ->
            ( Playing {gameId=state.gameId, player=p, history=state.history, freezeFrame=Nothing}
            , SS.poll SetHistory conn state.history
            )

        -- Playing
        (Playing state, SetHistory result) ->
            let
                history = case Debug.log (Debug.toString result) result of
                    Ok h -> h
                    Err e -> Debug.log (Debug.toString ("Error fetching history", e)) state.history
            in
                ( Playing { state | history = history }
                , SS.poll SetHistory conn history
                )
        (Playing state, SetFreezeFrame t) -> (Playing { state | freezeFrame = t }, Cmd.none)
        (Playing state, MakeMove move) ->
            let
                oldHistory = state.history
                newHistory = { oldHistory | moves = oldHistory.moves ++ [move] }
            in
                ( Playing { state | history = newHistory }
                , SS.update (always MadeMove) conn oldHistory newHistory
                )
        (Playing state, MadeMove) -> (Playing state, Cmd.none)

        (a, b) ->
            Debug.todo (Debug.toString ("unhandled message", a, b))

main = Browser.element {init=init, update=update, view=view, subscriptions=(always Sub.none)}

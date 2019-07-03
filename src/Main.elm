import Browser
import Dict
import Random

import StateServer as SS
import Hanabi.Assistance exposing (History, run)
import Hanabi.Core exposing (randomGame, currentPlayer)
import Hanabi.MVC.API exposing (conn)
import Hanabi.MVC.Core exposing (Msg(..), Model(..), init)
import Hanabi.MVC.View exposing (view)
import Ports exposing (notify)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Creating state ->
            case msg of
                SetPlayers s -> (Creating {state | players = s}, Cmd.none)
                SetGameId id -> (Creating {state | gameId = id}, Cmd.none)
                Join -> (Creating state, SS.get SetHistory (conn state.gameId))
                Create ->
                    let
                        players = List.map String.trim <| String.split "," state.players
                    in
                        ( model
                        , Random.generate RandomGameGenerated (randomGame players)
                        )
                RandomGameGenerated g ->
                    ( model
                    , SS.create SetHistory (conn state.gameId) {init=g, moves=[]}
                    )
                SetHistory (Ok history) ->
                    (ChoosingPlayer {conn = conn state.gameId, history = history}, Cmd.none) -- TODO race condition with HTTP vs input to gameId field
                SetHistory (Err err) ->
                    (Debug.log (Debug.toString err) model, Cmd.none)
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

        ChoosingPlayer state ->
            case msg of
                SetPlayer p ->
                    ( Playing {conn=state.conn, player=p, history=state.history, freezeFrame=Nothing, polling=True}
                    , SS.poll SetHistory state.conn state.history
                    )
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

        Playing state ->
            case msg of
                SetHistory (Ok history) ->
                    ( Playing { state | history = history }
                    , Cmd.batch [ SS.poll SetHistory state.conn history
                                , if currentPlayer (run history) == state.player then notify "Your turn!"
                                  else Cmd.none
                                ]
                    )

                SetHistory (Err e) ->
                    ( Playing { state | polling = False }
                    , notify <| "Error talking to server: " ++ Debug.log "" (Debug.toString e)
                    )
                SetFreezeFrame t -> (Playing { state | freezeFrame = t }, Cmd.none)
                MakeMove move ->
                    let
                        oldHistory = state.history
                        newHistory = { oldHistory | moves = oldHistory.moves ++ [move] }
                    in
                        ( Playing { state | history = newHistory }
                        , SS.update (always MadeMove) state.conn oldHistory newHistory
                        )
                MadeMove -> (Playing state, Cmd.none)
                Poll -> (Playing { state | polling = True }, SS.poll SetHistory state.conn state.history )
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

main = Browser.element {init=init, update=update, view=view, subscriptions=(always Sub.none)}

import Browser
import Browser.Navigation as Nav
import Dict
import Random
import Url

import StateServer as SS
import Hanabi.Assistance exposing (History, run)
import Hanabi.Core exposing (randomGame, currentPlayer)
import Hanabi.MVC.API exposing (conn)
import Hanabi.MVC.Core exposing (Msg(..), User, AppModel, PageModel(..))
import Hanabi.MVC.View exposing (view)
import Ports exposing (notify)
import Routes exposing (toRoute)

init : () -> Url.Url -> Nav.Key -> (AppModel, Cmd Msg)
init flags url key =
    case toRoute (Debug.log "init url" url) of
        Routes.Home ->
            ( {navKey = key, user=Nothing, page=Creating {gameId = "", players = ""}}
            , Cmd.none
            )
        Routes.Game gid player ->
            let
                user: Maybe User
                user = Maybe.map User player

                appModel : PageModel -> AppModel
                appModel page = {navKey = key, user = user, page = page}

                intention = case player of
                    Nothing -> (\h ->
                        ( appModel <| ChoosingPlayer {conn=conn gid, history=h}
                        , Cmd.none
                        ))
                    Just p -> (\h ->
                        ( appModel <| Playing {conn=conn gid, history=h, freezeFrame=Nothing, player=p, polling=True}
                        , SS.poll LoadedGame (conn gid) h
                        ))
            in
                ( appModel <| LoadingGame {conn=(conn gid), intention=intention}
                , SS.get LoadedGame (conn gid)
                )
        Routes.NotFound ->
            ( {navKey = key, user=Nothing, page=Creating {gameId = "", players = ""}}
            , Cmd.none
            )

update : Msg -> AppModel -> (AppModel, Cmd Msg)
update msg model =
    let
        still : PageModel -> AppModel
        still p = { model | page = p }
    in
    case model.page of
        Creating state ->
            case msg of
                SetPlayers s -> (still <| Creating {state | players = s}, Cmd.none)
                SetGameId id -> (still <| Creating {state | gameId = id}, Cmd.none)
                Join -> (still <| Creating state, SS.get LoadedGame (conn state.gameId))
                Create ->
                    let
                        players = List.map String.trim <| String.split "," state.players
                    in
                        ( model
                        , Random.generate RandomGameGenerated (randomGame players)
                        )
                RandomGameGenerated g ->
                    ( model
                    , SS.create LoadedGame (conn state.gameId) {init=g, moves=[]}
                    )
                LoadedGame (Ok history) ->
                    (still <| ChoosingPlayer {conn = conn state.gameId, history = history}, Cmd.none) -- TODO race condition with HTTP vs input to gameId field
                LoadedGame (Err err) ->
                    (Debug.log (Debug.toString err) model, Cmd.none)
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

        LoadingGame {conn, intention} ->
            case msg of
                LoadedGame (Ok h) -> intention h
                LoadedGame (Err e) ->
                    ( still <| LoadingFailed { message = Debug.toString e, conn=conn, intention = intention}
                    , Cmd.none
                    )
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

        LoadingFailed {conn, intention} ->
            case msg of
                RetryLoadGame ->
                    ( still <| LoadingGame {conn=conn, intention = intention}
                    , SS.get LoadedGame conn
                    )
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

        ChoosingPlayer state ->
            case msg of
                SetPlayer p ->
                    ( { model | page = Playing {conn=state.conn, player=p, history=state.history, freezeFrame=Nothing, polling=True}
                              , user = Just {name=p}
                      }
                    , SS.poll LoadedGame state.conn state.history
                    )
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

        Playing state ->
            case msg of
                LoadedGame (Ok history) ->
                    ( still <| Playing { state | history = history }
                    , Cmd.batch [ SS.poll LoadedGame state.conn history
                                , if currentPlayer (run history) == state.player then notify "Your turn!"
                                  else Cmd.none
                                ]
                    )

                LoadedGame (Err e) ->
                    ( still <| Playing { state | polling = False }
                    , notify <| "Error talking to server: " ++ Debug.log "" (Debug.toString e)
                    )
                SetFreezeFrame t -> (still <| Playing { state | freezeFrame = t }, Cmd.none)
                MakeMove move ->
                    let
                        oldHistory = state.history
                        newHistory = { oldHistory | moves = oldHistory.moves ++ [move] }
                    in
                        ( still <| Playing { state | history = newHistory }
                        , SS.update (always MadeMove) state.conn oldHistory newHistory
                        )
                MadeMove -> (still <| Playing state, Cmd.none)
                Poll ->
                    ( still <| Playing { state | polling = True }
                    , SS.poll LoadedGame state.conn state.history
                    )
                _ -> Debug.todo (Debug.toString ("unhandled message", model, msg))

main = Browser.application
    { init = init
    , update = update
    , view = view >> (\html -> {title="Hanabi", body=[html]})
    , subscriptions = (always Sub.none)
    , onUrlRequest = UrlRequested
    , onUrlChange = UrlChanged
    }

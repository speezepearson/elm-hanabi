module Hanabi.MVC.Core exposing (..)

import Http

import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (GameState, Player, Move)
import StateServer as SS

type alias Connection = SS.Connection History

type alias TimeStep = Int

type Model
    = Creating {gameId: SS.Name, players: String}
    | ChoosingPlayer {conn: Connection, history: History}
    | Playing {conn: Connection, player: Player, history: History, freezeFrame: Maybe TimeStep}

type Msg
    -- Creating
    = SetPlayers String
    | SetGameId SS.Name
    | Join
    | Create
    | RandomGameGenerated GameState
    | NewGamePosted (Result Http.Error History)
    -- ChoosingPlayer
    | SetPlayer Player
    -- Playing
    | MakeMove Move
    | SetHistory (Result Http.Error History)
    | SetFreezeFrame (Maybe TimeStep)
    -- Misc
    | MadeMove

init : () -> (Model, Cmd Msg)
init _ = (Creating {gameId = "", players = ""}, Cmd.none)

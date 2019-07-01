module Hanabi.MVC.Core exposing (..)

import Http

import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (GameState, Player, Move)

type alias TimeStep = Int

type alias GameId = String
type Model = Creating {players: String}
    | ChoosingPlayer {gameId: GameId, history: History}
    | Playing {gameId: GameId, player: Player, history: History, freezeFrame: Maybe TimeStep}

type Msg
    -- Creating
    = SetPlayers String
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
init _ = (Creating {players = ""}, Cmd.none)

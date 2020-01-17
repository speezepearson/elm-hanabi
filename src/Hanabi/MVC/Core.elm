module Hanabi.MVC.Core exposing (..)

import Browser
import Browser.Navigation as Nav
import Http
import Url

import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (GameState, Player, Move)
import StateServer as SS

type alias Connection = SS.Connection (Maybe History)

type alias TimeStep = Int
type alias LoadingIntention = History -> (AppModel, Cmd Msg)

type alias User =
    { name : String
    }

type alias AppModel =
    { navKey : Nav.Key
    , stateServerRoot : String
    , user : Maybe User
    , page : PageModel
    }

type PageModel
    = Creating {gameId: SS.Name, players: String}
    | LoadingGame { conn: Connection, intention : LoadingIntention }
    | LoadingFailed { message : String, conn: Connection, intention : LoadingIntention }
    | ChoosingPlayer {conn: Connection, history: History}
    | Playing {conn: Connection, player: Player, history: History, freezeFrame: Maybe TimeStep, polling: Bool}

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
    | SetFreezeFrame (Maybe TimeStep)
    -- Misc
    | RetryLoadGame
    | LoadedGame (Result Http.Error History)
    | MadeMove
    | Poll
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url

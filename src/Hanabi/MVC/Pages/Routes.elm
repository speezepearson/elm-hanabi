module Hanabi.MVC.Pages.Routes exposing (..)
import Url
import StateServer as SS
import Hanabi.Core exposing (Player)

type alias HomePageFlags = {}
type alias PlayerSelectPageFlags = {gameId : SS.Name}
type alias PlayPageFlags = {gameId : SS.Name, player : Player}

type Escaped a
    = Escape Route
    | Stay a

type Route
    = NotFound
    | Home HomePageFlags
    | PlayerSelect PlayerSelectPageFlags
    | Play PlayPageFlags


fromUrl : Url.Url -> Route
fromUrl {fragment} =
    case fragment |> Maybe.map (String.split "/") of
        Nothing -> Home {}
        Just [""] -> Home {}
        Just [gid] -> PlayerSelect {gameId=gid}
        Just [gid, pid] -> Play {gameId=gid, player=pid}
        _ -> NotFound

toNavString : Route -> String
toNavString route =
    "#" ++
    case route of
        NotFound -> ""
        Home _ -> ""
        PlayerSelect {gameId} -> gameId
        Play {gameId, player} -> gameId ++ "/" ++ player

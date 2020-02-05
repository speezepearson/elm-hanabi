module Pages.Routes exposing
    ( Escaped(..)
    , PlayPageFlags
    , PlayerSelectPageFlags
    , Route(..)
    , fromUrl
    , toNavString
    )

import Hanabi.Core exposing (Player)
import StateServer as SS
import Url
import Url.Parser as UP exposing ((</>))


type alias PlayerSelectPageFlags =
    { gameId : SS.Name }


type alias PlayPageFlags =
    { gameId : SS.Name, player : Player }


type Escaped a
    = Escape Route
    | Stay a


type Route
    = NotFound
    | Home
    | PlayerSelect PlayerSelectPageFlags
    | Play PlayPageFlags


parser : UP.Parser (Route -> a) a
parser =
    UP.oneOf
        [ UP.map Home UP.top
        , UP.map (\gid -> PlayerSelect { gameId = gid }) (UP.s "game" </> UP.string)
        , UP.map (\gid pid -> Play { gameId = gid, player = pid }) (UP.s "game" </> UP.string </> UP.string)
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    url
        |> Debug.log "parsing url"
        |> UP.parse parser
        |> Maybe.withDefault NotFound


toNavString : Route -> String
toNavString route =
    case route of
        NotFound ->
            "/"

        Home ->
            "/"

        PlayerSelect { gameId } ->
            "/game/" ++ gameId

        Play { gameId, player } ->
            "/game/" ++ gameId ++ "/" ++ player

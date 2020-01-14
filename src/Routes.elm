module Routes exposing (toRoute, Route(..))

import Url
import Url.Parser exposing (Parser, parse, string, map, oneOf, s, top, (</>), (<?>))
import Url.Parser.Query as Query

import Hanabi.Core exposing (Player)

import StateServer as SS

type Route = Home | Game SS.Name (Maybe Player) | NotFound

route : Parser (Route -> a) a
route =
    map (\g p -> case (g, p) of
            (Just gid, Just player) -> Game gid (Just player)
            _ -> Home)
        (top <?> Query.string "game" <?> Query.string "player")

toRoute : String -> Url.Url -> Route
toRoute appRoot url =
    let
        strippedPath : String
        strippedPath =
            Debug.log "stripped path is" <|
            if String.startsWith appRoot url.path
                then String.dropLeft (String.length appRoot) url.path
                else url.path
    in
    Maybe.withDefault NotFound (parse route {url | path = strippedPath})

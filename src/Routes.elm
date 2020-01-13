module Routes exposing (toRoute, Route(..))

import Url
import Url.Parser exposing (Parser, parse, string, map, oneOf, s, top, (</>), (<?>))
import Url.Parser.Query as Query

import Hanabi.Core exposing (Player)

import StateServer as SS

type Route = Home | Game SS.Name (Maybe Player) | NotFound

route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Game (s "game" </> string <?> Query.string "player")
        ]

toRoute : String -> Url.Url -> Route
toRoute appRoot url =
    let
        strippedPath : String
        strippedPath =
            if String.startsWith appRoot url.path
                then String.dropLeft (String.length appRoot) url.path
                else url.path
    in
    Maybe.withDefault NotFound (parse route {url | path = strippedPath})

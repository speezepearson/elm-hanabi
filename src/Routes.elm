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
        [ map Home (s "apps" </> s "hanabi")
        , map Game (s "apps" </> s "hanabi" </> s "game" </> string <?> Query.string "player")
        ]

toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)

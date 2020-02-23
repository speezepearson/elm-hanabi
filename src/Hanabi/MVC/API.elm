module Hanabi.MVC.API exposing (..)

import Dict
import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (Card, GameState, Hand, Move(..))
import Json.Decode as D
import Json.Encode as E
import StateServer as SS


type alias ServerState =
    Maybe History


type alias Connection =
    SS.Connection ServerState


conn : String -> SS.Name -> Connection
conn urlRoot name =
    { encode = Maybe.map encodeHistory >> Maybe.withDefault E.null
    , decoder = D.maybe historyDecoder
    , name = name
    , urlRoot = urlRoot
    }


encodeCard : Card -> E.Value
encodeCard { color, rank } =
    E.object [ ( "color", E.string color ), ( "rank", E.int rank ) ]


cardDecoder : D.Decoder Card
cardDecoder =
    D.map2 Card (D.field "color" D.string) (D.field "rank" D.int)


encodeHand : Hand -> E.Value
encodeHand h =
    E.object <| Dict.toList <| mapValues encodeCard h


handDecoder : D.Decoder Hand
handDecoder =
    D.dict cardDecoder


encodeGameState : GameState -> E.Value
encodeGameState g =
    E.object
        [ ( "nFuses", E.int g.nFuses )
        , ( "nHints", E.int g.nHints )
        , ( "towers", E.object <| Dict.toList <| Dict.map (\_ r -> E.int r) g.towers )
        , ( "players", E.list E.string g.players )
        , ( "hands", E.object <| Dict.toList <| Dict.map (\_ h -> encodeHand h) g.hands )
        , ( "deck", E.list encodeCard g.deck )
        , ( "discardPile", E.list encodeCard g.discardPile )
        , ( "movesLeft", Maybe.map E.int g.movesLeft |> Maybe.withDefault E.null )
        ]


gameStateDecoder : D.Decoder GameState
gameStateDecoder =
    D.map8 (\fus hin tow pla han dec dis moves -> { nFuses = fus, nHints = hin, towers = tow, players = pla, hands = han, deck = dec, discardPile = dis, movesLeft = moves })
        (D.field "nFuses" D.int)
        (D.field "nHints" D.int)
        (D.field "towers" <| D.dict D.int)
        (D.field "players" <| D.list D.string)
        (D.field "hands" <| D.dict handDecoder)
        (D.field "deck" <| D.list cardDecoder)
        (D.field "discardPile" <| D.list cardDecoder)
        (D.field "movesLeft" <| D.nullable D.int)


encodeMove : Move -> E.Value
encodeMove m =
    case m of
        Play pos ->
            E.object [ ( "play", E.string pos ) ]

        Discard pos ->
            E.object [ ( "discard", E.string pos ) ]

        HintColor p c ->
            E.object [ ( "hint", E.string p ), ( "color", E.string c ) ]

        HintRank p n ->
            E.object [ ( "hint", E.string p ), ( "rank", E.int n ) ]


moveDecoder : D.Decoder Move
moveDecoder =
    D.oneOf
        [ D.map Play (D.field "play" D.string)
        , D.map Discard (D.field "discard" D.string)
        , D.map2 HintColor (D.field "hint" D.string) (D.field "color" D.string)
        , D.map2 HintRank (D.field "hint" D.string) (D.field "rank" D.int)
        ]


encodeHistory : History -> E.Value
encodeHistory history =
    E.object [ ( "init", encodeGameState history.init ), ( "moves", E.list encodeMove history.moves ) ]


historyDecoder : D.Decoder History
historyDecoder =
    D.map2 History
        (D.field "init" gameStateDecoder)
        (D.field "moves" <| D.list moveDecoder)


mapValues : (a -> b) -> Dict.Dict k a -> Dict.Dict k b
mapValues f d =
    Dict.map (\_ v -> f v) d

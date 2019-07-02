module Hanabi.Core exposing (..)

import Dict
import Random
import Random.List exposing (shuffle)

type alias Color = String
type alias Rank = Int
type alias Card = { color : Color , rank : Rank }
type alias Player = String
type alias CardPosition = String
type alias Hand = Dict.Dict CardPosition Card

type alias GameState =
    { nFuses : Int
    , nHints : Int
    , towers : Dict.Dict Color Rank
    , players : List Player
    , hands : Dict.Dict Player Hand
    , deck : List Card
    }

type Move
    = Play CardPosition
    | Discard CardPosition
    | HintRank Player Rank
    | HintColor Player Color


colors : List Color
colors = ["B", "G", "R", "W", "Y"]

ranks : List Rank
ranks = [1, 2, 3, 4, 5]

posns : List CardPosition
posns = ["left", "midleft", "midright", "right"]

rankMultiplicities : Dict.Dict Rank Int
rankMultiplicities = Dict.fromList [(1, 3), (2, 2), (3, 2), (4, 2), (5, 1)]

rankMultiplicity : Rank -> Int
rankMultiplicity r = rankMultiplicities |> Dict.get r |> Maybe.withDefault 0

unshuffledDeck : List Card
unshuffledDeck =
    List.concatMap
        (\color -> List.concatMap
            (\rank -> List.repeat (rankMultiplicity rank) {color=color, rank=rank})
            ranks)
        colors

uninitializedGame : List Card -> GameState
uninitializedGame deck =
    { nFuses = 3
    , nHints = 8
    , towers = Dict.empty
    , players = []
    , hands = Dict.empty
    , deck = deck
    }

initializeGame : List Player -> GameState -> GameState
initializeGame players game =
    let
        drawHandFor : Player -> GameState -> GameState
        drawHandFor player g =
            let
                hand = List.take 4 g.deck |> List.map2 Tuple.pair posns |> Dict.fromList
                deck = List.drop 4 g.deck
            in
                { g | hands = g.hands |> Dict.insert player hand
                    , deck = deck
                    }
    in
        List.foldl drawHandFor {game | players = players} players

currentPlayer : GameState -> Player
currentPlayer game =
    case List.head game.players of
        Nothing -> Debug.todo "no players!?"
        Just player -> player

isOver : GameState -> Bool
isOver g =
       (g.nFuses == 0)
    || (g.towers |> Dict.values |> List.sum |> (\n -> n == 25))
    || (g.hands |> Dict.values |> List.map Dict.size |> List.all (\n -> n < 4))

score : GameState -> Int
score g = g.towers |> Dict.values |> List.sum

draw : Player -> CardPosition -> GameState -> GameState
draw p i g =
    let
        topCard = List.head g.deck
        updateHand = Dict.update i <| always topCard
        updateHands = Dict.update p (Maybe.map updateHand)
    in
        { g  | deck = List.drop 1 g.deck
             , hands = g.hands |> updateHands
             }

advancePlayers : GameState -> GameState
advancePlayers g =
    { g | players = List.drop 1 g.players ++ List.take 1 g.players }

step : Move -> GameState -> GameState
step move game =
    let
        active = currentPlayer game
    in
        advancePlayers <| case move of
            Play i ->
                let
                    card = case getCard game active i of
                        Nothing -> Debug.todo "no such card!?"
                        Just c -> c
                    isMatch = card.rank == 1 + (game.towers |> Dict.get card.color |> Maybe.withDefault 0)
                in
                    draw active i
                        { game | nFuses = game.nFuses - if isMatch then 0 else 1
                               , nHints = game.nHints + if isMatch && card.rank == 5 then 1 else 0
                               , towers = if isMatch then game.towers |> Dict.update card.color (\_ -> Just card.rank) else game.towers
                               }
            Discard i ->
                draw active i { game | nHints = game.nHints + 1 |> min 8 }
            HintColor p c -> { game | nHints = game.nHints - 1 }
            HintRank p c -> { game | nHints = game.nHints - 1 }

randomGame : List Player -> Random.Generator GameState
randomGame players =
    unshuffledDeck
    |> shuffle
    |> Random.map (uninitializedGame >> initializeGame players)

getCard : GameState -> Player -> CardPosition -> Maybe Card
getCard g p i = g.hands |> Dict.get p |> Maybe.andThen (Dict.get i)

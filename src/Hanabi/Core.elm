module Hanabi.Core exposing (..)

import Dict
import Random
import Random.List exposing (shuffle)


type alias Color =
    String


type alias Rank =
    Int


type alias Card =
    { color : Color, rank : Rank }


type alias Player =
    String


type alias CardPosition =
    String


type alias Hand =
    Dict.Dict CardPosition Card


type alias GameState =
    { nFuses : Int
    , nHints : Int
    , towers : Dict.Dict Color Rank
    , players : List Player
    , hands : Dict.Dict Player Hand
    , deck : List Card
    , discardPile : List Card
    , movesLeft : Maybe Int
    }


type Move
    = Play CardPosition
    | Discard CardPosition
    | HintRank Player Rank
    | HintColor Player Color


colors : List Color
colors =
    [ "B", "G", "R", "W", "Y" ]


ranks : List Rank
ranks =
    [ 1, 2, 3, 4, 5 ]


posns : Int -> List CardPosition
posns nPlayers =
    if nPlayers < 4 then
        [ "top", "midtop", "middle", "midbottom", "bottom" ]

    else
        [ "top", "midtop", "midbottom", "bottom" ]


rankMultiplicities : Dict.Dict Rank Int
rankMultiplicities =
    Dict.fromList [ ( 1, 3 ), ( 2, 2 ), ( 3, 2 ), ( 4, 2 ), ( 5, 1 ) ]


rankMultiplicity : Rank -> Int
rankMultiplicity r =
    rankMultiplicities |> Dict.get r |> Maybe.withDefault 0


unshuffledDeck : List Card
unshuffledDeck =
    List.concatMap
        (\color ->
            List.concatMap
                (\rank -> List.repeat (rankMultiplicity rank) { color = color, rank = rank })
                ranks
        )
        colors


uninitializedGame : List Card -> GameState
uninitializedGame deck =
    { nFuses = 3
    , nHints = 8
    , towers = Dict.empty
    , players = []
    , hands = Dict.empty
    , deck = deck
    , discardPile = []
    , movesLeft = Nothing
    }


initializeGame : List Player -> GameState -> GameState
initializeGame players game =
    let
        positions =
            posns (List.length players)

        drawHandFor : Player -> GameState -> GameState
        drawHandFor player g =
            let
                hand =
                    List.map2 Tuple.pair positions g.deck |> Dict.fromList

                deck =
                    List.drop (Dict.size hand) g.deck
            in
            { g
                | hands = g.hands |> Dict.insert player hand
                , deck = deck
            }
    in
    List.foldl drawHandFor { game | players = players } players


currentPlayer : GameState -> Player
currentPlayer game =
    case List.head game.players of
        Nothing ->
            Debug.todo "no players!?"

        Just player ->
            player


isOver : GameState -> Bool
isOver g =
    (g.nFuses == 0)
        || (g.towers |> Dict.values |> List.sum |> (\n -> n == 25))
        || (g.hands |> Dict.values |> List.map Dict.size |> List.all (\n -> n < 4))
        || (g.movesLeft |> Maybe.map (\n -> n <= 0) |> Maybe.withDefault False)


score : GameState -> Int
score g =
    g.towers |> Dict.values |> List.sum


draw : Player -> CardPosition -> GameState -> GameState
draw p i g =
    let
        movesLeft =
            if List.length g.deck == 1 then
                -- removing the last card
                Just (List.length g.players + 1)
                -- decrement is handled after draw

            else
                g.movesLeft

        topCard =
            List.head g.deck

        updateHand =
            Dict.update i <| always topCard

        updateHands =
            Dict.update p (Maybe.map updateHand)
    in
    { g
        | deck = List.drop 1 g.deck
        , hands = g.hands |> updateHands
        , movesLeft = movesLeft
    }


postMove : GameState -> GameState
postMove g =
    { g
        | players = List.drop 1 g.players ++ List.take 1 g.players
        , movesLeft = g.movesLeft |> Maybe.map (\n -> n - 1)
    }


step : Move -> GameState -> GameState
step move game =
    let
        active =
            currentPlayer game
    in
    postMove <|
        case move of
            Play i ->
                let
                    card =
                        case getCard game active i of
                            Nothing ->
                                Debug.todo "no such card!?"

                            Just c ->
                                c

                    isMatch =
                        card.rank == 1 + (game.towers |> Dict.get card.color |> Maybe.withDefault 0)
                in
                draw active
                    i
                    { game
                        | nFuses =
                            game.nFuses
                                - (if isMatch then
                                    0

                                   else
                                    1
                                  )
                        , nHints =
                            game.nHints
                                + (if isMatch && card.rank == 5 then
                                    1

                                   else
                                    0
                                  )
                        , towers =
                            if isMatch then
                                game.towers |> Dict.update card.color (\_ -> Just card.rank)

                            else
                                game.towers
                        , discardPile =
                            if isMatch then
                                game.discardPile

                            else
                                card :: game.discardPile
                    }

            Discard i ->
                draw active
                    i
                    { game
                        | nHints = game.nHints + 1 |> min 8
                        , discardPile =
                            case getCard game active i of
                                Nothing ->
                                    Debug.todo "no such card!?"

                                Just c ->
                                    c :: game.discardPile
                    }

            HintColor _ _ ->
                { game | nHints = game.nHints - 1 }

            HintRank _ _ ->
                { game | nHints = game.nHints - 1 }


randomGame : List Player -> Random.Generator GameState
randomGame players =
    unshuffledDeck
        |> shuffle
        |> Random.map (uninitializedGame >> initializeGame players)


getCard : GameState -> Player -> CardPosition -> Maybe Card
getCard g p i =
    g.hands |> Dict.get p |> Maybe.andThen (Dict.get i)

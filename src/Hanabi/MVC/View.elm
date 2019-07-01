module Hanabi.MVC.View exposing (view)

import Dict
import Html exposing (Html, div, button, text, input, ul, li, span)
import Html.Attributes exposing (value, placeholder)
import Html.Events exposing (onClick, onInput)

import Hanabi.Assistance exposing (History, aggregateHints, AggregatedHints, decisions)
import Hanabi.Core exposing (Hand, CardPosition, Move(..), Player, GameState, Card, isOver, posns, step, currentPlayer)
import Hanabi.MVC.Core exposing (..)


view : Model -> Html Msg
view model =
    case model of
        Creating {players} ->
            div []
                [ text "Players: "
                , input [ onInput SetPlayers, value players, placeholder "Alice, Bob, Charlie" ] []
                , button [ onClick Create ] [text "Create"]
                ]

        ChoosingPlayer {history} ->
            div []
                (text "You are: " :: List.map (\p -> button [onClick <| SetPlayer p] [text p]) history.init.players)

        Playing {player, history, freezeFrame} ->
            let
                nMoves = freezeFrame |> Maybe.withDefault (List.length history.moves)
                effectiveHistory = { history | moves = List.take nMoves history.moves}
                game = run effectiveHistory
            in
                div []
                    [ div []
                        [ if nMoves > 0 then button [onClick <| SetFreezeFrame <| Just (nMoves-1)] [text "<"] else text ""
                        , text <| "Time step " ++ (String.fromInt (nMoves+1)) ++ "/" ++ (String.fromInt <| List.length history.moves + 1 )
                        , if nMoves < List.length history.moves then button [onClick <| SetFreezeFrame <| Just (nMoves+1)] [text ">"] else text ""
                        , if not (isNothing freezeFrame) then button [onClick <| SetFreezeFrame Nothing] [text "unfreeze"] else text ""
                        ]
                    , viewGame (not (isOver game) && isNothing freezeFrame && player == currentPlayer game) player effectiveHistory
                    , text "Moves:"
                    , decisions effectiveHistory
                      |> List.indexedMap (\i (g, m) -> li [] [ button [onClick <| SetFreezeFrame <| Just i] [text "Rewind"]
                                                             , text <| currentPlayer g ++ " -- " ++ Debug.toString m
                                                             ]
                                         )
                      |> List.reverse
                      |> ul []
                    ]

cardKey : Card -> String
cardKey {color, rank} = color ++ String.fromInt rank

run : History -> GameState
run history = List.foldl step history.init history.moves

viewAggregatedHints : AggregatedHints -> Html a
viewAggregatedHints hints =
    let
        c = hints.colors |> List.sort |> String.join ""
        r = hints.ranks |> List.sort |> List.map String.fromInt |> String.join ""
    in
        text <| "(" ++ c ++ "," ++ r ++ ")"

viewOwnHand : History -> Bool -> Player -> Html Msg
viewOwnHand history interactive player =
    Dict.get player (run history).hands
    |> Maybe.withDefault Dict.empty
    |> Dict.toList
    |> List.map (\(posn, card) ->
        li []
            [ text posn
            , viewAggregatedHints (aggregateHints history player posn)
            , if interactive then button [onClick <| MakeMove <| Discard posn] [text "Discard"] else text ""
            , if interactive then button [onClick <| MakeMove <| Play posn] [text "Play"] else text ""
            ]
        )
    |> ul []

viewOtherHand : History -> Bool -> Player -> Hand -> Html Msg
viewOtherHand history interactive player hand =
    let
        game = run history
        elem : CardPosition -> Html Msg
        elem posn =
            case Dict.get posn hand of
                Nothing -> text "__"
                Just card ->
                    let
                        cardRepr : Html Msg
                        cardRepr =
                            if interactive && game.nHints > 0 then
                                span []
                                    [ button [onClick <| MakeMove <| HintColor player card.color] [text card.color]
                                    , button [onClick <| MakeMove <| HintRank player card.rank] [text <| String.fromInt card.rank]
                                    ]
                            else
                                text (card.color ++ String.fromInt card.rank)
                    in
                        span [] [cardRepr, viewAggregatedHints (aggregateHints history player posn)]
    in
        posns
        |> List.map elem
        |> List.intersperse (text ", ")
        |> span []

isNothing : Maybe a -> Bool
isNothing x = case x of
    Nothing -> True
    Just _ -> False

viewGame : Bool -> Player -> History -> Html Msg
viewGame interactive viewer history =
    let
        game = run history
    in
        ul []
            [ text <| (if isOver game then "Game over. " else "")
            , li [] [text (String.fromInt game.nFuses ++ " fuses, "
                           ++ String.fromInt game.nHints ++ " hints. "
                           ++ "Turn order: " ++ String.join ", " game.players
                           )]
            , li [] [game.towers |> Dict.toList |> List.map (\(c, r) -> c ++ String.fromInt r) |> String.join ", " |> text]
            , li [] [text "Your hand:", viewOwnHand history interactive viewer]
            , li [] [text "Other hands:", game.hands
                                          |> Dict.remove viewer
                                          |> Dict.toList
                                          |> List.map (\(player, hand) -> li [] [text (player ++ ": "), viewOtherHand history interactive player hand])
                                          |> ul []]
            , li [] [text "Unseen cards:", (game.deck ++ (Dict.get viewer game.hands |> Maybe.withDefault Dict.empty |> Dict.values)) |> List.map cardKey |> List.sort |> String.join " " |> text]
            ]

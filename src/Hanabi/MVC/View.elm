module Hanabi.MVC.View exposing (view)

import Dict
import Html exposing (Html, div, button, text, input, ul, li, span, table, tr, td, th, b, br, pre)
import Html.Attributes as Attrs exposing (value, placeholder, style)
import Html.Events exposing (onClick, onInput)

import Hanabi.Assistance exposing (History, aggregateHints, AggregatedHints, decisions, run)
import Hanabi.Core exposing (Hand, CardPosition, Move(..), Player, GameState, Card, isOver, posns, step, currentPlayer, getCard, colors, ranks)
import Hanabi.MVC.Core exposing (..)


view : Model -> Html Msg
view model =
    case model of
        Creating {gameId, players} ->
            div []
                [ text "Game id:"
                , input [ onInput SetGameId, value gameId, placeholder "srpplayground" ] []
                , button [ onClick Join ] [text "Join"]
                , br [] [], text "or", br [] []
                , text "Players: "
                , input [ onInput SetPlayers, value players, placeholder "Alice, Bob, Charlie" ] []
                , button [ onClick Create ] [text "Create"]
                ]

        LoadingGame _ -> text "Loading..."
        LoadingFailed {message} ->
            div []
                [ text <| "Loading game failed:"
                , pre [] [text <| String.replace "\\n" "\n" message]
                , button [onClick RetryLoadGame] [text "Retry"]
                ]

        ChoosingPlayer {history} ->
            div []
                (text "You are: " :: (history.init.players
                                      |> List.map (\p -> button [onClick <| SetPlayer p] [text p])
                                      |> List.intersperse (text " or ")))

        Playing {player, history, freezeFrame, polling} ->
            let
                nMoves = freezeFrame |> Maybe.withDefault (List.length history.moves)
                effectiveHistory = { history | moves = List.take nMoves history.moves}
                game = run effectiveHistory
            in
                div []
                    [ div []
                        [ button [Attrs.disabled <| nMoves == 0, onClick <| SetFreezeFrame <| Just (nMoves-1)] [text "<"]
                        , text <| " Time step " ++ (String.fromInt (nMoves+1)) ++ "/" ++ (String.fromInt <| List.length history.moves + 1 ) ++ " "
                        , button [Attrs.disabled <| nMoves == List.length history.moves, onClick <| SetFreezeFrame <| Just (nMoves+1)] [text ">"]
                        , button [Attrs.disabled <| isNothing freezeFrame, onClick <| SetFreezeFrame Nothing] [text "Unfreeze"]
                        , if polling then text "" else button [ style "background-color" "red", onClick Poll ] [text "Reconnect"]
                        ]
                    , viewGame (polling && not (isOver game) && isNothing freezeFrame && player == currentPlayer game) player effectiveHistory
                    , text "Moves:"
                    , decisions effectiveHistory
                      |> List.indexedMap (\i (g, m) -> tr [] [ td [] [button [onClick <| SetFreezeFrame <| Just i] [text "Context"]]
                                                             , td [] [text <| currentPlayer g]
                                                             , td [] [viewMove player g m]
                                                             , td [] [button [onClick <| SetFreezeFrame <| Just (i+1)] [text "Result"]]
                                                             ]
                                         )
                      |> List.reverse
                      |> table [Attrs.attribute "border" "1px solid black"]
                    ]

conciseHand : GameState -> Player -> String
conciseHand g p =
    g.hands
    |> Dict.get p
    |> Maybe.withDefault Dict.empty
    |> Dict.values
    |> List.map cardKey
    |> String.join ", "

viewMove : Player -> GameState -> Move -> Html a
viewMove viewer g m =
    let
        active = currentPlayer g
    in
        case m of
            HintColor p c ->
                text <| "hinted "
                        ++ p
                        ++ (if p /= viewer then " (holding " ++ conciseHand g p ++ ")" else "")
                        ++ ": " ++ c
            HintRank p r ->
                text <| "hinted "
                        ++ p
                        ++ (if p /= viewer then " (holding " ++ conciseHand g p ++ ")" else "")
                        ++ ": " ++ String.fromInt r
            Play posn ->
                text <| (if (step m g).towers /= g.towers then "successfully " else "unsuccessfully ")
                        ++ "played "
                        ++ posn
                        ++ " (" ++ (getCard g active posn |> Maybe.withDefault (Card "" 0) |> cardKey) ++ ")"
                        ++ (if active /= viewer then " (out of " ++ conciseHand g active ++ ")" else "")
            Discard posn ->
                text <| "discarded "
                        ++ posn
                        ++ " (" ++ (getCard g active posn |> Maybe.withDefault (Card "" 0) |> cardKey) ++ ")"
                        ++ (if active /= viewer then " (out of " ++ conciseHand g active ++ ")" else "")

cardKey : Card -> String
cardKey {color, rank} = color ++ String.fromInt rank

viewHands : Player -> Bool -> History -> Html Msg
viewHands player interactive history =
    let
        active = currentPlayer (run history)
        headers = history.init.players |> List.map (\p -> th [] [text p, text (if p == active then " (active)" else "")])
        cells = history.init.players |> List.map (\p -> td [] [(if p == player then viewOwnHand else viewOtherHand) history interactive p])
    in
        table [Attrs.attribute "border" "1px solid black"]
            [ tr [] headers
            , tr [] cells ]


viewAggregatedHints : AggregatedHints -> Html a
viewAggregatedHints hints =
    let
        c = hints.colors |> List.sort |> String.join ""
        r = hints.ranks |> List.sort |> List.map String.fromInt |> String.join ""
    in
        text <| "(" ++ c ++ ", " ++ r ++ ")"

viewOwnHand : History -> Bool -> Player -> Html Msg
viewOwnHand history interactive player =
    Dict.get player (run history).hands
    |> Maybe.withDefault Dict.empty
    |> Dict.toList
    |> List.map (\(posn, card) -> tr []
        [ td [] [ button [Attrs.disabled (not interactive), onClick <| MakeMove <| Discard posn] [text "Discard"]
                , button [Attrs.disabled (not interactive), onClick <| MakeMove <| Play posn] [text "Play"]
                ]
        , td [] [viewAggregatedHints (aggregateHints history player posn)]
        ])
    |> (\rows -> [tr [] [th [] [text "Actions"], th [] [text "Options"]]] ++ rows)
    |> table []

viewOtherHand : History -> Bool -> Player -> Html Msg
viewOtherHand history interactive player =
    let
        game = run history
        hand = game.hands |> Dict.get player |> Maybe.withDefault Dict.empty
        row : CardPosition -> Html Msg
        row posn =
            case Dict.get posn hand of
                Nothing -> tr [] [td [] [text "__"], td [] [text "__"]]
                Just card ->
                    let
                        canHint : Bool
                        canHint = interactive && game.nHints > 0

                        cardRepr : Html Msg
                        cardRepr =
                            span []
                                [ button [Attrs.disabled (not canHint), onClick <| MakeMove <| HintColor player card.color] [text card.color]
                                , button [Attrs.disabled (not canHint), onClick <| MakeMove <| HintRank player card.rank] [text <| String.fromInt card.rank]
                                ]
                    in
                        tr [] [td [] [cardRepr], td [] [viewAggregatedHints (aggregateHints history player posn)]]
    in
        posns
        |> List.map row
        |> (\rows -> [tr [] [th [] [text "Card"], th [] [text "Options"]]] ++ rows)
        |> table []

isNothing : Maybe a -> Bool
isNothing x = case x of
    Nothing -> True
    Just _ -> False

viewGame : Bool -> Player -> History -> Html Msg
viewGame interactive viewer history =
    let
        game = run history
        unseenCards = game.deck ++ (Dict.get viewer game.hands |> Maybe.withDefault Dict.empty |> Dict.values)
    in
        ul []
            [ text <| (if isOver game then "Game over. " else "")
            , li [] [text (String.fromInt game.nFuses ++ " fuses, "
                           ++ String.fromInt game.nHints ++ " hints. "
                           )]
            , li [] [text "Towers: ", game.towers |> Dict.toList |> List.map (\(c, r) -> c ++ String.fromInt r) |> String.join ", " |> text]
            , viewHands viewer interactive history
            , li [] [text "Unseen cards: ", viewCardCountingTable unseenCards]
            ]

viewCardCountingTable : List Card -> Html a
viewCardCountingTable cards =
    colors
    |> List.map (\c ->
        ranks
        |> List.map (\r -> td [] [cards
                                  |> List.filter ((==) {color=c, rank=r})
                                  |> List.length
                                  |> String.fromInt
                                  |> text])
        |> (::) (td [] [b [] [text c]])
        |> tr []
        )
    |> (::) (tr [] <| (td [] []) :: (ranks |> List.map (\r -> td [] [b [] [text <| String.fromInt <| r]])))
    |> table []

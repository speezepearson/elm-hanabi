module Hanabi.Assistance exposing (..)

import Hanabi.Core exposing (CardPosition, Color, GameState, Move(..), Player, Rank, colors, currentPlayer, getCard, ranks, step)


type alias History =
    { init : GameState
    , moves : List Move
    }


decisions : History -> List ( GameState, Move )
decisions history =
    case history.moves of
        [] ->
            []

        m :: ms ->
            ( history.init, m ) :: decisions { init = step m history.init, moves = ms }


type alias AggregatedHints =
    { colors : List Color, ranks : List Rank }


noInformation : AggregatedHints
noInformation =
    { colors = colors, ranks = ranks }


aggregateHints : History -> Player -> CardPosition -> AggregatedHints
aggregateHints history player posn =
    let
        aggregate : ( GameState, Move ) -> AggregatedHints -> AggregatedHints
        aggregate ( game, move ) agg =
            case move of
                Play i ->
                    if (currentPlayer game == player) && i == posn then
                        noInformation

                    else
                        agg

                Discard i ->
                    if (currentPlayer game == player) && i == posn then
                        noInformation

                    else
                        agg

                HintColor p c ->
                    if p == player then
                        if Just c == (getCard game player posn |> Maybe.map .color) then
                            { agg | colors = [ c ] }

                        else
                            { agg | colors = agg.colors |> List.filter (\c2 -> c2 /= c) }

                    else
                        agg

                HintRank p r ->
                    if p == player then
                        if Just r == (getCard game player posn |> Maybe.map .rank) then
                            { agg | ranks = [ r ] }

                        else
                            { agg | ranks = agg.ranks |> List.filter (\r2 -> r2 /= r) }

                    else
                        agg
    in
    List.foldl aggregate noInformation (decisions history)


run : History -> GameState
run history =
    List.foldl step history.init history.moves

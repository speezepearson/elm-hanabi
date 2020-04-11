module Pages.Play exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Dict
import Flags exposing (Flags)
import Hanabi.Assistance exposing (AggregatedHints, History, aggregateHints, decisions, run)
import Hanabi.Core exposing (Card, CardPosition, GameState, Hand, Move(..), Player, colors, currentPlayer, getCard, isOver, posns, ranks, score, step)
import Hanabi.MVC.API exposing (conn)
import Hanabi.MVC.Core exposing (TimeStep)
import Html exposing (Html, b, button, div, li, span, table, td, text, th, tr, ul)
import Html.Attributes as Attrs exposing (style)
import Html.Events exposing (onClick)
import Http
import Pages.Routes as Routes exposing (Escaped(..), PlayPageFlags)
import StateServer as SS


type alias Model =
    { flags : Flags
    , gameId : SS.Name
    , player : Player
    , history : Maybe History
    , freezeFrame : Maybe TimeStep
    , polling : Bool
    }


type Msg
    = MakeMove Move
    | SetFreezeFrame (Maybe TimeStep)
    | LoadedGame (Result Http.Error (Maybe History))
    | MadeMove
    | Poll


init : Flags -> PlayPageFlags -> ( Model, Cmd Msg )
init flags pageFlags =
    ( { flags = flags
      , gameId = pageFlags.gameId
      , player = pageFlags.player
      , history = Nothing
      , freezeFrame = Nothing
      , polling = True
      }
    , SS.get LoadedGame (conn flags.stateServerRoot pageFlags.gameId)
    )


update : Msg -> Model -> Escaped ( Model, Cmd Msg )
update msg model =
    let
        connection =
            conn model.flags.stateServerRoot model.gameId
    in
    case msg of
        LoadedGame (Ok (Just history)) ->
            Stay <|
                ( { model | history = Just history }
                , SS.poll LoadedGame connection (Just history)
                )

        LoadedGame (Ok Nothing) ->
            Escape Routes.Home

        LoadedGame (Err _) ->
            Stay <|
                ( { model | polling = False }
                , Cmd.none
                )

        SetFreezeFrame t ->
            Stay <|
                ( { model | freezeFrame = t }
                , Cmd.none
                )

        MakeMove move ->
            Stay <|
                case model.history of
                    Nothing ->
                        Debug.todo "got MakeMove before history loaded!?"

                    Just oldHistory ->
                        let
                            newHistory =
                                { oldHistory | moves = oldHistory.moves ++ [ move ] }
                        in
                        ( { model | history = Just newHistory }
                        , SS.update (always MadeMove) connection (Just oldHistory) (Just newHistory)
                        )

        MadeMove ->
            Stay <|
                ( model
                , Cmd.none
                )

        Poll ->
            Stay <|
                ( { model | polling = True }
                , SS.poll LoadedGame connection model.history
                )


view : Model -> Html Msg
view model =
    let
        retryButton : Html Msg
        retryButton =
            button [ style "background-color" "red", onClick Poll ] [ text "Reconnect" ]
    in
    case model.history of
        Nothing ->
            if model.polling then
                text "loading..."

            else
                div [] [ text "Loading failed. ", retryButton ]

        Just history ->
            let
                nMoves =
                    model.freezeFrame |> Maybe.withDefault (List.length history.moves)

                effectiveHistory =
                    { history | moves = List.take nMoves history.moves }

                game =
                    run effectiveHistory
            in
            div []
                [ div []
                    [ button [ Attrs.disabled <| nMoves == 0, onClick <| SetFreezeFrame <| Just (nMoves - 1) ] [ text "<" ]
                    , text <| " Time step " ++ String.fromInt (nMoves + 1) ++ "/" ++ (String.fromInt <| List.length history.moves + 1) ++ " "
                    , button [ Attrs.disabled <| nMoves == List.length history.moves, onClick <| SetFreezeFrame <| Just (nMoves + 1) ] [ text ">" ]
                    , button [ Attrs.disabled (model.freezeFrame == Nothing), onClick <| SetFreezeFrame Nothing ] [ text "Unfreeze" ]
                    , if model.polling then
                        text ""

                      else
                        retryButton
                    ]
                , viewGame (model.polling && not (isOver game) && (model.freezeFrame == Nothing) && model.player == currentPlayer game) model.player effectiveHistory
                , text "Moves:"
                , decisions effectiveHistory
                    |> List.indexedMap
                        (\i ( g, m ) ->
                            tr []
                                [ td [] [ button [ onClick <| SetFreezeFrame <| Just i ] [ text <| "Context (t=" ++ String.fromInt (i+1) ++ ")" ] ]
                                , td [] [ text <| currentPlayer g ]
                                , td [] [ viewMove model.player g m ]
                                , td [] [ button [ onClick <| SetFreezeFrame <| Just (i + 1) ] [ text "Result" ] ]
                                ]
                        )
                    |> List.reverse
                    |> table [ Attrs.attribute "border" "1px solid black" ]
                ]


conciseHand : GameState -> Player -> String
conciseHand g p =
    let
        hand : Dict.Dict CardPosition Card
        hand =
            g.hands
                |> Dict.get p
                |> Maybe.withDefault Dict.empty

        stringForPosn : CardPosition -> String
        stringForPosn posn =
            Dict.get posn hand
                |> Maybe.map cardKey
                |> Maybe.withDefault "_"
    in
    posns (List.length <| g.players)
        |> List.map stringForPosn
        |> String.join ", "


viewMove : Player -> GameState -> Move -> Html a
viewMove viewer g m =
    let
        active =
            currentPlayer g
    in
    case m of
        HintColor p c ->
            text <|
                "hinted "
                    ++ p
                    ++ (if p /= viewer then
                            " (holding " ++ conciseHand g p ++ ")"

                        else
                            ""
                       )
                    ++ ": "
                    ++ c

        HintRank p r ->
            text <|
                "hinted "
                    ++ p
                    ++ (if p /= viewer then
                            " (holding " ++ conciseHand g p ++ ")"

                        else
                            ""
                       )
                    ++ ": "
                    ++ String.fromInt r

        Play posn ->
            text <|
                (if (step m g).towers /= g.towers then
                    "successfully "

                 else
                    "unsuccessfully "
                )
                    ++ "played "
                    ++ posn
                    ++ " ("
                    ++ (getCard g active posn |> Maybe.withDefault (Card "" 0) |> cardKey)
                    ++ ")"
                    ++ (if active /= viewer then
                            " (out of " ++ conciseHand g active ++ ")"

                        else
                            ""
                       )

        Discard posn ->
            text <|
                "discarded "
                    ++ posn
                    ++ " ("
                    ++ (getCard g active posn |> Maybe.withDefault (Card "" 0) |> cardKey)
                    ++ ")"
                    ++ (if active /= viewer then
                            " (out of " ++ conciseHand g active ++ ")"

                        else
                            ""
                       )


cardKey : Card -> String
cardKey { color, rank } =
    color ++ String.fromInt rank


viewHands : Player -> Bool -> History -> Html Msg
viewHands player interactive history =
    let
        active =
            currentPlayer (run history)

        headers =
            history.init.players
                |> List.map
                    (\p ->
                        th []
                            [ text p
                            , text
                                (if p == active then
                                    " (active)"

                                 else
                                    ""
                                )
                            ]
                    )

        cells =
            history.init.players
                |> List.map
                    (\p ->
                        td []
                            [ (if p == player then
                                viewOwnHand

                               else
                                viewOtherHand
                              )
                                history
                                interactive
                                p
                            ]
                    )
    in
    table [ Attrs.attribute "border" "1px solid black" ]
        [ tr [] headers
        , tr [] cells
        ]


viewAggregatedHints : AggregatedHints -> Html a
viewAggregatedHints hints =
    let
        c =
            hints.colors |> List.sort |> String.join ""

        r =
            hints.ranks |> List.sort |> List.map String.fromInt |> String.join ""
    in
    text <| "(" ++ c ++ ", " ++ r ++ ")"


viewOwnHand : History -> Bool -> Player -> Html Msg
viewOwnHand history interactive player =
    let
        game =
            run history
    in
    posns (List.length history.init.players)
        |> List.map
            (\posn ->
                case getCard game player posn of
                    Just _ ->
                        tr []
                            [ td []
                                [ button [ Attrs.disabled (not interactive), onClick <| MakeMove <| Discard posn ] [ text "Discard" ]
                                , button [ Attrs.disabled (not interactive), onClick <| MakeMove <| Play posn ] [ text "Play" ]
                                ]
                            , td [] [ viewAggregatedHints (aggregateHints history player posn) ]
                            ]

                    Nothing ->
                        tr [] [ td [] [ text "__" ], td [] [ text "__" ] ]
            )
        |> (\rows -> [ tr [] [ th [] [ text "Actions" ], th [] [ text "Options" ] ] ] ++ rows)
        |> table []


viewOtherHand : History -> Bool -> Player -> Html Msg
viewOtherHand history interactive player =
    let
        game =
            run history

        hand =
            game.hands |> Dict.get player |> Maybe.withDefault Dict.empty

        row : CardPosition -> Html Msg
        row posn =
            case Dict.get posn hand of
                Nothing ->
                    tr [] [ td [] [ text "__" ], td [] [ text "__" ] ]

                Just card ->
                    let
                        canHint : Bool
                        canHint =
                            interactive && game.nHints > 0

                        cardRepr : Html Msg
                        cardRepr =
                            span []
                                [ button [ Attrs.disabled (not canHint), onClick <| MakeMove <| HintColor player card.color ] [ text card.color ]
                                , button [ Attrs.disabled (not canHint), onClick <| MakeMove <| HintRank player card.rank ] [ text <| String.fromInt card.rank ]
                                ]
                    in
                    tr [] [ td [] [ cardRepr ], td [] [ viewAggregatedHints (aggregateHints history player posn) ] ]
    in
    posns (List.length game.players)
        |> List.map row
        |> (\rows -> [ tr [] [ th [] [ text "Card" ], th [] [ text "Options" ] ] ] ++ rows)
        |> table []


viewGame : Bool -> Player -> History -> Html Msg
viewGame interactive viewer history =
    let
        game =
            run history

        unseenCards =
            game.deck ++ (Dict.get viewer game.hands |> Maybe.withDefault Dict.empty |> Dict.values)
    in
    ul []
        [ text <|
            if isOver game then
                "Game over. Final score: " ++ String.fromInt (score game)

            else
                ""
        , li []
            [ text
                (String.fromInt game.nFuses
                    ++ " fuses, "
                    ++ String.fromInt game.nHints
                    ++ " hints, "
                    ++ String.fromInt (List.length game.deck)
                    ++ " cards left. "
                )
            ]
        , li [] [ text "Towers: ", game.towers |> Dict.toList |> List.map (\( c, r ) -> c ++ String.fromInt r) |> String.join ", " |> text ]
        , li [] [ viewHands viewer interactive history ]
        , li [] [ text "Discarded:", viewCardCountingTable game.discardPile ]
        , li [] [ text "Unseen cards: ", viewCardCountingTable unseenCards ]
        ]


viewCardCountingTable : List Card -> Html a
viewCardCountingTable cards =
    colors
        |> List.map
            (\c ->
                ranks
                    |> List.map
                        (\r ->
                            td []
                                [ cards
                                    |> List.filter ((==) { color = c, rank = r })
                                    |> List.length
                                    |> String.fromInt
                                    |> text
                                ]
                        )
                    |> (::) (td [] [ b [] [ text c ] ])
                    |> tr []
            )
        |> (::) (tr [] <| td [] [] :: (ranks |> List.map (\r -> td [] [ b [] [ text <| String.fromInt <| r ] ])))
        |> table []

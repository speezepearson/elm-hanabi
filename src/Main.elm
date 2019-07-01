import Browser
import Dict
import Html exposing (Html, div, button, text, input, ul, li, span)
import Html.Attributes exposing (value, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
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

type alias History =
    { init : GameState
    , moves : List Move
    }

-------------------------------------------------------------------
-- JSON

encodeCard : Card -> E.Value
encodeCard {color, rank} = E.object [("color", E.string color), ("rank", E.int rank)]
cardDecoder : D.Decoder Card
cardDecoder = D.map2 Card (D.field "color" D.string) (D.field "rank" D.int)

encodeHand : Hand -> E.Value
encodeHand h = E.object <| Dict.toList <| mapValues encodeCard h
handDecoder : D.Decoder Hand
handDecoder = D.dict cardDecoder

encodeGameState : GameState -> E.Value
encodeGameState g =
    E.object
        [ ("nFuses", E.int g.nFuses)
        , ("nHints", E.int g.nHints)
        , ("towers", E.object <| Dict.toList <| Dict.map (\c r -> E.int r) g.towers)
        , ("players", E.list E.string g.players)
        , ("hands", E.object <| Dict.toList <| Dict.map (\p h -> encodeHand h) g.hands)
        , ("deck", E.list encodeCard g.deck)
        ]
gameStateDecoder : D.Decoder GameState
gameStateDecoder =
    D.map6 (\fus hin tow pla han dec -> {nFuses=fus, nHints=hin, towers=tow, players=pla, hands=han, deck=dec})
        (D.field "nFuses" D.int)
        (D.field "nHints" D.int)
        (D.field "towers" <| D.dict D.int)
        (D.field "players" <| D.list D.string)
        (D.field "hands" <| D.dict handDecoder)
        (D.field "deck" <| D.list cardDecoder)

encodeMove : Move -> E.Value
encodeMove m =
    case m of
        Play pos -> E.object [("play", E.string pos)]
        Discard pos -> E.object [("discard", E.string pos)]
        HintColor p c -> E.object [("hint", E.string p), ("color", E.string c)]
        HintRank p n -> E.object [("hint", E.string p), ("rank", E.int n)]
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
    E.object [("init", encodeGameState history.init), ("moves", E.list encodeMove history.moves)]
historyDecoder : D.Decoder History
historyDecoder =
    D.map2 History
        (D.field "init" gameStateDecoder)
        (D.field "moves" <| D.list moveDecoder)

-------------------------------------------------------------------
-- GAME LOGIC

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

randomGame : List Player -> Random.Generator GameState
randomGame players =
    let
        gameForDeck : List Card -> GameState
        gameForDeck deck =
            { nFuses = 3
            , nHints = 8
            , towers = Dict.empty
            , players = players
            , hands = Dict.empty
            , deck = deck
            }

        drawHandFor : Player -> GameState -> GameState
        drawHandFor player game =
            let
                hand = List.take 4 game.deck |> List.map2 Tuple.pair posns |> Dict.fromList
                deck = List.drop 4 game.deck
            in
                { game | hands = game.hands |> Dict.insert player hand
                       , deck = deck
                       }
    in
        unshuffledDeck
        |> shuffle
        |> Random.map gameForDeck
        |> Random.map (\g -> List.foldl drawHandFor g players)

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
                draw active i <|
                    let
                        card = case game.hands |> Dict.get active |> Maybe.andThen (Dict.get i) of
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


-------------------------------------------------------------------
-- MVC

type alias TimeStep = Int

type alias GameId = String
type Model = Creating {players: String}
    | ChoosingPlayer {gameId: GameId, history: History}
    | Playing {gameId: GameId, player: Player, history: History, freezeFrame: Maybe TimeStep}

type Msg
    -- Creating
    = SetPlayers String
    | Create
    | RandomGameGenerated GameState
    | NewGamePosted (Result Http.Error History)
    -- ChoosingPlayer
    | SetPlayer Player
    -- Playing
    | MakeMove Move
    | SetHistory (Result Http.Error History)
    | SetFreezeFrame (Maybe TimeStep)
    -- Misc
    | MadeMove

init : () -> (Model, Cmd Msg)
init _ = (Creating {players = ""}, Cmd.none)

postNewGame : GameId -> GameState -> Cmd Msg
postNewGame id game =
    Http.post
        { url = "/states/" ++ id
        , body = Http.jsonBody <| E.object [("old", E.null), ("new", encodeHistory {init=game, moves=[]})]
        , expect = Http.expectJson NewGamePosted (D.field "current_state" historyDecoder)
        }

pollForHistory : GameId -> History -> Cmd Msg
pollForHistory id history =
    Http.post
        { url = "/poll/" ++ id
        , body = Http.jsonBody <| E.object [("current_state", encodeHistory history)]
        , expect = Http.expectJson SetHistory (D.field "current_state" historyDecoder)
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model, msg) of
        -- Creating
        (Creating state, SetPlayers s) -> (Creating {state | players = s}, Cmd.none)
        (Creating state, Create) ->
            let
                players = List.map String.trim <| String.split "," state.players
            in
                ( Creating {players=state.players}
                , Random.generate RandomGameGenerated (randomGame players)
                )
        (Creating state, RandomGameGenerated g) -> (model, postNewGame "TODO" g)
        (Creating state, NewGamePosted (Ok history)) ->
            (ChoosingPlayer {gameId = "TODO", history = history}, Cmd.none)
        (Creating state, NewGamePosted (Err err)) ->
            (Debug.log (Debug.toString err) model, Cmd.none)

        -- ChoosingPlayer
        (ChoosingPlayer state, SetPlayer p) ->
            ( Playing {gameId=state.gameId, player=p, history=state.history, freezeFrame=Nothing}
            , pollForHistory state.gameId state.history
            )

        -- Playing
        (Playing state, SetHistory result) ->
            let
                history = case Debug.log (Debug.toString result) result of
                    Ok h -> h
                    Err e -> Debug.log (Debug.toString ("Error fetching history", e)) state.history
            in
                ( Playing { state | history = history }
                , pollForHistory state.gameId history
                )
        (Playing state, SetFreezeFrame t) -> (Playing { state | freezeFrame = t }, Cmd.none)
        (Playing state, MakeMove move) ->
            let
                oldHistory = state.history
                newHistory = { oldHistory | moves = oldHistory.moves ++ [move] }
            in
                ( Playing { state | history = newHistory }
                , Http.post
                    { url = "/states/" ++ state.gameId
                    , body = Http.jsonBody (E.object [("old", encodeHistory oldHistory), ("new", encodeHistory newHistory)])
                    , expect = Http.expectWhatever (always MadeMove)
                    }
                )

        (a, b) ->
            Debug.todo (Debug.toString ("unhandled message", a, b))

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
                moves = List.take nMoves history.moves
                game = moves |> List.foldl step history.init
            in
                div []
                    [ div []
                        [ if nMoves > 0 then button [onClick <| SetFreezeFrame <| Just (nMoves-1)] [text "<"] else text ""
                        , text <| "Time step " ++ (String.fromInt (nMoves+1)) ++ "/" ++ (String.fromInt <| List.length history.moves + 1 )
                        , if nMoves < List.length history.moves then button [onClick <| SetFreezeFrame <| Just (nMoves+1)] [text ">"] else text ""
                        , if not (isNothing freezeFrame) then button [onClick <| SetFreezeFrame Nothing] [text "unfreeze"] else text ""
                        ]
                    , viewGame (isNothing freezeFrame && player == currentPlayer game) player game
                    , text "Moves:"
                    , moves
                      |> List.indexedMap (\i m -> li [] [ button [onClick <| SetFreezeFrame <| Just i] [text "Rewind"]
                                                        , text <| Debug.toString m
                                                        ]
                                         )
                      |> List.reverse
                      |> ul []
                    ]

cardKey : Card -> String
cardKey {color, rank} = color ++ String.fromInt rank

viewOwnHand : GameState -> Bool -> Player -> Html Msg
viewOwnHand game interactive player =
    Dict.get player game.hands
    |> Maybe.withDefault Dict.empty
    |> Dict.toList
    |> List.map (\(pos, card) ->
        li []
            [ text pos
            , if interactive then button [onClick <| MakeMove <| Discard pos] [text "Discard"] else text ""
            , if interactive then button [onClick <| MakeMove <| Play pos] [text "Play"] else text ""
            ]
        )
    |> ul []

viewOtherHand : GameState -> Bool -> Player -> Hand -> Html Msg
viewOtherHand game interactive player hand =
    let
        elem : CardPosition -> Html Msg
        elem posn =
            case Dict.get posn hand of
                Nothing -> text "__"
                Just card ->
                    if interactive && game.nHints > 0 then
                        span []
                            [ button [onClick <| MakeMove <| HintColor player card.color] [text card.color]
                            , button [onClick <| MakeMove <| HintRank player card.rank] [text <| String.fromInt card.rank]
                            ]
                    else
                        text (card.color ++ String.fromInt card.rank)
    in
        posns
        |> List.map elem
        |> List.intersperse (text ", ")
        |> span []

mapValues : (a -> b) -> Dict.Dict k a -> Dict.Dict k b
mapValues f d = Dict.map (\_ v -> f v) d
isNothing : Maybe a -> Bool
isNothing x = case x of
    Nothing -> True
    Just _ -> False

viewGame : Bool -> Player -> GameState -> Html Msg
viewGame interactive viewer game =
    ul []
        [ text <| (if isOver game then "Game over. " else "") ++ "Score " ++ String.fromInt (score game)
        , li [] [text (String.fromInt game.nFuses ++ " fuses, "
                       ++ String.fromInt game.nHints ++ " hints. "
                       ++ "Turn order: " ++ String.join ", " game.players
                       )]
        , li [] [game.towers |> Dict.toList |> List.map (\(c, r) -> c ++ String.fromInt r) |> String.join ", " |> text]
        , li [] [text "Your hand:", viewOwnHand game interactive viewer]
        , li [] [text "Other hands:", game.hands
                                      |> Dict.remove viewer
                                      |> Dict.toList
                                      |> List.map (\(player, hand) -> li [] [text (player ++ ": "), viewOtherHand game interactive player hand])
                                      |> ul []]
        , li [] [text "Unseen cards:", (game.deck ++ (Dict.get viewer game.hands |> Maybe.withDefault Dict.empty |> Dict.values)) |> List.map cardKey |> List.sort |> String.join " " |> text]
        ]

main = Browser.element {init=init, update=update, view=view, subscriptions=(always Sub.none)}

module Pages.PlayerSelect exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Flags exposing (Flags)
import Hanabi.Assistance exposing (History)
import Hanabi.Core exposing (Player)
import Hanabi.MVC.API exposing (conn)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Pages.Routes as Routes exposing (Escaped(..), PlayerSelectPageFlags)
import StateServer as SS


type alias Model =
    { flags : Flags
    , gameId : SS.Name
    , players : Maybe (List Player)
    , polling : Bool
    }


type Msg
    = SetPlayer Player
    | LoadedGame (Result Http.Error (Maybe History))
    | Poll


init : Flags -> PlayerSelectPageFlags -> ( Model, Cmd Msg )
init flags pageFlags =
    ( { flags = flags
      , gameId = pageFlags.gameId
      , players = Nothing
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
                ( { model | players = Just history.init.players, polling = False }
                , Cmd.none
                )

        LoadedGame (Ok Nothing) ->
            Escape Routes.Home

        LoadedGame (Err _) ->
            Stay <|
                ( { model | polling = False }
                , Cmd.none
                )

        SetPlayer player ->
            Escape (Routes.Play { gameId = model.gameId, player = player })

        Poll ->
            Stay <|
                ( { model | polling = True }
                , SS.get LoadedGame connection
                )


view : Model -> Html Msg
view model =
    case model.players of
        Nothing ->
            if model.polling then
                text "loading..."

            else
                div [] [ text "Loading failed. ", button [ style "background-color" "red", onClick Poll ] [ text "Reconnect" ] ]

        Just players ->
            div []
                (text "You are: "
                    :: (players
                            |> List.map (\p -> button [ onClick <| SetPlayer p ] [ text p ])
                            |> List.intersperse (text " or ")
                       )
                )

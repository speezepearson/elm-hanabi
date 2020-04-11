module Pages.Meta exposing
    ( Model
    , Msg(..)
    , init
    , main
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Flags exposing (Flags)
import Html exposing (Html)
import Pages.Home as Home
import Pages.Play as Play
import Pages.PlayerSelect as PlayerSelect
import Pages.Routes as Routes
import Url


type alias Model =
    { flags : Flags
    , navKey : Nav.Key
    , pageModel : PageModel

    -- , numPageTransitions : Int -- TODO: to keep pages from issuing commands that are obeyed across transitions
    }


type Msg
    = WrappedMsg PageMsg -- WrappedMsg {pageMsg: PageMsg, numPageTransitions} -- TODO: to keep pages from issuing commands that are obeyed across transitions
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


type PageModel
    = HomeModel Home.Model
    | PlayerSelectModel PlayerSelect.Model
    | PlayModel Play.Model


type PageMsg
    = HomeMsg Home.Msg
    | PlayerSelectMsg PlayerSelect.Msg
    | PlayMsg Play.Msg


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( modelToWrap, cmdToWrap ) =
            initWrapped flags (Routes.fromUrl url) key
    in
    ( { flags = flags
      , navKey = key
      , pageModel = modelToWrap
      }
    , Cmd.map WrappedMsg cmdToWrap
    )


initWrapped : Flags -> Routes.Route -> Nav.Key -> ( PageModel, Cmd PageMsg )
initWrapped flags route key =
    case route of
        Routes.NotFound ->
            let
                ( wrapped, _ ) =
                    Home.init flags key
            in
            ( HomeModel wrapped, Nav.pushUrl key (Routes.toNavString Routes.Home) )

        Routes.Home ->
            let
                ( wrapped, cmd ) =
                    Home.init flags key
            in
            ( HomeModel wrapped, Cmd.map HomeMsg cmd )

        Routes.PlayerSelect pageFlags ->
            let
                ( wrapped, cmd ) =
                    PlayerSelect.init flags key pageFlags
            in
            ( PlayerSelectModel wrapped, Cmd.map PlayerSelectMsg cmd )

        Routes.Play pageFlags ->
            let
                ( wrapped, cmd ) =
                    Play.init flags key pageFlags
            in
            ( PlayModel wrapped, Cmd.map PlayMsg cmd )


view : Model -> Html Msg
view model =
    case model.pageModel of
        HomeModel wrapped ->
            Html.map (WrappedMsg << HomeMsg) <| Home.view wrapped

        PlayerSelectModel wrapped ->
            Html.map (WrappedMsg << PlayerSelectMsg) <| PlayerSelect.view wrapped

        PlayModel wrapped ->
            Html.map (WrappedMsg << PlayMsg) <| Play.view wrapped


update : Msg -> Model -> ( Model, Cmd Msg )
update metaMsg metaModel =
    case ( metaModel.pageModel, metaMsg ) of
        ( HomeModel model, WrappedMsg (HomeMsg msg) ) ->
            let (newModel, cmd) = Home.update msg model
            in
                    ( { metaModel | pageModel = HomeModel newModel }, Cmd.map (WrappedMsg << HomeMsg) cmd )

        ( PlayerSelectModel model, WrappedMsg (PlayerSelectMsg msg) ) ->
            let (newModel, cmd) = PlayerSelect.update msg model
            in
                    ( { metaModel | pageModel = PlayerSelectModel newModel }, Cmd.map (WrappedMsg << PlayerSelectMsg) cmd )

        ( PlayModel model, WrappedMsg (PlayMsg msg) ) ->
            let (newModel, cmd) = Play.update msg model
            in
                    ( { metaModel | pageModel = PlayModel newModel }, Cmd.map (WrappedMsg << PlayMsg) cmd )

        ( _, UrlRequested request ) ->
            Debug.todo <| "not sure what to do with url request: " ++ Debug.toString request

        ( _, UrlChanged url ) ->
            init metaModel.flags url metaModel.navKey

        ( _, _ ) ->
            Debug.log ("ignoring msg: " ++ Debug.toString metaMsg) <|
                Debug.log ("...in state: " ++ Debug.toString metaModel) <|
                    ( metaModel, Cmd.none )


main =
    Browser.application
        { init = init
        , update = update
        , view = view >> (\html -> { title = "Hanabi", body = [ html ] })
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }

module Pages.Meta exposing
    ( Model
    , Msg(..)
    , init
    , view
    , update
    , main
    )

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Url

import Pages.Home as Home
import Pages.PlayerSelect as PlayerSelect
import Pages.Play as Play
import Pages.Routes as Routes
import Flags exposing (Flags)

type alias Model =
    { flags : Flags
    , navKey : Nav.Key
    , pageModel : PageModel
    -- , numPageTransitions : Int -- TODO: to keep pages from issuing commands that are obeyed across transitions
    }

type Msg
    = PageMsg PageMsg -- PageMsg {pageMsg: PageMsg, numPageTransitions} -- TODO: to keep pages from issuing commands that are obeyed across transitions
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
    let (modelToWrap, cmdToWrap) = initWrapped flags (Routes.fromUrl url) key
    in
    ( { flags = flags
      , navKey = key
      , pageModel = modelToWrap
      }
    , Cmd.map PageMsg cmdToWrap
    )

initWrapped : Flags -> Routes.Route -> Nav.Key -> ( PageModel, Cmd PageMsg )
initWrapped flags route key =
    case route of
        Routes.NotFound ->
            let
                (wrapped, cmd) = Home.init flags
            in
                (HomeModel wrapped, Nav.pushUrl key (Routes.toNavString Routes.Home))
        Routes.Home ->
            let
                (wrapped, cmd) = Home.init flags
            in
                (HomeModel wrapped, Cmd.map HomeMsg cmd)
        Routes.PlayerSelect pageFlags ->
            let
                (wrapped, cmd) = PlayerSelect.init flags pageFlags
            in
                (PlayerSelectModel wrapped, Cmd.map PlayerSelectMsg cmd)
        Routes.Play pageFlags ->
            let
                (wrapped, cmd) = Play.init flags pageFlags
            in
                (PlayModel wrapped, Cmd.map PlayMsg cmd)

view : Model -> Html Msg
view model =
    case model.pageModel of
        HomeModel wrapped ->
            Html.map (PageMsg << HomeMsg) <| Home.view wrapped
        PlayerSelectModel wrapped ->
            Html.map (PageMsg << PlayerSelectMsg) <| PlayerSelect.view wrapped
        PlayModel wrapped ->
            Html.map (PageMsg << PlayMsg) <| Play.view wrapped

update : Msg -> Model -> ( Model, Cmd Msg )
update metaMsg metaModel =
    case (metaModel.pageModel, metaMsg) of
        (HomeModel model, PageMsg (HomeMsg msg)) ->
            case Home.update msg model of
                Routes.Stay (newModel, cmd) -> ({metaModel|pageModel=HomeModel newModel}, Cmd.map (PageMsg << HomeMsg) cmd)
                Routes.Escape route -> (metaModel, Nav.pushUrl metaModel.navKey (Routes.toNavString route))
        (PlayerSelectModel model, PageMsg (PlayerSelectMsg msg)) ->
            case PlayerSelect.update msg model of
                Routes.Stay (newModel, cmd) -> ({metaModel|pageModel=PlayerSelectModel newModel}, Cmd.map (PageMsg << PlayerSelectMsg) cmd)
                Routes.Escape route -> (metaModel, Nav.pushUrl metaModel.navKey (Routes.toNavString route))
        (PlayModel model, PageMsg (PlayMsg msg)) ->
            case Play.update msg model of
                Routes.Stay (newModel, cmd) -> ({metaModel|pageModel=PlayModel newModel}, Cmd.map (PageMsg << PlayMsg) cmd)
                Routes.Escape route -> (metaModel, Nav.pushUrl metaModel.navKey (Routes.toNavString route))
        (_, UrlRequested request) ->
            Debug.todo <| "not sure what to do with url request: " ++ Debug.toString request
        (_, UrlChanged url) ->
            init metaModel.flags url metaModel.navKey
        (_, _) ->
            Debug.log ("ignoring msg: " ++ Debug.toString metaMsg) <|
            Debug.log ("...in state: " ++ Debug.toString metaModel) <|
            (metaModel, Cmd.none)



main = Browser.application
    { init = init
    , update = update
    , view = view >> (\html -> {title="Hanabi", body=[html]})
    , subscriptions = (always Sub.none)
    , onUrlRequest = UrlRequested
    , onUrlChange = UrlChanged
    }

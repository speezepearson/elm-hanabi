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
    , numPageTransitions : Int -- TODO: to keep pages from issuing commands that are obeyed across transitions
    }


type Msg
    = WrappedMsg {numPageTransitions: Int, pageMsg: PageMsg}
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
init = initWithNumPageTransitions 0

initWithNumPageTransitions : Int -> Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
initWithNumPageTransitions numPageTransitions flags url key =
    let
        ( modelToWrap, cmdToWrap ) =
            initPage flags (Routes.fromUrl url) key
        model = wrapPageModel numPageTransitions flags key modelToWrap
    in
    ( model
    , Cmd.map (wrapPageMsg model) cmdToWrap
    )

wrapPageModel : Int -> Flags -> Nav.Key -> PageModel -> Model
wrapPageModel numPageTransitions flags key modelToWrap =
    { flags = flags
    , navKey = key
    , pageModel = modelToWrap
    , numPageTransitions = numPageTransitions
    }

wrapPageMsg : Model -> PageMsg -> Msg
wrapPageMsg model msg =
    WrappedMsg {numPageTransitions=model.numPageTransitions, pageMsg=msg}

initPage : Flags -> Routes.Route -> Nav.Key -> ( PageModel, Cmd PageMsg )
initPage flags route key =
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
            Html.map (wrapPageMsg model << HomeMsg) <| Home.view wrapped

        PlayerSelectModel wrapped ->
            Html.map (wrapPageMsg model << PlayerSelectMsg) <| PlayerSelect.view wrapped

        PlayModel wrapped ->
            Html.map (wrapPageMsg model << PlayMsg) <| Play.view wrapped


update : Msg -> Model -> ( Model, Cmd Msg )
update metaMsg metaModel =
    case metaMsg of

        UrlRequested request ->
            Debug.todo <| "not sure what to do with url request: " ++ Debug.toString request

        UrlChanged url ->
            initWithNumPageTransitions (metaModel.numPageTransitions+1) metaModel.flags url metaModel.navKey

        WrappedMsg {numPageTransitions, pageMsg} ->
            if numPageTransitions < metaModel.numPageTransitions
                then (metaModel, Cmd.none)
                else let (newPageModel, pageCmd) = updatePage metaModel.pageModel pageMsg in
                    ( { metaModel | pageModel = newPageModel}
                    , Cmd.map (wrapPageMsg metaModel) pageCmd
                    )

updatePage : PageModel -> PageMsg -> (PageModel, Cmd PageMsg)
updatePage pageModel pageMsg =
    case (pageModel, pageMsg) of
        ( HomeModel model, HomeMsg msg ) ->
            let (newModel, cmd) = Home.update msg model
            in
                ( HomeModel newModel
                , Cmd.map HomeMsg cmd
                )

        ( PlayerSelectModel model, PlayerSelectMsg msg ) ->
            let (newModel, cmd) = PlayerSelect.update msg model
            in
                ( PlayerSelectModel newModel
                , Cmd.map PlayerSelectMsg cmd
                )

        ( PlayModel model, PlayMsg msg ) ->
            let (newModel, cmd) = Play.update msg model
            in
                ( PlayModel newModel
                , Cmd.map PlayMsg cmd
                )

        ( _, _ ) ->
            Debug.log ("ignoring msg: " ++ Debug.toString pageMsg) <|
                Debug.log ("...in state: " ++ Debug.toString pageModel) <|
                    ( pageModel, Cmd.none )

subscriptions : Model -> Sub PageMsg
subscriptions model =
    case model.pageModel of
        HomeModel m -> Home.subscriptions m |> Sub.map HomeMsg
        PlayerSelectModel m -> PlayerSelect.subscriptions m |> Sub.map PlayerSelectMsg
        PlayModel m -> Play.subscriptions m |> Sub.map PlayMsg

main =
    Browser.application
        { init = init
        , update = update
        , view = view >> (\html -> { title = "Hanabi", body = [ html ] })
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }

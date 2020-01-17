import Browser
import Dict
import Random
import Url
import Url.Builder

import Hanabi.Assistance exposing (History, run)
import Hanabi.Core exposing (randomGame, currentPlayer)
import Hanabi.MVC.API exposing (conn)
import Hanabi.MVC.Core exposing (Msg(..), User, AppModel, PageModel(..))
import Hanabi.MVC.View exposing (view)
import Flags exposing (Flags)

import Pages.Meta as Meta

main = Meta.main

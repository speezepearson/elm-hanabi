port module Ports exposing (notify)

import Json.Decode as D
import Json.Encode as E

port notifyJson : E.Value -> Cmd msg

notify : String -> Cmd msg
notify s = notifyJson (E.string s)

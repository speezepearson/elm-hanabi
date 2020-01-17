module StateServer exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E

type alias Name = String

type alias Connection a =
    { encode : a -> E.Value
    , decoder : D.Decoder a
    , name : Name
    , urlRoot : String
    }

create : (Result Http.Error a -> msg) -> Connection a -> a -> Cmd msg
create msg conn state =
    Http.post
        { url = (Debug.log "create" <| (conn.urlRoot ++ "/" ++ conn.name))
        , body = Http.jsonBody <| E.object [("old", E.null), ("new", conn.encode state)]
        , expect = Http.expectJson msg (D.field "current_state" conn.decoder)
        }

get : (Result Http.Error a -> msg) -> Connection a -> Cmd msg
get msg conn =
    Http.get
        { url = (Debug.log "get" <| (conn.urlRoot ++ "/" ++ conn.name))
        , expect = Http.expectJson msg (D.field "current_state" conn.decoder)
        }

poll : (Result Http.Error a -> msg) -> Connection a -> a -> Cmd msg
poll msg conn current =
    Http.post
        { url = (Debug.log "poll" <| (conn.urlRoot ++ "/" ++ conn.name)) ++ "/poll"
        , body = Http.jsonBody <| E.object [("current_state", conn.encode current)]
        , expect = Http.expectJson msg (D.field "current_state" conn.decoder)
        }

update : (Result Http.Error a -> msg) -> Connection a -> a -> a -> Cmd msg
update msg conn old new =
    Http.post
        { url = (Debug.log "update" <| (conn.urlRoot ++ "/" ++ conn.name))
        , body = Http.jsonBody <| E.object [("old", conn.encode old), ("new", conn.encode new)]
        , expect = Http.expectJson msg (D.field "current_state" conn.decoder)
        }

pollOrGet : (Result Http.Error a -> msg) -> Connection a -> Maybe a -> Cmd msg
pollOrGet msg conn current =
    case current of
        Nothing -> get msg conn
        Just x -> poll msg conn x

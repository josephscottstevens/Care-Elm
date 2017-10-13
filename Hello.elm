module Hello exposing (..)

import Html exposing (Html, text, div)
import Json.Decode exposing (..)

xRaw = """
    {"data": 
    [ {
        "num": 1, 
        "msg": "thing1"
      },
      {
        "num": 2, 
        "msg": "thing2"
      }
    ]}
    """

type alias Comment =
    {
        num : Int
    ,   msg : String
    }
nestedDecoder : Decoder Comment
nestedDecoder =
    map2 Comment
        (field "num" int)
        (field "msg" string)

decoder : Decoder (List Comment)    
decoder =
  at ["data"] (list nestedDecoder)


x : Result String (List Comment)
x = decodeString decoder xRaw

main : Html msg
main =
    case x of
        Err t -> div [] [text t]
        Ok t -> div [] (t |> List.map (\n -> div [] [text n.msg]))
module Hello exposing (tasks)

import Json.Decode exposing (..)

testData : String
testData = """
    {"data": 
    [ {
        "id": 103723, 
        "priority": "high",
        "title": "CARE MANAGER OR WELLNESS COORDINATOR TO PERFORM MONTHLY WELLNESS REVIEW",
        "name": "Oglesby-Odom, Gwendolyn",
        "initiatedOn": "10/01/2017 09:00:00 EST",
        "dueAt": "11/01/2017 11:38:56 EST",
        "closed": true

      },
      {
        "id": 103722, 
        "priority": "medium",
        "title": "CARE MANAGER TO CREATE A CARE PLAN",
        "name": "Oglesby-Odom, Gwendolyn",
        "initiatedOn": "09/01/2017 09:00:00 EST",
        "dueAt": "10/19/2017 14:16:03 EST",
        "closed": false
      }
    ]}
    """

type alias Task =
    {
        id : Int
    ,   priority : String
    ,   title : String
    ,   name : String
    ,   initiatedOn : String
    ,   dueAt : String
    ,   closed : Bool
    }
nestedDecoder : Decoder Task
nestedDecoder =
    map7 Task
        (field "id" int)
        (field "priority" string)
        (field "title" string)
        (field "name" string)
        (field "initiatedOn" string)
        (field "dueAt" string)
        (field "closed" bool)

decoder : Decoder (List Task)    
decoder =
  at ["data"] (list nestedDecoder)


tasks : List Task
tasks =
    case decodeString decoder testData of
        Err t -> []
        Ok t -> t
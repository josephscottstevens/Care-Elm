module Model exposing (..)

import Http


type alias Employment =
    { patientId : Int
    , employer : String
    , occupation : String
    , startDate : String
    }


type Msg
    = Load (Result Http.Error Employment)
    | EditStart Employment
    | EditEnd Employment


type ModelState
    = Initial
    | Grid Employment
    | Edit Employment
    | Error


type alias Model =
    { state : ModelState }

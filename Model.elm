module Model exposing (..)

import Http

type alias Employment =
    {
        patientId : Int
    ,   employer : String
    ,   occupation : String
    ,   startDate : String
    }

type Msg =
    Initial
    | Load (Result Http.Error Employment)
    | Failed
    | Grid Employment
    -- | EditMode Employment

-- type Msg = 
--     GetEmploy (Result Http.Error Employment)
--     | Change String

type alias Model =
  { status : Msg
  }

emptyEmploy : Employment
emptyEmploy = (Employment 0 "" "" "")
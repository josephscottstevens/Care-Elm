module Records.Model exposing (..)

import Table
import Http


type Msg
    = Load (Result Http.Error Model)
    | EditStart Record
    | SetTableState Table.State
    | Reset


type ModelState
    = Initial
    | Grid
    | Edit Record
    | Error Http.Error


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias Model =
    { state : ModelState
    , records : List Record
    , tableState : Table.State
    , query : String
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , records = []
    , tableState = Table.initialSort "dob"
    , query = ""
    }


type alias Record =
    { iD : Int
    , date : Maybe String
    , speciality : Maybe String
    , comments : Maybe String
    , transferedTo : Maybe String
    , transferedOn : Maybe String
    , patientID : Int
    , title : Maybe String
    , dateAccessioned : Maybe String
    , provider : Maybe String
    , patientName : Maybe String
    , recordType : Maybe String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dischargePhysician : Maybe String
    , dischargeDiagnosis : Maybe String
    , hospitalizationServiceType : Maybe String
    , hospitalizationID : Maybe Int
    , reportDate : Maybe String
    , fileName : Maybe String
    , canTransfer : Bool
    , facility : Maybe String
    , facilityFax : Maybe String
    , recommendations : Maybe String
    }

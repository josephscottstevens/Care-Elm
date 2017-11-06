module Records.Model exposing (..)

import Table
import Http
import Utils.CommonGrid exposing (DropDownState)


type Msg
    = Load (Result Http.Error Model)
    | AddNewStart
    | SetTableState Table.State
    | DropdownToggle Record
    | Reset
    | Delete Record
    | DeleteCompleted (Result Http.Error String)
    | ViewFile Int
    | UpdateStartDate String
    | Cancel



--| Dropdown DropDownButtonEvent


type DropDownButtonEvent
    = SendByEmail
    | SendByFax
    | SaveToClientPortal
    | Transfer


type ModelState
    = Initial
    | Grid
    | AddNew
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
    , addNewRecord : NewRecord
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , records = []
    , tableState = Table.initialSort "dob"
    , query = ""
    , addNewRecord = emptyNewRecord
    }


emptyNewRecord : NewRecord
emptyNewRecord =
    { facility = ""
    , category = ""
    , dateTimeOfVisit = ""
    , comments = ""
    , recordFile = ""
    }


type alias NewRecord =
    { facility : String
    , category : String
    , dateTimeOfVisit : String
    , comments : String
    , recordFile : String
    }


type alias Record =
    { id : Int
    , date : Maybe String
    , speciality : Maybe String
    , comments : Maybe String
    , transferedTo : Maybe String
    , transferedOn : Maybe String
    , patientId : Int
    , title : Maybe String
    , dateAccessed : Maybe String
    , provider : Maybe String
    , patientName : Maybe String
    , recordType : Maybe String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dischargePhysician : Maybe String
    , dischargeDiagnosis : Maybe String
    , hospitalizationServiceType : Maybe String
    , hospitalizationId : Maybe Int
    , reportDate : Maybe String
    , fileName : Maybe String
    , canTransfer : Bool
    , facility : Maybe String
    , facilityFax : Maybe String
    , recommendations : Maybe String
    , dropDownState : DropDownState
    }

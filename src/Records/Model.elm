module Records.Model exposing (..)

import Table
import Http


type Msg
    = Load (Result Http.Error Model)
    | AddNewStart
    | SetTableState Table.State
    | DropDownToggle DropDownState
    | Reset
    | Save NewRecord
    | Delete Record
    | DeleteCompleted (Result Http.Error String)
    | SaveCompleted String
    | ViewFile Int
    | UpdateFacility String
    | UpdateCategory String
    | UpdateDateTimeOfVisit String
    | UpdateDoctorOfVisit String
    | UpdateSpecialtyOfVisit String
    | UpdateComments String
    | UpdateRecordFile String
    | Cancel


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
    , showValidationErrors : Bool
    , dropDownState : DropDownState
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , records = []
    , tableState = Table.initialSort "dob"
    , query = ""
    , addNewRecord = emptyNewRecord
    , showValidationErrors = False
    , dropDownState = emptyDropDownState
    }


emptyNewRecord : NewRecord
emptyNewRecord =
    { facility = ""
    , category = ""
    , dateTimeOfVisit = ""
    , doctorOfVisit = ""
    , specialityOfVisit = ""
    , comments = ""
    , recordFile = ""
    }


type alias NewRecord =
    { facility : String
    , category : String
    , dateTimeOfVisit : String
    , doctorOfVisit : String
    , specialityOfVisit : String
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
    }


type alias DropDownState =
    { x : Int
    , y : Int
    , rowId : String
    , showEditMenu : Bool
    }


emptyDropDownState : DropDownState
emptyDropDownState =
    { x = -500
    , y = -500
    , rowId = ""
    , showEditMenu = False
    }

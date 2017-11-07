module Records.Model exposing (..)

import Table
import Http


type alias ImagePortData =
    { contents : String
    , filename : String
    }


type alias Image =
    { contents : String
    , filename : String
    }


type Msg
    = Load (Result Http.Error Model)
    | AddNewStart
    | SetTableState Table.State
    | DropdownToggle Record
    | Reset
    | Save NewRecord
    | Delete Record
    | DeleteCompleted (Result Http.Error String)
      -- | SaveCompleted (Result Http.Error String)
    | ViewFile Int
    | UpdateFacility String
    | UpdateCategory String
    | UpdateDateTimeOfVisit String
    | UpdateDoctorOfVisit String
    | UpdateSpecialtyOfVisit String
    | UpdateComments String
    | UpdateRecordFile String
    | ImageSelected
    | ImageRead ImagePortData
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
    , id : String
    , mImage : Maybe Image
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , records = []
    , tableState = Table.initialSort "dob"
    , query = ""
    , addNewRecord = emptyNewRecord
    , id = "ImageInputId"
    , mImage = Nothing
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
    , dropDownOpen : Bool
    }

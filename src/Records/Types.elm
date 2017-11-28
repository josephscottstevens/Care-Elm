module Records.Types exposing (..)

import Table
import Http
import Common.Types exposing (..)


type Msg
    = Load (Result Http.Error WebResponse)
    | SetTableState Table.State
    | DropDownToggle Int
    | SendMenuMessage Int String
    | SetFilter FilterState
    | EditTask Int
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | AddNewStart


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias WebResponse =
    { facilityId : Maybe Int
    , records : List RecordRow
    }


type alias Model =
    { records : List RecordRow
    , patientId : Int
    , facilityId : Maybe Int
    , recordTypeId : Maybe Int
    , tableState : Table.State
    , query : String
    , filterFields : Filters
    , dropDownState : DropDownState
    }


emptyModel : Flags -> Model
emptyModel flags =
    { records = []
    , patientId = flags.patientId
    , facilityId = Nothing
    , recordTypeId = flags.recordTypeId
    , tableState = Table.initialSort "Date"
    , query = ""
    , filterFields = emptyFilters
    , dropDownState = emptyDropDownState
    }


type alias Filters =
    { date : String
    , dateAccessioned : String
    , provider : String
    , specialty : String
    , comments : String
    , title : String
    , recordingDate : String
    , recording : String
    , taskTitle : String
    , enrollment : String
    , hasVerbalConsent : String
    , staffName : String
    , fileName : String
    , reportDate : String

    -- Hospitilizations:
    , hospitalizationId : String
    , dateOfAdmission : String
    , dateOfDischarge : String
    , hospitalizationServiceType : String
    , recommendations : String
    , dischargePhysician : String
    }


emptyFilters : Filters
emptyFilters =
    { date = ""
    , dateAccessioned = ""
    , provider = ""
    , specialty = ""
    , comments = ""
    , title = ""
    , recordingDate = ""
    , recording = ""
    , taskTitle = ""
    , enrollment = ""
    , hasVerbalConsent = ""
    , staffName = ""
    , fileName = ""
    , reportDate = ""

    -- Hospitilizations:
    , hospitalizationId = ""
    , dateOfAdmission = ""
    , dateOfDischarge = ""
    , hospitalizationServiceType = ""
    , recommendations = ""
    , dischargePhysician = ""
    }


type alias FilterField =
    { fieldName : String
    , fieldText : String
    }


type alias RecordRow =
    { id : Int
    , date : Maybe String
    , specialty : Maybe String
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
    , taskId : Maybe Int
    , taskTitle : Maybe String
    , recording : Maybe String
    , recordingDate : String
    , recordingDuration : Int
    , enrollment : Bool
    , staffId : Int
    , staffName : Maybe String
    , hasVerbalConsent : Bool
    , dropDownOpen : Bool
    }


emptyDropDownState : DropDownState
emptyDropDownState =
    { x = -5000.0
    , y = 0.0
    , rowId = 0
    }


type alias MenuMessage =
    { name : String
    , recordId : Int
    , recordTypeId : Maybe Int
    , hasConsent : Maybe Bool
    }

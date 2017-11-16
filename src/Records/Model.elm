module Records.Model exposing (..)

import Table
import Http
import Utils.CommonTypes exposing (..)


type Msg
    = Load (Result Http.Error WebResponse)
    | AddNewStart
    | SetTableState Table.State
    | DropDownToggle Int
    | Save NewRecord
    | SendMenuMessage Int String
    | SetFilter FilterState
    | OpenTask Int
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | SaveCompleted (Result Http.Error String)
    | UpdateTitle NewRecord String
    | UpdateRecordType NewRecord DropDownItem
    | UpdateSpecialty NewRecord String
    | UpdateProvider NewRecord String
    | UpdateTimeVisit NewRecord (Maybe String)
    | UpdateTimeAcc NewRecord (Maybe String)
    | UpdateFileName NewRecord String
    | UpdateComments NewRecord String
    | UpdateFacility NewRecord DropDownItem
    | UpdateReportDate NewRecord (Maybe String)
    | UpdateCallSid NewRecord String
    | UpdateRecordingSid NewRecord String
    | UpdateDuration NewRecord String
    | UpdateRecordingDate NewRecord (Maybe String)
    | UpdateUser NewRecord DropDownItem
    | UpdateTask NewRecord DropDownItem
    | Cancel


type ModelState
    = Grid
    | AddNew NewRecord
    | Error String


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias WebResponse =
    { facilityId : Maybe Int
    , records : List RecordRow
    , facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , users : List DropDownItem
    , tasks : List DropDownItem
    }


type alias Model =
    { state : ModelState
    , records : List RecordRow
    , facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , users : List DropDownItem
    , tasks : List DropDownItem
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
    { state = Grid
    , records = []
    , facilities = []
    , recordTypes = []
    , users = []
    , tasks = []
    , patientId = flags.patientId
    , facilityId = Nothing
    , recordTypeId = flags.recordType
    , tableState = Table.initialSort "Date"
    , query = ""
    , filterFields = emptyFilters
    , dropDownState = emptyDropDownState
    }



-- , stringColumn "Report Date" (\t -> defaultDate t.reportDate)


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
    }


emptyNewRecord : NewRecord
emptyNewRecord =
    { recordId = 0
    , patientId = 0
    , title = ""
    , recordTypeId = Nothing
    , recordTypeText = ""
    , specialty = ""
    , provider = ""
    , timeVisit = Nothing
    , timeAcc = Nothing
    , fileName = ""
    , comments = ""
    , showValidationErrors = False
    , reportDate = Nothing
    , facilityId = Nothing
    , facilityText = ""
    , recording = ""
    , callSid = ""
    , duration = 0
    , recordingDate = Nothing
    , userId = Nothing
    , userText = ""
    , taskId = Nothing
    , taskText = ""
    }


type alias NewRecord =
    { recordId : Int
    , patientId : Int
    , title : String
    , recordTypeId : Maybe Int
    , recordTypeText : String
    , specialty : String
    , provider : String
    , timeVisit : Maybe String
    , timeAcc : Maybe String
    , fileName : String
    , comments : String
    , showValidationErrors : Bool
    , reportDate : Maybe String
    , facilityId : Maybe Int
    , facilityText : String
    , recording : String
    , callSid : String
    , duration : Int
    , recordingDate : Maybe String
    , userId : Maybe Int
    , userText : String
    , taskId : Maybe Int
    , taskText : String
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


type alias SyncFusionMessage =
    { facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , users : List DropDownItem
    , tasks : List DropDownItem
    , facilityId : Maybe Int
    , recordTypeId : Maybe Int
    }


type alias MenuMessage =
    { name : String
    , recordId : Int
    , recordTypeId : Maybe Int
    , hasConsent : Maybe Bool
    }

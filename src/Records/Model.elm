module Records.Model exposing (..)

import Table
import Http
import Utils.CommonTypes exposing (..)


type Msg
    = Load (Result Http.Error WebResponse)
    | AddNewStart
    | SetTableState Table.State
    | DropDownToggle DropDownState
    | Save NewRecord
    | SendMenuMessage Int String
    | Delete Int
    | DeleteCompleted (Result Http.Error String)
    | SaveCompleted (Result Http.Error String)
    | UpdateFacility NewRecord DropDownItem
    | UpdateCategory NewRecord DropDownItem
    | UpdateDateOfVisit NewRecord String
    | UpdateDoctorOfVisit NewRecord String
    | UpdateSpecialtyOfVisit NewRecord String
    | UpdateComments NewRecord String
    | UpdateRecordFile NewRecord String
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
    }


type alias Model =
    { state : ModelState
    , records : List RecordRow
    , facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , patientId : Int
    , facilityId : Maybe Int
    , recordTypeId : Int
    , tableState : Table.State
    , query : String
    , dropDownState : DropDownState
    }


emptyModel : Flags -> Model
emptyModel flags =
    let
        recordType =
            case flags.recordType of
                Just t ->
                    t

                Nothing ->
                    0
    in
        { state = Grid
        , records = []
        , facilities = []
        , recordTypes = []
        , patientId = flags.patientId
        , facilityId = Nothing
        , recordTypeId = recordType
        , tableState = Table.initialSort "dob"
        , query = ""
        , dropDownState = emptyDropDownState
        }


emptyNewRecord : NewRecord
emptyNewRecord =
    { recordId = 0
    , patientId = 0
    , facilityId = Nothing
    , facility = ""
    , recordType = ""
    , recordTypeId = 0
    , timeVisit = ""
    , provider = ""
    , speciality = ""
    , comments = ""
    , recordFile = ""
    , showValidationErrors = False
    }



--I don't need to track facility's text or recordType's text, just the ID, syncfusion manages the text's state anyway


type alias NewRecord =
    { recordId : Int
    , patientId : Int
    , facilityId : Maybe Int
    , facility : String
    , recordType : String
    , recordTypeId : Int
    , timeVisit : String
    , provider : String
    , speciality : String
    , comments : String
    , recordFile : String
    , showValidationErrors : Bool
    }



--all have facilityId (optional), recordTypeId(category), patientId


type Date
    = String


type DropDown
    = Int



-- TODO: update below, anywhere text and it's dropdown, want int
-- TODO: remove Record from the below names (IE: StandardRecordFields, just be StandardFields)
-- TODO: instead of primitive types... maybe have
--       like, for example, instead of
--       Title : String
--       Title : TextBox Required "title"
--       and link it up with the control types in commonHtml
--       then in common html, also have a textWithButton one for the thing on Records
--       also, need numberBox for Call Recordings


type RecordType
    = PrimaryCare StandardRecordFields
    | Speciality StandardRecordFields
    | Labs LabRecordFields
    | Radiology RadiliologyRecordFields
    | Misc StandardRecordFields
    | Legal LegalRecordFields
    | Hospitalizations HospitilizationRecordFields
    | CallRecordings CallRecordingFields
    | PreviousHistories PreviousHistoriesRecordFields
    | Enrollment EnrollmentRecord


type alias RecordFields =
    { facility : DropDown
    , recordType : DropDown
    , patientId : Int
    , fields : Int
    }


type alias StandardRecordFields =
    { dateTimeOfVisit : Date, doctorOfVisit : Maybe String, specialityOfVisit : Maybe String, comments : String, recordFile : String }


type alias LabRecordFields =
    { dateTimeOfLabsCollected : Date, dateTimeOfLabsAccessioned : Date, nameOfLab : Maybe String, providerOfLab : Maybe String, comments : String, recordFile : String }


type alias RadiliologyRecordFields =
    { dateTimeOfStudyCollected : Date, dateTimeOfStudyAccessioned : Date, nameOfLab : Maybe String, providerOfLab : Maybe String, comments : String, recordFile : String }


type alias LegalRecordFields =
    { title : Maybe String, comments : String, recordFile : String }


type alias HospitilizationRecordFields =
    { facility : Maybe DropDown
    , dateOfAdmission : Date
    , dateOfDiscsharge : Date
    , hospitalServiceType : DropDown
    , dischargeRecommendations : String
    , dischargePhysician : Maybe DropDown
    , comments : String
    , recordFile : String
    }


type alias CallRecordingFields =
    { callSid : Maybe String, recordingSid : Maybe String, durationSec : Int, recordingDate : Date, user : Maybe String, task : Maybe String }


type alias PreviousHistoriesRecordFields =
    { reportDate : Maybe Date, recordFile : String }


type alias EnrollmentRecord =
    { title : Maybe String, comments : String, recordFile : String }



-- includes two new add new buttons
--      which both of these are nested forms
-- Dropdowns
-- Facility -- All Forms
-- Category -- All Forms
-- User -- [Call Recordings]
-- Task -- [Task]
-- Hospitilization Service Type -- [Hospitilizations]
-- Discharge Physician -- [Hospitilizations]


type alias RecordRow =
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


emptyDropDownState : DropDownState
emptyDropDownState =
    { x = -5000.0
    , y = 0.0
    , rowId = 0
    }


type alias SyncFusionMessage =
    { facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , facilityId : Maybe Int
    , recordTypeId : Int
    }


type alias MenuMessage =
    { name : String
    , recordId : Int
    , recordType : Int
    }

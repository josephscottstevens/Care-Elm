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
                    Debug.crash "Invalid recordTypeId, cannot start program"
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
    , title = ""
    , recordTypeId = 0
    , speciality = ""
    , provider = ""
    , timeVisit = ""
    , timeAcc = ""
    , fileName = ""
    , comments = ""
    , showValidationErrors = False
    , facilityId = Nothing
    }



--I don't need to track facility's text or recordType's text, just the ID, syncfusion manages the text's state anyway


type alias NewRecord =
    { recordId : Int
    , patientId : Int
    , title : String
    , recordTypeId : Int
    , speciality : String
    , provider : String
    , timeVisit : String
    , timeAcc : String
    , fileName : String
    , comments : String
    , showValidationErrors : Bool
    , facilityId : Maybe Int
    }



--all have facilityId (optional), recordTypeId(category), patientId


type Date
    = String


type DropDown
    = Int


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

module Records.Types exposing (Model, RecordRow, Filters, emptyModel)

import Table
import Common.Types exposing (RecordType)


type alias Model =
    { recordType : RecordType
    , records : List RecordRow
    , tableState : Table.State
    , query : String
    , filterFields : Filters
    , dropDownState : Int
    }


emptyModel : RecordType -> Model
emptyModel recordType =
    { recordType = recordType
    , records = []
    , tableState = Table.initialSort "Date"
    , query = ""
    , filterFields = emptyFilters
    , dropDownState = -1
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


type alias RecordRow =
    { id : Int
    , date : Maybe String
    , specialty : Maybe String
    , comments : Maybe String
    , transferedTo : Maybe String
    , patientId : Int
    , title : Maybe String
    , dateAccessed : Maybe String
    , provider : Maybe String
    , recordType : Maybe String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dischargePhysician : Maybe String
    , dischargeDiagnosis : Maybe String
    , hospitalizationServiceType : Maybe String
    , hospitalizationId : Maybe Int
    , reportDate : Maybe String
    , fileName : Maybe String
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

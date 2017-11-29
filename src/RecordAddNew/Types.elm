module RecordAddNew.Types exposing (..)

import Common.Types exposing (..)
import Http


type Msg
    = AddNewFacility
    | AddNewPhysician
    | Save
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateTitle String
    | UpdateRecordType DropDownItem
    | UpdateSpecialty String
    | UpdateProvider String
    | UpdateTimeVisit (Maybe String)
    | UpdateTimeAcc (Maybe String)
    | UpdateFileName String
    | UpdateComments String
    | UpdateFacility DropDownItem
    | UpdateReportDate (Maybe String)
    | UpdateCallSid String
    | UpdateRecordingSid String
    | UpdateDuration String
    | UpdateRecordingDate (Maybe String)
    | UpdateUser DropDownItem
    | UpdateTask DropDownItem
      -- Hospitilizations
    | UpdateIsExistingHospitilization Bool
    | UpdateHospitilization DropDownItem
    | UpdatePatientReported Bool
    | UpdateFacility2 DropDownItem
    | UpdateDateOfAdmission (Maybe String)
    | UpdateDateOfDischarge (Maybe String)
    | UpdateHospitalServiceType DropDownItem
    | UpdateDischargeRecommendations String
    | UpdateDischargePhysician DropDownItem


type alias Model =
    { recordId : Int
    , title : String
    , patientId : Int
    , recordType : RecordType
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

    -- Hospitilizations
    , isExistingHospitilization : Bool
    , patientReported : Bool
    , hospitalizationId : Maybe Int
    , hospitalizationText : String
    , facilityId2 : Maybe Int
    , facilityText2 : String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , hospitalServiceTypeId : Maybe Int
    , hospitalServiceTypeText : String
    , dischargeRecommendations : String
    , dischargePhysicianId : Maybe Int
    , dischargePhysicianText : String
    }


emptyModel : RecordType -> Int -> Model
emptyModel recordType patientId =
    { recordId = 0
    , title = ""
    , patientId = patientId
    , recordType = recordType
    , recordTypeId = getId recordType
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

    -- Hospitilizations
    , isExistingHospitilization = False
    , patientReported = False
    , hospitalizationId = Nothing
    , hospitalizationText = ""
    , facilityId2 = Nothing
    , facilityText2 = ""
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , hospitalServiceTypeId = Nothing
    , hospitalServiceTypeText = ""
    , dischargeRecommendations = ""
    , dischargePhysicianId = Nothing
    , dischargePhysicianText = ""
    }

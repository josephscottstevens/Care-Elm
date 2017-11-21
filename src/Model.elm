module Model exposing (..)

import Billing.Model as Billing
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Utils.CommonTypes exposing (..)
import Http


type Page
    = NoPage
    | BillingPage
    | RecordsPage
    | RecordAddNewPage AddEditDataSource
    | Error String


type alias Model =
    { page : Page
    , patientId : Int
    , recordType : Maybe Int
    , billingState : Billing.Model
    , recordsState : Records.Model
    , recordAddNewState : RecordAddNew.Model
    , addEditDataSource : Maybe AddEditDataSource
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddNewStart AddEditDataSource
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , patientId = flags.patientId
    , recordType = flags.recordType
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    , recordAddNewState = RecordAddNew.emptyModel flags
    , addEditDataSource = Nothing
    }



-- getAddEditDataSource : AddEditDataSource -> RecordAddNew.Model
-- getAddEditDataSource t =
--     let
--         emptyModel =
--             RecordAddNew.emptyModel
--     in
--         { emptyModel
--             | facilityId = t.facilityId
--             , facilities = t.facilities
--             , recordTypes = t.recordTypes
--             , tasks = t.tasks
--             , users = t.users
--             , hospitilizationServiceTypes = t.hospitilizationServiceTypes
--             , hospitalizationDischargePhysicians = t.hospitalizationDischargePhysicians
--         }

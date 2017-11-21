module Model exposing (..)

import Billing.Types as Billing
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Utils.CommonTypes exposing (..)
import Http


type State
    = NoPage
    | BillingPage Billing.Model
    | RecordsPage Records.Model
    | RecordAddNewPage
    | Error String


type alias Model =
    { state : State
    , patientId : Int
    , recordTypeId : Maybe Int
    , recordAddNewState : RecordAddNew.Model
    , addEditDataSource : Maybe AddEditDataSource
    }


type Msg
    = BillingMsg Billing.Msg Billing.Model
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddNewStart AddEditDataSource
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | UpdatePage String


emptyModel : Flags -> Model
emptyModel flags =
    { state = NoPage
    , patientId = flags.patientId
    , recordTypeId = flags.recordType
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

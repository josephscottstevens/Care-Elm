module Model exposing (..)

import Billing.Types as Billing
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Hospitilizations.Types as Hospitilizations
import Common.Types exposing (..)
import Http


type alias Model =
    { page : Page
    , flags : Flags
    , addEditDataSource : Maybe AddEditDataSource
    , billingState : Billing.Model
    , recordsState : Records.Model
    , recordAddNewState : RecordAddNew.Model
    , hospitalizationsState : Hospitilizations.Model
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | HospitilizationsMsg Hospitilizations.Msg
    | PageLoadStart String
    | PageLoadComplete String


emptyModel : Flags -> Model
emptyModel flags =
    { page = None
    , flags = flags
    , addEditDataSource = Nothing
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    , recordAddNewState = RecordAddNew.emptyModel flags
    , hospitalizationsState = Hospitilizations.emptyModel flags
    }

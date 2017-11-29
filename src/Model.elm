module Model exposing (..)

import Billing.Types as Billing
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Hospitilizations.Types as Hospitilizations
import HospitilizationsAddEdit.Types as HospitilizationsAddEdit
import Common.Types exposing (..)
import Http
import Navigation


type alias Model =
    { page : Page
    , flags : Flags
    , addEditDataSource : Maybe AddEditDataSource
    , billingState : Billing.Model
    , recordsState : Records.Model
    , recordAddNewState : RecordAddNew.Model
    , hospitalizationsState : Hospitilizations.Model
    , hospitilizationsAddEditState : HospitilizationsAddEdit.Model
    , currentUrl : Navigation.Location
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | HospitilizationsMsg Hospitilizations.Msg
    | HospitilizationsAddEditMsg HospitilizationsAddEdit.Msg
    | PresetPageComplete String
    | SetPageComplete String
    | UrlChange Navigation.Location


emptyModel : Navigation.Location -> Flags -> Model
emptyModel location flags =
    { page = None
    , flags = flags
    , addEditDataSource = Nothing
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    , recordAddNewState = RecordAddNew.emptyModel flags
    , hospitalizationsState = Hospitilizations.emptyModel flags
    , hospitilizationsAddEditState = HospitilizationsAddEdit.emptyModel flags
    , currentUrl = location
    }

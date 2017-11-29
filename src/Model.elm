module Model exposing (..)

import Billing.Types as Billing
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Hospitilizations.Types as Hospitilizations
import HospitilizationsAddEdit.Types as HospitilizationsAddEdit
import Common.Types exposing (..)
import Http
import Navigation
import Common.Routes exposing (getPage, getPatientId)


type alias Model =
    { page : Page
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
    | IsApp String


emptyModel : Navigation.Location -> Model
emptyModel location =
    let
        page =
            getPage location.href

        patientId =
            getPatientId location.href
    in
        { page = None
        , addEditDataSource = Nothing
        , billingState = Billing.emptyModel
        , recordsState = Records.emptyModel recordType patientId
        , recordAddNewState = RecordAddNew.emptyModel recordType patientId
        , hospitalizationsState = Hospitilizations.emptyModel patientId
        , hospitilizationsAddEditState = HospitilizationsAddEdit.emptyModel patientId
        , currentUrl = location
        }

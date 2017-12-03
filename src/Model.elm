module Model exposing (..)

import Billing.Types as Billing
import Hospitilizations.Types as Hospitilizations
import HospitilizationsAddEdit.Types as HospitilizationsAddEdit
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Common.Types exposing (..)
import Http
import Navigation
import Common.Routes exposing (getPage, getPatientId)


type alias Model =
    { page : Page
    , patientId : Int
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
    | UrlChange Navigation.Location
    | KnockoutUrlChange String


emptyModel : Navigation.Location -> Model
emptyModel location =
    let
        page =
            getPage location.hash

        patientId =
            getPatientId location.search
    in
        { page = None
        , patientId = patientId
        , addEditDataSource = Nothing
        , billingState = Billing.emptyModel
        , recordsState = Records.emptyModel patientId
        , recordAddNewState = RecordAddNew.emptyModel patientId
        , hospitalizationsState = Hospitilizations.emptyModel patientId
        , hospitilizationsAddEditState = HospitilizationsAddEdit.emptyModel patientId
        , currentUrl = location
        }

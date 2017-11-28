module Model exposing (..)

import Billing.Types as Billing
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Hospitilizations.Types as Hospitalizations
import Common.Types exposing (..)
import Http


type Page
    = NoPage
    | BillingPage
    | RecordsPage
    | RecordAddNewPage
    | Error String


type alias Model =
    { page : Page
    , flags : Flags
    , addEditDataSource : Maybe AddEditDataSource
    , billingState : Billing.Model
    , recordsState : Records.Model
    , recordAddNewState : Maybe RecordAddNew.Model
    , hospitalizationsState : Maybe Hospitalizations.Model
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)



-- | UpdatePage ( String, Maybe Int )


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , flags = flags
    , addEditDataSource = Nothing
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    , recordAddNewState = Nothing
    , hospitalizationsState = Hospitalizations.emptyModel flags
    }

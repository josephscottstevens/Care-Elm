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
    | RecordAddNewPage RecordAddNew.Model
    | Error String


type alias Model =
    { state : State
    , flags : Flags
    , addEditDataSource : Maybe AddEditDataSource
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddNewStart AddEditDataSource
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | UpdatePage String


emptyModel : Flags -> Model
emptyModel flags =
    { state = NoPage
    , flags = flags
    , addEditDataSource = Nothing
    }

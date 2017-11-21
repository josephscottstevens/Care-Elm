module Model exposing (..)

import Billing.Model as Billing
import Records.Types as Records
import RecordAddNew.Types as RecordAddNew
import Utils.CommonTypes exposing (..)


type Page
    = NoPage
    | BillingPage
    | RecordsPage
    | RecordAddNewPage


type alias Model =
    { page : Page
    , patientId : Int
    , recordType : Maybe Int
    , billingState : Billing.Model
    , recordsState : Records.Model
    , recordAddNewState : RecordAddNew.Model
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddNewStart


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , patientId = flags.patientId
    , recordType = flags.recordType
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    , recordAddNewState = RecordAddNew.emptyModel
    }

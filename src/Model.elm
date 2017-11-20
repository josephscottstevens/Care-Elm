module Model exposing (..)

import Billing.Model as Billing
import Records.Types as Records
import Utils.CommonTypes exposing (..)


type Page
    = NoPage
    | BillingPage
    | RecordsPage


type alias Model =
    { page : Page
    , patientId : Int
    , recordType : Maybe Int
    , billingState : Billing.Model
    , recordsState : Records.Model
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | AddNewStart


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , patientId = flags.patientId
    , recordType = flags.recordType
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    }

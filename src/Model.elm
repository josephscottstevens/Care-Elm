module Model exposing (..)

import Billing.Model as Billing
import Records.Model as Records
import Utils.CommonTypes exposing (Flags)


type Page
    = NoPage
    | BillingPage
    | RecordsPage


type alias Model =
    { page : Page
    , billingState : Billing.Model
    , recordsState : Records.Model
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    }

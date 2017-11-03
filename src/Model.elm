module Model exposing (..)

import Billing.Model as Billing
import Records.Model as Records


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


emptyModel : Model
emptyModel =
    { page = NoPage
    , billingState = Billing.emptyModel
    , recordsState = Records.emptyModel
    }

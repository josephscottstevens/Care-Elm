module Model exposing (..)

import Billing.Types
import Http


type Page
    = NoPage
    | BillingPage


type alias Model =
    { page : Page
    , billingState : Billing.Types.Model
    }


type Msg
    = NoMessage
    | OpenBilling
    | BillingMsg Billing.Types.Model Billing.Types.Msg
    | BillingLoad (Result Http.Error Billing.Types.Model)
    | BillingMsgTag Billing.Types.Msg


emptyModel : Model
emptyModel =
    { page = NoPage
    , billingState = Billing.Types.emptyModel
    }

module Model exposing (..)

import Billing.Types


type Page
    = NoPage
    | BillingPage


type alias Model =
    { page : Page
    , billingState : Billing.Types.Model
    }


type Msg
    = NoMessage
    | BillingMsg Billing.Types.Model Billing.Types.Msg


emptyModel : Model
emptyModel =
    { page = NoPage
    , billingState = Billing.Types.emptyModel
    }

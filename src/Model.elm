module Model exposing (..)

import Billing.Model as Billing


type Page
    = NoPage
    | BillingPage


type alias Model =
    { page : Page
    , billingState : Billing.Model
    }


type Msg
    = BillingMsg Billing.Msg


emptyModel : Model
emptyModel =
    { page = NoPage
    , billingState = Billing.emptyModel
    }

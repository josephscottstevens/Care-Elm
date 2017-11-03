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
    = OpenPage String
    | BillingMsg Billing.Types.Msg


emptyModel : Model
emptyModel =
    { page = NoPage
    , billingState = Billing.Types.emptyModel
    }

module Model exposing (..)

import Billing.Types


type Page
    = NoPage
    | BillingPage Billing.Types.Model


type alias Model =
    { page : Page
    }


type Msg
    = NoMessage
    | BillingMsg Billing.Types.Msg


emptyModel : Model
emptyModel =
    { page = NoPage
    }

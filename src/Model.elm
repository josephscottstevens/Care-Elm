module Model exposing (..)

import Billing.Model as Billing
import Records.Model as Records
import Demographics
import Utils.CommonTypes exposing (Flags)


type Page
    = NoPage
    | BillingPage
    | RecordsPage
    | DemographicsPage


type alias Model =
    { page : Page
    , billingState : Billing.Model
    , recordsState : Records.Model
    , demographicsState : Demographics.Model
    }


type Msg
    = BillingMsg Billing.Msg
    | RecordsMsg Records.Msg
    | DemographicsMsg Demographics.Msg


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , billingState = Billing.emptyModel flags
    , recordsState = Records.emptyModel flags
    , demographicsState = Demographics.emptyModel flags
    }

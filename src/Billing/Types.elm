module Billing.Types exposing (..)

import Common.Table as Table
import Http


type Page
    = First
    | Previous
    | PreviousBlock
    | Index Int
    | NextBlock
    | Next
    | Last


type Msg
    = Load (Result Http.Error Model)
    | EditStart BillingCcm
      -- | EditSave Enrollment
    | EditCancel
      -- | UpdateStartDate String
      -- | UpdateCity Enrollment String
      -- | UpdateState Enrollment String
    | SetPagingState Page
    | SetQuery String
    | SetTableState Table.State
    | Reset


type ModelState
    = Initial
    | Grid
    | Edit BillingCcm
    | Error Http.Error


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias Model =
    { state : ModelState
    , billingCcm : List BillingCcm
    , tableState : Table.State
    , query : String
    , currentPage : Int
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , billingCcm = []
    , tableState = Table.initialSort "dob"
    , query = ""
    , currentPage = 0
    }


type alias BillingCcm =
    { iD : Int
    , facility : String
    , facilityId : Int
    , practiceLocation : Maybe String
    , mainProvider : String
    , providerId : Int
    , patientName : String
    , patientId : Int
    , dob : String
    , patientFacilityIdNo : Maybe String
    , phone : String
    , assignedTo : Maybe String
    , staffId : Maybe Int
    , openTasks : Int
    , totalTimeSpent : Maybe Int
    , ccmRegistrationDate : String
    , dateOfService : String
    , billingDate : String
    , billingMonth : Int
    , billingYear : Int
    , isClosed : Bool
    , tocId : Maybe Int
    , readmission : Bool
    , isComplexCCM : Bool
    , batchCloseOnInvoiceCompletion : Bool
    , reviewedByStaffName : Maybe String
    , canModifyReviewedStatus : Bool
    , cPT : String
    , isReviewed : Bool
    , dxPresent : Bool
    , carePlanPresent : Bool
    , medsPresent : Bool
    , allergiesPresent : Bool
    , vitalsPresent : Bool
    , recordingPresent : Bool
    , chartComplete : Bool
    , status : String
    , is24HoursSinceBilledString : String
    }

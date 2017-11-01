module Model exposing (..)

import Http
import Table
import GridPaging


type Msg
    = Load (Result Http.Error Model)
    | EditStart BillingCcm
      -- | EditSave Enrollment
    | EditCancel
      -- | UpdateStartDate String
      -- | UpdateCity Enrollment String
      -- | UpdateState Enrollment String
    | SetPagingState GridPaging.PageState
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
    , pageState : GridPaging.PageState
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , billingCcm = []
    , tableState = Table.initialSort "dob"
    , query = ""
    , pageState = GridPaging.initialPageState
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

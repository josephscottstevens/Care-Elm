module Model exposing (..)

import Http
import Table


type Msg
    = Load (Result Http.Error Model)
    | EditStart Enrollment
      -- | EditSave Enrollment
    | EditCancel
      -- | UpdateStartDate String
      -- | UpdateCity Enrollment String
      -- | UpdateState Enrollment String
    | SetQuery String
    | SetTableState Table.State
    | Reset


type ModelState
    = Initial
    | Grid
    | Edit Enrollment
    | Error Http.Error


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias Enrollment =
    { iD : Int
    , facilityId : Int
    , providerId : Maybe Int
    , firstName : Maybe String
    , middleName : Maybe String
    , lastName : Maybe String
    , sexTypeId : Maybe Int
    , dob : String
    , sSN : Maybe String
    , mRN : Maybe String
    , pAN : Maybe String
    , email : Maybe String
    , primaryPhone : String
    , primaryPhoneNumberTypeId : Maybe Int
    , secondaryPhone : Maybe String
    , secondaryPhoneNumberTypeId : Maybe Int
    , address : Maybe String
    , address2 : Maybe String
    , address3 : Maybe String
    , city : Maybe String
    , stateId : Maybe String
    , zip : Maybe String
    , proxyFirstName : Maybe String
    , proxyMiddleName : Maybe String
    , proxyLastName : Maybe String
    , proxyRelationshipTypeId : Maybe Int
    , proxyPhone : Maybe String
    , proxyPhoneNumberTypeId : Maybe Int
    , facility : String
    , provider : String
    , name : String
    , primaryInsurance : Maybe String
    , secondaryInsurance : Maybe String
    , status : String
    , assignedTo : String
    , proxyName : String
    , lastContactAttempt : Maybe String
    , contactAttempts : Maybe String
    , comments : Maybe String
    , existingComments : Maybe String
    , importDate : String
    , consentObtained : Maybe String
    , elligibleICD10 : Int
    , elligibleICD9 : Int
    , disableCall : Bool
    , batchId : Int
    , canRegister : Bool
    }


type alias Model =
    { state : ModelState
    , enrollment : List Enrollment
    , tableState : Table.State
    , query : String
    , currentPage : Int
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , enrollment = []
    , tableState = Table.initialSort "dob"
    , query = ""
    , currentPage = 0
    }

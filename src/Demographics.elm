port module Demographics exposing (..)

import Html exposing (Html, text, div, button, ul, li, a, input, label)
import Html.Attributes exposing (class, id, type_, value, style)
import Utils.CommonTypes exposing (DropDownItem, Flags)


-- port setUnsavedChanges : Bool -> Cmd msg
-- port resetUpdateComplete : (String -> msg) -> Sub msg


type alias Model =
    { patientId : Int
    , demographicsId : Maybe Int
    , nickName : Maybe String
    , vip : Maybe Bool
    , ssn : Maybe String
    , lastName : Maybe String
    , firstName : Maybe String
    , middle : Maybe String
    , dateOfBirth : Maybe String
    , birthPlace : Maybe String
    , dateOfDeath : Maybe String
    , mrn : Maybe String
    , patientAccountNumber : Maybe String
    , facilityPtID : Maybe String
    , sexualOrientationNote : Maybe String
    , genderIdentityNote : Maybe String
    , email : Maybe String
    , careCoordinatorId : Maybe Int
    , ethnicityId : Maybe Int
    , sexTypeId : Maybe Int
    , sexualOrientationId : Maybe Int
    , genderIdentityId : Maybe Int
    , facilityId : Maybe Int
    , mainProviderId : Maybe Int
    , raceId : Maybe Int
    , suffixId : Maybe Int
    , prefixId : Maybe Int
    , uSVeteranId : Maybe Int
    , religionId : Maybe Int
    , patientLanguagesMap : List PatientLanguagesMap
    , patientLanguageDropdown : List DropDownItem
    , careCoordinatorDropdown : List DropDownItem
    , languageDropdown : List DropDownItem
    , ethnicityDropdown : List DropDownItem
    , sexTypeDropdown : List DropDownItem
    , sexualOrientationDropdown : List DropDownItem
    , genderIdentityDropdown : List DropDownItem
    , facilityDropdown : List DropDownItem
    , mainProviderDropdown : List DropDownItem
    , raceDropdown : List DropDownItem
    , suffixDropdown : List DropDownItem
    , prefixDropdown : List DropDownItem
    , uSVeteranDropdown : List DropDownItem
    , religionDropdown : List DropDownItem
    , preferredLanguageIndex : Int
    }


type alias PatientLanguagesMap =
    { bob : String }


subscriptions : Sub Msg
subscriptions =
    Sub.none


init : Flags -> Cmd Msg
init flag =
    Cmd.batch
        []


view : Model -> Html Msg
view model =
    div [ class "form-group" ]
        [ label [ class "col-md-4" ] [ text "comments" ]
        , div [ class "col-md-8" ]
            [ input [ class "e-textbox" ] []
            ]
        ]


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            model ! []


emptyModel : Flags -> Model
emptyModel flags =
    { patientId = flags.patientId
    , demographicsId = Nothing
    , nickName = Nothing
    , vip = Nothing
    , ssn = Nothing
    , lastName = Nothing
    , firstName = Nothing
    , middle = Nothing
    , dateOfBirth = Nothing
    , birthPlace = Nothing
    , dateOfDeath = Nothing
    , mrn = Nothing
    , patientAccountNumber = Nothing
    , facilityPtID = Nothing
    , sexualOrientationNote = Nothing
    , genderIdentityNote = Nothing
    , email = Nothing
    , careCoordinatorId = Nothing
    , ethnicityId = Nothing
    , sexTypeId = Nothing
    , sexualOrientationId = Nothing
    , genderIdentityId = Nothing
    , facilityId = Nothing
    , mainProviderId = Nothing
    , raceId = Nothing
    , suffixId = Nothing
    , prefixId = Nothing
    , uSVeteranId = Nothing
    , religionId = Nothing
    , patientLanguagesMap = []
    , patientLanguageDropdown = []
    , careCoordinatorDropdown = []
    , languageDropdown = []
    , ethnicityDropdown = []
    , sexTypeDropdown = []
    , sexualOrientationDropdown = []
    , genderIdentityDropdown = []
    , facilityDropdown = []
    , mainProviderDropdown = []
    , raceDropdown = []
    , suffixDropdown = []
    , prefixDropdown = []
    , uSVeteranDropdown = []
    , religionDropdown = []
    , preferredLanguageIndex = -1
    }

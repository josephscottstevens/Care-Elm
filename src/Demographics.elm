port module Demographics exposing (..)

import Html exposing (Html, text, div, span, button, ul, li, a, input, label, h4)
import Html.Attributes exposing (class, id, type_, value, style, title, checked, hidden, attribute)
import Html.Events exposing (onClick)
import Utils.CommonTypes exposing (DropDownItem, Flags)
import Utils.CommonFunctions exposing (decodeDropDownItem)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Http
import Char


port initDemographics : SfData -> Cmd msg


port initDemographicsDone : (String -> msg) -> Sub msg


port initContactHours : String -> Cmd msg


port initLanguagesMap : PatiantLanguageMessage -> Cmd msg


port updateLanguagesMap : (PatiantLanguageMessage -> msg) -> Sub msg


port updateDemographics : (SfData -> msg) -> Sub msg


port logError : String -> Cmd msg


type alias Model =
    { d : DemographicsInformationModel
    , c : ContactInformationModel
    }


type alias DemographicsInformationModel =
    { patientId : Int
    , demographicsId : Maybe Int
    , nickName : Maybe String
    , ssn : Maybe String
    , lastName : Maybe String
    , firstName : Maybe String
    , middle : Maybe String
    , birthPlace : Maybe String
    , mrn : Maybe String
    , patientAccountNumber : Maybe String
    , facilityPtID : Maybe String
    , sexualOrientationNote : Maybe String
    , genderIdentityNote : Maybe String
    , email : Maybe String
    , preferredLanguageIndex : Int
    , sfData : SfData
    , patientLanguagesMap : List PatientLanguagesMap
    , patientLanguagesMapCounter : Int
    }


type alias ContactInformationModel =
    { patientPhoneNumbers : List PatientPhoneNumber
    , patientAddresses : List PatientAddress
    , phoneNumberTypeDropdown : List DropDownItem
    , stateDropdown : List DropDownItem
    , primaryAddressIndex : Int
    , preferredPhoneIndex : Int
    }


type alias PatientPhoneNumber =
    { id : Maybe Int
    , phoneNumber : Maybe String
    , phoneNumberTypeId : Maybe Int
    , isPreferred : Bool
    }


type alias PatientAddress =
    { id : Maybe Int
    , addressLine1 : Maybe String
    , addressLine2 : Maybe String
    , addressLine3 : Maybe String
    , city : Maybe String
    , stateId : Int
    , zipCode : Maybe String
    , isPrimary : Bool
    }


type alias SfData =
    { facilityId : Maybe Int
    , mainProviderId : Maybe Int
    , careCoordinatorId : Maybe Int
    , prefixId : Maybe Int
    , sexTypeId : Maybe Int
    , sexualOrientationId : Maybe Int
    , suffixId : Maybe Int
    , genderIdentityId : Maybe Int
    , raceId : Maybe Int
    , ethnicityId : Maybe Int
    , uSVeteranId : Maybe Int
    , religionId : Maybe Int
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
    , dateOfBirth : Maybe String
    , dateOfDeath : Maybe String
    , vip : Maybe Bool
    }


type alias PatientLanguagesMap =
    { id : Maybe Int
    , languageId : Int
    , isPreferred : Bool
    , index : Int
    }


type alias PatiantLanguageMessage =
    { patientLanguagesMap : PatientLanguagesMap
    , patientLanguageDropdown : List DropDownItem
    }


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ updateDemographics UpdateDemographics
        , initDemographicsDone InitDemographicsDone
        ]


maybeValue : Maybe String -> Html.Attribute msg
maybeValue str =
    value (Maybe.withDefault "" str)


rowStyle : List (Html.Attribute msg)
rowStyle =
    [ class "col-xs-12 col-sm-12 col-md-5 col-lg-4 padding-left-0" ]


idAttr : String -> Html.Attribute msg
idAttr str =
    id ((String.filter isAlpha str) ++ "Id")


isAlpha : Char -> Bool
isAlpha char =
    Char.isLower char || Char.isUpper char


isRequiredClass : Bool -> Html.Attribute msg
isRequiredClass isRequired =
    case isRequired of
        True ->
            class "required "

        False ->
            class ""


commonStructure : String -> Bool -> Html msg -> Html msg
commonStructure displayText isRequired t =
    div [ class "col-xs-12 padding-h-0" ]
        [ label [ isRequiredClass isRequired ] [ text (displayText ++ ":") ]
        , div [ class "DemographicsInputDiv padding-h-0" ]
            [ t ]
        ]


onlyNumbers : Html.Attribute msg
onlyNumbers =
    attribute "onkeypress" "return event.charCode >= 48 && event.charCode <= 57"


noNumbers : Html.Attribute msg
noNumbers =
    attribute "onkeypress" "return event.charCode < 48 || event.charCode > 57"


textbox : String -> Bool -> Maybe String -> Html msg
textbox displayText isRequired maybeStr =
    commonStructure displayText isRequired <|
        input [ type_ "text", idAttr displayText, maybeValue maybeStr, class "e-textbox" ] []


numberbox : String -> Bool -> Maybe String -> Html msg
numberbox displayText isRequired maybeStr =
    commonStructure displayText isRequired <|
        input [ type_ "text", idAttr displayText, maybeValue maybeStr, onlyNumbers, class "e-textbox" ] []


nonumberbox : String -> Bool -> Maybe String -> Html msg
nonumberbox displayText isRequired maybeStr =
    commonStructure displayText isRequired <|
        input [ type_ "text", idAttr displayText, maybeValue maybeStr, noNumbers, class "e-textbox" ] []


sfbox : String -> Bool -> Html msg
sfbox displayText isRequired =
    commonStructure displayText isRequired <|
        input [ type_ "text", idAttr displayText, class "e-textbox" ] []


sfcheckbox : String -> Bool -> Maybe String -> Html msg
sfcheckbox displayText isRequired maybeStr =
    commonStructure displayText isRequired <|
        input [ type_ "checkbox", idAttr displayText, class "e-checkbox" ] []


view : Model -> Html Msg
view { d, c } =
    div [ id "demographicInformationForm", class "col-xs-12 padding-h-0" ]
        [ h4 [ class "col-xs-12 padding-h-0" ] [ text "Assigned To" ]
        , div [ class "col-xs-12 padding-h-0" ]
            -- TODO
            [ div [ class "error", hidden True ] []
            ]
        , div rowStyle
            [ sfbox "Facility" True
            , textbox "Patient's Facility ID No" True d.facilityPtID
            , numberbox "Medical Record No" False d.mrn
            , numberbox "Patient Account No" False d.patientAccountNumber
            ]
        , div rowStyle
            [ sfbox "Main Provider" True
            , sfbox "Care Coordinator" True
            ]
        , h4 [ class "col-xs-12 padding-h-0 padding-top-10" ] [ text "Demographic Information" ]
        , div rowStyle
            [ sfbox "Prefix" False
            , nonumberbox "First Name" True d.firstName
            , nonumberbox "Middle Name" False d.middle
            , nonumberbox "Last Name" True d.lastName
            , sfbox "Suffix" False
            , textbox "Nickname" False d.nickName
            , sfbox "Date of Birth" True
            , textbox "Birth Place" False d.birthPlace
            , sfbox "Date of Death" False
            , textbox "SSN" False d.ssn
            ]
        , div rowStyle
            [ sfbox "VIP" False
            , sfbox "Sex at Birth" True
            , sfbox "Sexual Orientation" False
            , textbox "Sexual Orientation Note" False d.sexualOrientationNote
            , sfbox "Gender Identity" False
            , textbox "Gender Identity Note" False d.genderIdentityNote
            , sfbox "Race" False
            , sfbox "Ethnicity" False
            , sfbox "US Veteran" False
            , sfbox "Religion" False
            , textbox "Email" False d.email
            ]
        , div [ class "col-xs-12 padding-h-0 padding-top-10" ]
            [ div [ class "col-xs-12 col-sm-12 col-md-10 col-lg-8 padding-h-0" ]
                [ h4 [ class "inline-block" ] [ text "Languages" ]
                , div [ class "inline-block e-tooltxt pointer", title "Add new language", onClick AddNewLanguage ]
                    [ span [ class "e-addnewitem e-toolbaricons e-icon e-addnew" ] []
                    ]
                , div [] (List.map viewLanguages d.patientLanguagesMap)
                ]
            ]
        ]



--TODO add  events


viewLanguages : PatientLanguagesMap -> Html Msg
viewLanguages lang =
    div [ class "margin-bottom-5", style [ ( "width", "350px" ) ] ]
        [ div [ class "inline-block ", style [ ( "width", "20px" ), ( "padding-top", "5px" ), ( "vertical-align", "middle" ) ], title "Mark as preferred" ]
            [ input [ type_ "radio", checked lang.isPreferred ] [] ]
        , div [ class "inline-block", style [ ( "width", "calc(100% - 50px)" ), ( "vertical-align", "middle" ) ] ]
            [ input [ id ("PatientLanguagesMapId" ++ (toString lang.index)) ] [] ]
        , div [ class "inline-block", style [ ( "width", "20px" ), ( "vertical-align", "middle" ) ], title "remove", onClick (RemoveLanguage lang.index) ]
            [ span [ class "e-cancel e-toolbaricons e-icon e-cancel margin-bottom-5 pointer" ] []
            ]
        ]


viewPhones : PatientLanguagesMap -> Html Msg
viewPhones lang =
    div [ class "margin-bottom-5", style [ ( "width", "350px" ) ] ]
        [ div [ class "inline-block ", style [ ( "width", "20px" ), ( "padding-top", "5px" ), ( "vertical-align", "middle" ) ], title "Mark as preferred" ]
            [ input [ type_ "radio", checked lang.isPreferred ] [] ]
        , div [ class "inline-block", style [ ( "width", "calc(100% - 50px)" ), ( "vertical-align", "middle" ) ] ]
            [ input [ id ("PatientLanguagesMapId" ++ (toString lang.index)) ] [] ]
        , div [ class "inline-block", style [ ( "width", "20px" ), ( "vertical-align", "middle" ) ], title "remove", onClick (RemoveLanguage lang.index) ]
            [ span [ class "e-cancel e-toolbaricons e-icon e-cancel margin-bottom-5 pointer" ] []
            ]
        ]


type Msg
    = Load (Result Http.Error Model)
    | UpdateDemographics SfData
    | InitDemographicsDone String
    | AddNewLanguage
    | RemoveLanguage Int


patiantLanguageToMessage : DemographicsInformationModel -> PatientLanguagesMap -> Cmd Msg
patiantLanguageToMessage d patientLanguagesMap =
    initLanguagesMap (PatiantLanguageMessage patientLanguagesMap d.sfData.languageDropdown)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateDemo t =
            { model | d = t }

        d =
            model.d
    in
        case msg of
            Load (Ok newModel) ->
                let
                    newPatientLanguagesMap =
                        newModel.d.patientLanguagesMap
                            |> List.indexedMap (\t y -> { y | index = t })

                    newD =
                        newModel.d
                in
                    updateDemo
                        { newD
                            | patientLanguagesMap = newPatientLanguagesMap
                            , patientLanguagesMapCounter = List.length newPatientLanguagesMap
                        }
                        ! [ initDemographics newModel.d.sfData ]

            Load (Err t) ->
                model ! [ logError (toString t) ]

            InitDemographicsDone _ ->
                model ! (initContactHours "" :: List.map (patiantLanguageToMessage model.d) model.d.patientLanguagesMap)

            UpdateDemographics sfData ->
                let
                    d =
                        model.d
                in
                    updateDemo { d | sfData = sfData } ! []

            AddNewLanguage ->
                let
                    newPatientLanguagesMap =
                        emptyPatientLanguagesMap model.d.patientLanguagesMapCounter
                in
                    updateDemo
                        { d
                            | patientLanguagesMap = model.d.patientLanguagesMap ++ [ newPatientLanguagesMap ]
                            , patientLanguagesMapCounter = model.d.patientLanguagesMapCounter + 1
                        }
                        ! [ patiantLanguageToMessage model.d newPatientLanguagesMap ]

            RemoveLanguage index ->
                let
                    newPatientLanguagesMap =
                        model.d.patientLanguagesMap
                            |> List.filter (\t -> t.index /= index)

                    updatedPatientLanguagesMap =
                        case List.any (\t -> t.isPreferred == True) newPatientLanguagesMap of
                            True ->
                                newPatientLanguagesMap

                            False ->
                                List.indexedMap
                                    (\t y ->
                                        if t == 0 then
                                            { y | isPreferred = True }
                                        else
                                            y
                                    )
                                    newPatientLanguagesMap
                in
                    updateDemo { d | patientLanguagesMap = updatedPatientLanguagesMap } ! []


emptyModel : Flags -> DemographicsInformationModel
emptyModel flags =
    { patientId = flags.patientId
    , demographicsId = Nothing
    , nickName = Nothing
    , ssn = Nothing
    , lastName = Nothing
    , firstName = Nothing
    , middle = Nothing
    , birthPlace = Nothing
    , mrn = Nothing
    , patientAccountNumber = Nothing
    , facilityPtID = Nothing
    , sexualOrientationNote = Nothing
    , genderIdentityNote = Nothing
    , email = Nothing
    , preferredLanguageIndex = 0
    , sfData = emptySfData
    , patientLanguagesMap = []
    , patientLanguagesMapCounter = 0
    }


emptySfData : SfData
emptySfData =
    { facilityId = Nothing
    , careCoordinatorId = Nothing
    , mainProviderId = Nothing
    , prefixId = Nothing
    , sexTypeId = Nothing
    , sexualOrientationId = Nothing
    , suffixId = Nothing
    , genderIdentityId = Nothing
    , raceId = Nothing
    , ethnicityId = Nothing
    , uSVeteranId = Nothing
    , religionId = Nothing
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
    , dateOfBirth = Nothing
    , dateOfDeath = Nothing
    , vip = Nothing
    }


emptyPatientLanguagesMap : Int -> PatientLanguagesMap
emptyPatientLanguagesMap index =
    { id = Nothing
    , languageId = -1
    , isPreferred = False
    , index = index
    }



--Decode.field "demographicsInformationModel" decodeModel


init : Flags -> Cmd Msg
init flag =
    decodeModel
        |> Http.get ("/People/GetDemographicsInformation?patientId=" ++ toString flag.patientId)
        |> Http.send Load


decodeModel : Decode.Decoder Model
decodeModel =
    Pipeline.decode Model
        |> Pipeline.required "demographicsInformationModel" decodeDemographicsInformationModel
        |> Pipeline.required "contactInformationModel" decodeContactInformationModel


decodePatientLanguagesMap : Decode.Decoder PatientLanguagesMap
decodePatientLanguagesMap =
    Pipeline.decode PatientLanguagesMap
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "LanguageId" Decode.int
        |> Pipeline.required "IsPreferred" Decode.bool
        |> Pipeline.hardcoded 0


decodeDemographicsInformationModel : Decode.Decoder DemographicsInformationModel
decodeDemographicsInformationModel =
    Pipeline.decode DemographicsInformationModel
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "DemographicsId" (Decode.maybe Decode.int)
        |> Pipeline.required "NickName" (Decode.maybe Decode.string)
        |> Pipeline.required "SSN" (Decode.maybe Decode.string)
        |> Pipeline.required "LastName" (Decode.maybe Decode.string)
        |> Pipeline.required "FirstName" (Decode.maybe Decode.string)
        |> Pipeline.required "Middle" (Decode.maybe Decode.string)
        |> Pipeline.required "BirthPlace" (Decode.maybe Decode.string)
        |> Pipeline.required "MRN" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientAccountNumber" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityPtID" (Decode.maybe Decode.string)
        |> Pipeline.required "SexualOrientationNote" (Decode.maybe Decode.string)
        |> Pipeline.required "GenderIdentityNote" (Decode.maybe Decode.string)
        |> Pipeline.required "Email" (Decode.maybe Decode.string)
        |> Pipeline.required "PreferredLanguageIndex" Decode.int
        |> Pipeline.custom decodeSfData
        |> Pipeline.required "PatientLanguagesMap" (Decode.list decodePatientLanguagesMap)
        |> Pipeline.hardcoded 0


decodeContactInformationModel : Decode.Decoder ContactInformationModel
decodeContactInformationModel =
    Pipeline.decode ContactInformationModel
        |> Pipeline.required "PatientPhoneNumbers" (Decode.list decodePatientPhoneNumber)
        |> Pipeline.required "PatientAddresses" (Decode.list decodePatientAddress)
        |> Pipeline.required "PhoneNumberTypeDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "StateDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "PrimaryAddressIndex" Decode.int
        |> Pipeline.required "PreferredPhoneIndex" Decode.int


decodePatientPhoneNumber : Decode.Decoder PatientPhoneNumber
decodePatientPhoneNumber =
    Pipeline.decode PatientPhoneNumber
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "PhoneNumber" (Decode.maybe Decode.string)
        |> Pipeline.required "PhoneNumberTypeId" (Decode.maybe Decode.int)
        |> Pipeline.required "IsPreferred" Decode.bool


decodePatientAddress : Decode.Decoder PatientAddress
decodePatientAddress =
    Pipeline.decode PatientAddress
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "AddressLine1" (Decode.maybe Decode.string)
        |> Pipeline.required "AddressLine2" (Decode.maybe Decode.string)
        |> Pipeline.required "AddressLine3" (Decode.maybe Decode.string)
        |> Pipeline.required "City" (Decode.maybe Decode.string)
        |> Pipeline.required "StateId" Decode.int
        |> Pipeline.required "ZipCode" (Decode.maybe Decode.string)
        |> Pipeline.required "IsPrimary" Decode.bool


decodeSfData : Decode.Decoder SfData
decodeSfData =
    Pipeline.decode SfData
        |> Pipeline.required "FacilityId" (Decode.maybe Decode.int)
        |> Pipeline.required "MainProviderId" (Decode.maybe Decode.int)
        |> Pipeline.required "CareCoordinatorId" (Decode.maybe Decode.int)
        |> Pipeline.required "PrefixId" (Decode.maybe Decode.int)
        |> Pipeline.required "SexTypeId" (Decode.maybe Decode.int)
        |> Pipeline.required "SexualOrientationId" (Decode.maybe Decode.int)
        |> Pipeline.required "SuffixId" (Decode.maybe Decode.int)
        |> Pipeline.required "GenderIdentityId" (Decode.maybe Decode.int)
        |> Pipeline.required "RaceId" (Decode.maybe Decode.int)
        |> Pipeline.required "EthnicityId" (Decode.maybe Decode.int)
        |> Pipeline.required "USVeteranId" (Decode.maybe Decode.int)
        |> Pipeline.required "ReligionId" (Decode.maybe Decode.int)
        |> Pipeline.required "PatientLanguageDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "CareCoordinatorDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "LanguageDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "EthnicityDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "SexTypeDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "SexualOrientationDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "GenderIdentityDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "FacilityDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "MainProviderDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "RaceDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "SuffixDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "PrefixDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "USVeteranDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "ReligionDropdown" (Decode.list decodeDropDownItem)
        |> Pipeline.required "DateOfBirth" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDeath" (Decode.maybe Decode.string)
        |> Pipeline.required "VIP" (Decode.maybe Decode.bool)

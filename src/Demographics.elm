port module Demographics exposing (..)

import Html exposing (Html, text, div, span, button, ul, li, a, input, label, h4)
import Html.Attributes exposing (class, id, type_, value, style, title, checked)
import Html.Events exposing (onClick)
import Utils.CommonTypes exposing (DropDownItem, Flags)
import Utils.CommonFunctions exposing (decodeDropDownItem)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Http


port initDemographics : SfData -> Cmd msg


port initDemographicsDone : (String -> msg) -> Sub msg


port initLanguagesMap : PatiantLanguageMessage -> Cmd msg


port updateLanguagesMap : (PatiantLanguageMessage -> msg) -> Sub msg


port updateDemographics : (SfData -> msg) -> Sub msg


port logError : String -> Cmd msg


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
    , preferredLanguageIndex : Int
    , sfData : SfData
    , patientLanguagesMap : List PatientLanguagesMap
    , patientLanguagesMapCounter : Int
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


init : Flags -> Cmd Msg
init flag =
    Decode.field "demographicsInformationModel" decodeModel
        |> Http.get ("/People/GetDemographicsInformation?patientId=" ++ toString flag.patientId)
        |> Http.send Load


rowStyle : List (Html.Attribute msg)
rowStyle =
    [ style [ ( "margin-top", "5px" ) ]
    , class "row"
    ]


labelStyle : Bool -> number -> List (Html.Attribute msg)
labelStyle isRequired sizePercent =
    let
        required =
            case isRequired of
                True ->
                    "required "

                False ->
                    ""
    in
        [ class ("padding-right-10 " ++ required ++ " col-md-1")
        , style
            [ ( "font-family", "Segoe UI,Helvetica Neue" )
            , ( "width", toString sizePercent ++ "%" )
            ]
        ]


labelStyleRequiredBig : List (Html.Attribute msg)
labelStyleRequiredBig =
    labelStyle True 11.0


labelStyleOptionalBig : List (Html.Attribute msg)
labelStyleOptionalBig =
    labelStyle False 11.0


labelStyleRequiredSmall : List (Html.Attribute msg)
labelStyleRequiredSmall =
    labelStyle True 11.0


labelStyleOptional : List (Html.Attribute msg)
labelStyleOptional =
    labelStyle False 11.0


divStyle : List (Html.Attribute msg)
divStyle =
    [ class "col-md-2 padding-left-5" ]


maybeValue : Maybe String -> Html.Attribute msg
maybeValue str =
    value (Maybe.withDefault "" str)


view : Model -> Html Msg
view model =
    div []
        [ h4 [] [ text "Assigned To" ]
        , div rowStyle
            [ label labelStyleRequiredBig [ text "Facility:" ]
            , div divStyle [ input [ id "FacilityId" ] [] ]
            , label labelStyleRequiredSmall [ text "Main Provider:" ]
            , div divStyle [ input [ id "MainProviderId" ] [] ]
            ]
        , div rowStyle
            [ label labelStyleOptionalBig [ text "Patient's Facility ID No:" ]
            , div divStyle [ input [ id "FacilityPtIDId", class "e-textbox", maybeValue model.facilityPtID ] [] ]
            , label labelStyleRequiredSmall [ text "Care Coordinator:" ]
            , div divStyle [ input [ id "CareCoordinatorId" ] [] ]
            ]
        , div rowStyle
            [ label labelStyleOptionalBig [ text "Medical Record No:" ]
            , div divStyle [ input [ id "MRNId", class "e-textbox", maybeValue model.mrn ] [] ]
            ]
        , div rowStyle
            [ label labelStyleOptionalBig [ text "Patient Account No:" ]
            , div divStyle [ input [ id "PatientAccountNumberId", class "e-textbox", maybeValue model.patientAccountNumber ] [] ]
            ]
        , div []
            [ h4 [ class "inline-block" ] [ text "Languages" ]
            , div [ class "inline-block e-tooltxt pointer", title "Add new language", onClick AddNewLanguage ]
                [ span [ class "e-addnewitem e-toolbaricons e-icon e-addnew" ] []
                ]
            ]
        , div rowStyle
            [ div [] (List.map viewLanguages model.patientLanguagesMap)
            ]
        ]


xButton : Int -> Html Msg
xButton index =
    let
        xButtonStyle =
            style
                [ ( "width", "20px" )
                ]
    in
        div [ class "inline-block", xButtonStyle, title "remove", onClick (RemoveLanguage index) ]
            [ span [ class "e-cancel e-toolbaricons e-icon e-cancel margin-bottom-5 pointer" ] []
            ]


languageStyle : Html.Attribute msg
languageStyle =
    style
        [ ( "display", "inline-block" )
        , ( "width", "2%" )
        ]


viewLanguages : PatientLanguagesMap -> Html Msg
viewLanguages lang =
    div [ class "row", style [ ( "margin-left", "5px" ), ( "margin-top", "5px" ) ] ]
        [ div [ class "col-md-2 ", languageStyle ] [ input [ type_ "radio", checked lang.isPreferred ] [] ]
        , div [ class "col-md-2" ] [ input [ id ("PatientLanguagesMapId" ++ (toString lang.index)) ] [] ]
        , div [ class "col-md-2", style [ ( "margin-left", "-26px" ), ( "margin-top", "5px" ) ] ] [ xButton lang.index ]
        ]


type Msg
    = Load (Result Http.Error Model)
    | UpdateDemographics SfData
    | InitDemographicsDone String
    | AddNewLanguage
    | RemoveLanguage Int


patiantLanguageToMessage : Model -> PatientLanguagesMap -> Cmd Msg
patiantLanguageToMessage model patientLanguagesMap =
    initLanguagesMap (PatiantLanguageMessage patientLanguagesMap model.sfData.languageDropdown)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok newModel) ->
            let
                newPatientLanguagesMap =
                    newModel.patientLanguagesMap
                        |> List.indexedMap (\t y -> { y | index = t })
            in
                { newModel
                    | patientLanguagesMap = newPatientLanguagesMap
                    , patientLanguagesMapCounter = List.length newPatientLanguagesMap
                }
                    ! [ initDemographics newModel.sfData ]

        Load (Err t) ->
            model ! [ logError (toString t) ]

        InitDemographicsDone _ ->
            model ! (List.map (patiantLanguageToMessage model) model.patientLanguagesMap)

        UpdateDemographics sfData ->
            { model | sfData = sfData } ! []

        AddNewLanguage ->
            let
                newPatientLanguagesMap =
                    emptyPatientLanguagesMap model.patientLanguagesMapCounter
            in
                { model
                    | patientLanguagesMap = model.patientLanguagesMap ++ [ newPatientLanguagesMap ]
                    , patientLanguagesMapCounter = model.patientLanguagesMapCounter + 1
                }
                    ! [ patiantLanguageToMessage model newPatientLanguagesMap ]

        RemoveLanguage index ->
            let
                newPatientLanguagesMap =
                    model.patientLanguagesMap
                        |> List.filter (\t -> t.index /= index)
            in
                { model | patientLanguagesMap = newPatientLanguagesMap } ! []


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
    }


emptyPatientLanguagesMap : Int -> PatientLanguagesMap
emptyPatientLanguagesMap index =
    { id = Nothing
    , languageId = -1
    , isPreferred = False
    , index = index
    }


decodePatientLanguagesMap : Decode.Decoder PatientLanguagesMap
decodePatientLanguagesMap =
    Pipeline.decode PatientLanguagesMap
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "LanguageId" Decode.int
        |> Pipeline.required "IsPreferred" Decode.bool
        |> Pipeline.hardcoded 0


decodeModel : Decode.Decoder Model
decodeModel =
    Pipeline.decode Model
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "DemographicsId" (Decode.maybe Decode.int)
        |> Pipeline.required "NickName" (Decode.maybe Decode.string)
        |> Pipeline.required "VIP" (Decode.maybe Decode.bool)
        |> Pipeline.required "SSN" (Decode.maybe Decode.string)
        |> Pipeline.required "LastName" (Decode.maybe Decode.string)
        |> Pipeline.required "FirstName" (Decode.maybe Decode.string)
        |> Pipeline.required "Middle" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfBirth" (Decode.maybe Decode.string)
        |> Pipeline.required "BirthPlace" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDeath" (Decode.maybe Decode.string)
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

port module Demographics exposing (..)

import Html exposing (Html, text, div, button, ul, li, a, input, label, h4)
import Html.Attributes exposing (class, id, type_, value, style)
import Utils.CommonTypes exposing (DropDownItem, Flags)
import Utils.CommonFunctions exposing (decodeDropDownItem)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Http


port initDemographics : SfData -> Cmd msg


port updateDemographics : (SfData -> msg) -> Sub msg


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
    , patientLanguagesMap : List PatientLanguagesMap
    , preferredLanguageIndex : Int
    , sfData : SfData
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
    { bob : String }


subscriptions : Sub Msg
subscriptions =
    updateDemographics UpdateDemographics


init : Flags -> Cmd Msg
init flag =
    Decode.field "demographicsInformationModel" decodeSfData
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

        -- , h4 [] [ text "Demographic Information" ]
        -- , div rowStyle
        --     [ label labelStyleRequiredBig [ text "aaa" ]
        --     , input [ class "col-md-2", id "aaaa" ] []
        --     , label [ class "col-md-2" ] [ text "aaa" ]
        --     , input [ class "col-md-2", id "aaaa" ] []
        --     ]
        -- , div rowStyle
        --     [ label labelStyleRequiredBig [ text "comments" ]
        --     , input [ class "col-md-2 e-textboxbox" ] []
        --     ]
        ]


type Msg
    = Load (Result Http.Error SfData)
    | UpdateDemographics SfData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok sfData) ->
            { model | sfData = sfData } ! [ initDemographics sfData ]

        Load (Err t) ->
            model ! []

        UpdateDemographics sfData ->
            { model | sfData = sfData } ! []



--  Functions.displayErrorMessage (toString t)


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
    , patientLanguagesMap = []
    , preferredLanguageIndex = 0
    , sfData = emptySfData
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


decodeSfData : Decode.Decoder SfData
decodeSfData =
    Pipeline.decode SfData
        |> Pipeline.required "FacilityId" (Decode.maybe Decode.int)
        |> Pipeline.required "CareCoordinatorId" (Decode.maybe Decode.int)
        |> Pipeline.required "MainProviderId" (Decode.maybe Decode.int)
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

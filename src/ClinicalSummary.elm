port module ClinicalSummary exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Html exposing (Html, text, div, button, h4, input)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Common.Html exposing (InputControlType(HtmlElement, AreaInput), makeControls)
import Common.Types exposing (RequiredType(Optional), monthDropdown, yearDropdown, DropdownItem)
import Common.Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal, postRequest)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)


port updateSyncfusionData : (SyncfusionData -> msg) -> Sub msg


port clinicalSummaryUpdate : (String -> msg) -> Sub msg


port clinicalSummaryInit : SyncfusionData -> Cmd msg


port generateInstructions : SyncfusionData -> Cmd msg


port generateCarePlanLetter : Maybe Int -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ updateSyncfusionData UpdateSyncfusionData
        , clinicalSummaryUpdate UpdateClinicalSummary
        ]


type alias Model =
    { id : Maybe Int
    , facilityId : Maybe Int
    , summary : Maybe String
    , carePlan : Maybe String
    , codeLegalStatus : Maybe String
    , impairment : Maybe String
    , comments : Maybe String
    , syncfusionData : SyncfusionData
    }


init : Int -> Cmd Msg
init patientId =
    let
        getInitData =
            decodeClinicalSummary
                |> Http.get ("/People/ClinicalSummary?patientId=" ++ toString patientId)
                |> Http.send LoadData

        loadDropdowns =
            clinicalSummaryInit (SyncfusionData monthDropdown yearDropdown 0 0)
    in
        Cmd.batch [ getInitData, loadDropdowns ]


type Msg
    = LoadData (Result Http.Error ClinicalSummaryResponseData)
    | UpdateSummary String
    | UpdateCarePlan String
    | UpdateCodeLegalStatus String
    | UpdateImpairment String
    | UpdateComments String
    | UpdateClinicalSummary String
    | UpdateSyncfusionData SyncfusionData
    | GenerateInstructions
    | Save
    | SaveCompleted (Result Http.Error String)
    | GenerateCarePlanLetter


view : Model -> Int -> Html Msg
view model _ =
    div [ class "form-horizontal" ]
        [ h4 [] [ text "Clinical Summary" ]
        , makeControls { controlAttributes = [ class "col-md-8" ] } (formInputs model)
        ]


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        LoadData (Ok newData) ->
            { model
                | id = newData.id
                , comments = newData.comments
                , carePlan = newData.carePlan
                , codeLegalStatus = newData.codeLegalStatus
                , impairment = newData.impairment
                , summary = newData.summary
                , facilityId = newData.facilityId
            }
                ! []

        LoadData (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        UpdateSyncfusionData t ->
            { model | syncfusionData = t } ! []

        UpdateClinicalSummary t ->
            { model | summary = Just t } ! []

        GenerateInstructions ->
            model ! [ generateInstructions model.syncfusionData ]

        GenerateCarePlanLetter ->
            model ! [ generateCarePlanLetter model.facilityId ]

        Save ->
            model
                ! [ "People/UpdateClinicalSummary"
                        |> postRequest (encodeClinicalSummary model patientId)
                        |> Http.send SaveCompleted
                  ]

        SaveCompleted (Ok _) ->
            model ! [ displaySuccessMessage "Clinical Summary Saved Successfully!" ]

        SaveCompleted (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        UpdateSummary str ->
            { model | summary = Just str } ! []

        UpdateCarePlan str ->
            { model | carePlan = Just str } ! []

        UpdateCodeLegalStatus str ->
            { model | codeLegalStatus = Just str } ! []

        UpdateImpairment str ->
            { model | impairment = Just str } ! []

        UpdateComments str ->
            { model | comments = Just str } ! []


generateSummaryDiv : Html Msg
generateSummaryDiv =
    let
        inline widthPercent topPadding =
            [ style
                [ ( "display", "inline-block" )
                , ( "width", widthPercent )
                , ( "padding-top", topPadding )
                ]
            , class "col-md-2 padding-h-0"
            ]
    in
        div []
            [ div (inline "34%" "5px") [ text "Generate Summary from Tasks Outcome:" ]
            , div (inline "18%" "") [ input [ id "MonthId" ] [] ]
            , div (inline "18%" "") [ input [ id "YearId" ] [] ]
            , div (inline "20%" "") [ button [ class "btn btn-sm btn-default", onClick GenerateInstructions ] [ text "Generate" ] ]
            ]


formInputs : Model -> List (InputControlType Msg)
formInputs { summary, carePlan, codeLegalStatus, impairment, comments } =
    [ HtmlElement "" (button [ class "btn btn-sm btn-default", onClick GenerateCarePlanLetter ] [ text "Generate Care Plan Letter" ])
    , AreaInput "Clinical Summary" Optional summary UpdateSummary
    , HtmlElement "" generateSummaryDiv
    , AreaInput "Instructions and Care Plan" Optional carePlan UpdateCarePlan
    , AreaInput "Code/Legal Status" Optional codeLegalStatus UpdateCodeLegalStatus
    , AreaInput "Impairment" Optional impairment UpdateImpairment
    , AreaInput "Comments" Optional comments UpdateComments
    , HtmlElement "" (button [ class "btn btn-sm btn-primary", onClick Save ] [ text "Update" ])
    ]


decodeClinicalSummary : Decode.Decoder ClinicalSummaryResponseData
decodeClinicalSummary =
    decode ClinicalSummaryResponseData
        |> required "Id" (Decode.maybe Decode.int)
        |> required "FacilityId" (Decode.maybe Decode.int)
        |> required "Summary" (Decode.maybe Decode.string)
        |> required "CarePlan" (Decode.maybe Decode.string)
        |> required "CodeLegalStatus" (Decode.maybe Decode.string)
        |> required "Impairment" (Decode.maybe Decode.string)
        |> required "Comments" (Decode.maybe Decode.string)


encodeClinicalSummary : Model -> Int -> Encode.Value
encodeClinicalSummary model patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| model.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Summary", maybeVal Encode.string <| model.summary )
        , ( "CarePlan", maybeVal Encode.string <| model.carePlan )
        , ( "Impairment", maybeVal Encode.string <| model.impairment )
        , ( "CodeLegalStatus", maybeVal Encode.string <| model.codeLegalStatus )
        , ( "Comments", maybeVal Encode.string <| model.comments )
        ]


type alias ClinicalSummaryResponseData =
    { id : Maybe Int
    , facilityId : Maybe Int
    , summary : Maybe String
    , carePlan : Maybe String
    , codeLegalStatus : Maybe String
    , impairment : Maybe String
    , comments : Maybe String
    }


type alias SyncfusionData =
    { months : List DropdownItem
    , years : List DropdownItem
    , currentMonth : Int
    , currentYear : Int
    }


emptyModel : Model
emptyModel =
    { id = Nothing
    , facilityId = Nothing
    , comments = Nothing
    , carePlan = Nothing
    , codeLegalStatus = Nothing
    , impairment = Nothing
    , summary = Nothing
    , syncfusionData = SyncfusionData monthDropdown yearDropdown 0 0
    }

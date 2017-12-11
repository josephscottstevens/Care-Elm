port module ClinicalSummary exposing (..)

import Html exposing (Html, text, div, button, h4, input)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Common.Html exposing (InputControlType(..), makeControls)
import Common.Types exposing (RequiredType(..), monthDropdown, yearDropdown, DropDownItem)
import Common.Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal, postRequest)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, hardcoded)


port clinicalSummaryUpdate : (SyncfusionData -> msg) -> Sub msg


port clinicalSummaryInit : SyncfusionData -> Cmd msg


port generateInstructions : SyncfusionData -> Cmd msg


port generateCarePlanLetter : Maybe Int -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ clinicalSummaryUpdate UpdateClinicalSummary ]


type alias Model =
    { id : Maybe Int
    , facilityId : Maybe Int
    , summary : String
    , carePlan : String
    , codeLegalStatus : String
    , impairment : String
    , comments : String
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
            clinicalSummaryInit (SyncfusionData monthDropdown yearDropdown 0 0 "")
    in
        Cmd.batch [ getInitData, loadDropdowns ]


type Msg
    = LoadData (Result Http.Error ClinicalSummaryResponseData)
    | UpdateSummary String
    | UpdateCarePlan String
    | UpdateCodeLegalStatus String
    | UpdateImpairment String
    | UpdateComments String
    | UpdateClinicalSummary SyncfusionData
    | GenerateInstructions
    | Save
    | SaveCompleted (Result Http.Error String)
    | GenerateCarePlanLetter


view : Model -> Int -> Html Msg
view model patientId =
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

        UpdateClinicalSummary t ->
            { model | syncfusionData = t, summary = t.summaryText } ! []

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
            { model | summary = str } ! []

        UpdateCarePlan str ->
            { model | carePlan = str } ! []

        UpdateCodeLegalStatus str ->
            { model | codeLegalStatus = str } ! []

        UpdateImpairment str ->
            { model | impairment = str } ! []

        UpdateComments str ->
            { model | comments = str } ! []


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
    [ HtmlElement <| button [ class "btn btn-sm btn-default", onClick GenerateCarePlanLetter ] [ text "Generate Care Plan Letter" ]
    , AreaInput "Clinical Summary" Optional summary UpdateSummary
    , HtmlElement generateSummaryDiv
    , AreaInput "Instructions and Care Plan" Optional carePlan UpdateCarePlan
    , AreaInput "Code/Legal Status" Optional codeLegalStatus UpdateCodeLegalStatus
    , AreaInput "Impairment" Optional impairment UpdateImpairment
    , AreaInput "Comments" Optional comments UpdateComments
    , HtmlElement <| button [ class "btn btn-sm btn-primary", onClick Save ] [ text "Update" ]
    ]


decodeClinicalSummary : Decode.Decoder ClinicalSummaryResponseData
decodeClinicalSummary =
    decode ClinicalSummaryResponseData
        |> required "Id" (Decode.maybe Decode.int)
        |> required "FacilityId" (Decode.maybe Decode.int)
        |> required "Summary" Decode.string
        |> required "CarePlan" Decode.string
        |> required "CodeLegalStatus" Decode.string
        |> required "Impairment" Decode.string
        |> required "Comments" Decode.string


encodeClinicalSummary : Model -> Int -> Encode.Value
encodeClinicalSummary model patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| model.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Summary", Encode.string <| model.summary )
        , ( "CarePlan", Encode.string <| model.carePlan )
        , ( "Impairment", Encode.string <| model.impairment )
        , ( "CodeLegalStatus", Encode.string <| model.codeLegalStatus )
        , ( "Comments", Encode.string <| model.comments )
        ]


type alias ClinicalSummaryResponseData =
    { id : Maybe Int
    , facilityId : Maybe Int
    , summary : String
    , carePlan : String
    , codeLegalStatus : String
    , impairment : String
    , comments : String
    }


type alias SyncfusionData =
    { months : List DropDownItem
    , years : List DropDownItem
    , currentMonth : Int
    , currentYear : Int
    , summaryText : String
    }


emptyModel : Model
emptyModel =
    { id = Nothing
    , facilityId = Nothing
    , comments = ""
    , carePlan = ""
    , codeLegalStatus = ""
    , impairment = ""
    , summary = ""
    , syncfusionData = SyncfusionData monthDropdown yearDropdown 0 0 ""
    }

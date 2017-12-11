module ClinicalSummary exposing (..)

import Html exposing (Html, text, div, button, h4, input)
import Html.Attributes exposing (class, id, style)
import Common.Html exposing (InputControlType(..), makeControls)
import Common.Types exposing (RequiredType(..), ClinicalSummaryInitData, monthDropdown, yearDropdown)
import Common.Functions exposing (displayErrorMessage)
import Http
import Json.Decode as Decode exposing (maybe)
import Json.Decode.Pipeline exposing (decode, required)
import Ports exposing (clinicalSummaryInit)


type alias Model =
    { id : Maybe Int
    , summary : String
    , carePlan : String
    , codeLegalStatus : String
    , impairment : String
    , comments : String
    , rnReviewedCarePlan : Bool
    }


init : Int -> Cmd Msg
init patientId =
    let
        getInitData =
            decodeClinicalSummary
                |> Http.get ("/People/ClinicalSummary?patientId=" ++ toString patientId)
                |> Http.send LoadData

        loadDropdowns =
            clinicalSummaryInit (ClinicalSummaryInitData monthDropdown yearDropdown)
    in
        Cmd.batch [ getInitData, loadDropdowns ]


type Msg
    = LoadData (Result Http.Error Model)
    | UpdateSummary String
    | UpdateCarePlan String
    | UpdateCodeLegalStatus String
    | UpdateImpairment String
    | UpdateComments String


view : Model -> Int -> Html Msg
view model patientId =
    div [ class "form-horizontal" ]
        [ h4 [] [ text "Clinical Summary" ]
        , makeControls { controlAttributes = [ class "col-md-8" ] } (formInputs model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadData (Ok newModel) ->
            newModel ! []

        LoadData (Err t) ->
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


generateSummaryDiv : Html msg
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
            , div (inline "20%" "") [ button [ class "btn btn-sm btn-default" ] [ text "Generate" ] ]
            ]


formInputs : Model -> List (InputControlType Msg)
formInputs { summary, carePlan, codeLegalStatus, impairment, comments } =
    [ HtmlElement <| button [ class "btn btn-sm btn-default" ] [ text "Generate Care Plan Letter" ]
    , AreaInput "Clinical Summary" Optional summary UpdateSummary
    , HtmlElement generateSummaryDiv
    , AreaInput "Instructions and Care Plan" Optional carePlan UpdateCarePlan
    , AreaInput "Code/Legal Status" Optional codeLegalStatus UpdateCodeLegalStatus
    , AreaInput "Impairment" Optional impairment UpdateImpairment
    , AreaInput "Comments" Optional comments UpdateComments
    , HtmlElement <| button [ class "btn btn-sm btn-primary" ] [ text "Update" ]
    ]


decodeClinicalSummary : Decode.Decoder Model
decodeClinicalSummary =
    decode Model
        |> required "Id" (maybe Decode.int)
        |> required "Summary" Decode.string
        |> required "CarePlan" Decode.string
        |> required "CodeLegalStatus" Decode.string
        |> required "Impairment" Decode.string
        |> required "Comments" Decode.string
        |> required "RnReviewedCarePlan" Decode.bool


emptyModel : Model
emptyModel =
    { id = Nothing
    , comments = ""
    , carePlan = ""
    , codeLegalStatus = ""
    , impairment = ""
    , summary = ""
    , rnReviewedCarePlan = False
    }

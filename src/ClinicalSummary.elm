module ClinicalSummary exposing (..)

import Html exposing (Html, text, div, button, h4, input)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (RequiredType(..))
import Task exposing (Task)
import Http
import Json.Decode as Decode exposing (maybe)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { id : Maybe Int
    , summary : String
    , carePlan : String
    , codeLegalStatus : String
    , impairment : String
    , comments : String
    , rnReviewedCarePlan : Bool
    }


init : Int -> Task Http.Error Model
init patientId =
    decodeClinicalSummary
        |> Http.get ("/People/ClinicalSummary?patientId=" ++ toString patientId)
        |> Http.toTask


type Msg
    = UpdateSummary String
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
        inline =
            style [ ( "display", "inline-block" ) ]
    in
        div []
            [ div [ inline ] [ text "Generate Summary" ]
            , div [ inline ] [ input [ id "MonthId" ] [] ]
            , div [ inline ] [ input [ id "YearId" ] [] ]
            , button [] [ text "sub" ]
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

module ClinicalSummary exposing (..)

import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (RequiredType(..))
import Task exposing (Task)
import Http
import Json.Decode as Decode exposing (maybe)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { id : Maybe Int
    , comments : String
    , carePlan : String
    , codeLegalStatus : String
    , diagnosis : String
    , impairment : String
    , summary : String
    , rnReviewedCarePlan : Bool
    , patientId : Int
    }


emptyModel : Int -> Model
emptyModel patientId =
    { id = Nothing
    , comments = ""
    , carePlan = ""
    , codeLegalStatus = ""
    , diagnosis = ""
    , impairment = ""
    , summary = ""
    , rnReviewedCarePlan = False
    , patientId = patientId
    }


init : Int -> Task Http.Error Model
init patientId =
    decodeClinicalSummary
        |> Http.get ("/People/ClinicalSummaryViewData?patientId=" ++ toString patientId)
        |> Http.toTask


type Msg
    = UpdateComments String


view : Model -> Html Msg
view model =
    div [ class "form-horizontal" ]
        [ h4 [] [ text "Clinical Summary" ]
        , makeControls { labelAttributes = [ class "col-md-8" ] } (formInputs model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateComments str ->
            { model | comments = str } ! []


formInputs : Model -> List (InputControlType Msg)
formInputs model =
    [ AreaInput "Facility" Optional model.comments UpdateComments
    ]


decodeClinicalSummary : Decode.Decoder Model
decodeClinicalSummary =
    decode Model
        |> required "ID" (maybe Decode.int)
        |> required "Comments" Decode.string
        |> required "CarePlan" Decode.string
        |> required "CodeLegalStatus" Decode.string
        |> required "Diagnosis" Decode.string
        |> required "Impairment" Decode.string
        |> required "Summary" Decode.string
        |> required "RnReviewedCarePlan" Decode.bool
        |> required "PatientId" Decode.int

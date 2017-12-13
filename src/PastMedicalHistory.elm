port module PastMedicalHistory exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Html exposing (Html, text, div, button, h4, input)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Common.Html exposing (InputControlType(HtmlElement, AreaInput), makeControls)
import Common.Types exposing (RequiredType(Optional), monthDropdown, yearDropdown, DropDownItem)
import Common.Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal, postRequest)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)


-- port pastMedicalHistoryUpdate : (SyncfusionData -> msg) -> Sub msg
-- port pastMedicalHistoryInit : SyncfusionData -> Cmd msg
-- port generateInstructions : SyncfusionData -> Cmd msg
-- port generateCarePlanLetter : Maybe Int -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.none



--Sub.batch [ pastMedicalHistoryUpdate PastMedicalHistoryUpdate ]


type alias Model =
    { rows : List PastMedicalHistoryRow
    }


init : Int -> Cmd Msg
init patientId =
    let
        getInitData =
            Decode.list decodePastMedicalHistoryRow
                |> Http.get ("/People/ClinicalSummary?patientId=" ++ toString patientId)
                |> Http.send LoadData

        -- loadDropdowns =
        --     pastMedicalHistoryInit (SyncfusionData monthDropdown yearDropdown 0 0 "")
    in
        Cmd.batch [ getInitData ]



-- loadDropdowns


type Msg
    = LoadData (Result Http.Error (List PastMedicalHistoryRow))
    | Save
    | SaveCompleted (Result Http.Error String)


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        LoadData (Ok newData) ->
            { model | rows = newData } ! []

        LoadData (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        Save ->
            model
                ! []

        -- [ "People/UpdateClinicalSummary"
        --         |> postRequest (encodeClinicalSummary model patientId)
        --         |> Http.send SaveCompleted
        --   ]
        SaveCompleted (Ok _) ->
            model ! [ displaySuccessMessage "Clinical Summary Saved Successfully!" ]

        SaveCompleted (Err t) ->
            model ! [ displayErrorMessage (toString t) ]


view : Model -> Int -> Html Msg
view model _ =
    div [ class "form-horizontal" ]
        [ h4 [] [ text "Clinical Summary" ]

        --, makeControls { controlAttributes = [ class "col-md-8" ] } (formInputs model)
        ]



-- formInputs : Model -> List (InputControlType Msg)
-- formInputs { summary, carePlan, codeLegalStatus, impairment, comments } =
--     [ HtmlElement <| button [ class "btn btn-sm btn-default", onClick GenerateCarePlanLetter ] [ text "Generate Care Plan Letter" ]
--     , AreaInput "Clinical Summary" Optional summary UpdateSummary
--     , HtmlElement generateSummaryDiv
--     , AreaInput "Instructions and Care Plan" Optional carePlan UpdateCarePlan
--     , AreaInput "Code/Legal Status" Optional codeLegalStatus UpdateCodeLegalStatus
--     , AreaInput "Impairment" Optional impairment UpdateImpairment
--     , AreaInput "Comments" Optional comments UpdateComments
--     , HtmlElement <| button [ class "btn btn-sm btn-primary", onClick Save ] [ text "Update" ]
--     ]


decodePastMedicalHistoryRow : Decode.Decoder PastMedicalHistoryRow
decodePastMedicalHistoryRow =
    decode PastMedicalHistoryRow
        |> required "Id" Decode.int
        |> required "Description" Decode.string
        |> required "Year" Decode.string
        |> required "Treatment" Decode.string
        |> required "Facility" Decode.string
        |> required "Provider" Decode.string
        |> required "Notes" Decode.string
        |> required "ProviderId" (Decode.maybe Decode.int)
        |> required "ProblemId" (Decode.maybe Decode.int)
        |> hardcoded False



-- encodeClinicalSummary : Model -> Int -> Encode.Value
-- encodeClinicalSummary model patientId =
--     Encode.object
--         [ ( "Id", maybeVal Encode.int <| model.id )
--         , ( "PatientId", Encode.int <| patientId )
--         , ( "Summary", Encode.string <| model.summary )
--         , ( "CarePlan", Encode.string <| model.carePlan )
--         , ( "Impairment", Encode.string <| model.impairment )
--         , ( "CodeLegalStatus", Encode.string <| model.codeLegalStatus )
--         , ( "Comments", Encode.string <| model.comments )
--         ]


type alias PastMedicalHistoryRow =
    { id : Int
    , description : String
    , year : String
    , treatment : String
    , facility : String
    , provider : String
    , notes : String
    , providerId : Maybe Int
    , problemId : Maybe Int
    , dropdownOpen : Bool
    }


emptyModel : Model
emptyModel =
    { rows = []
    }

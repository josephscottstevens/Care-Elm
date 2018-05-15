module ClinicalSummary exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Dates as Functions
import Common.Dropdown as Dropdown
import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal)
import Common.Html exposing (InputControlType(..), makeControls)
import Common.Types exposing (RequiredType(..), monthDropdown, yearDropdown)
import Html exposing (Html, button, div, h4, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Task
import Time


subscriptions : Sub Msg
subscriptions =
    Sub.none


type alias Model =
    { id : Maybe Int
    , facilityId : Maybe Int
    , summary : Maybe String
    , carePlan : Maybe String
    , codeLegalStatus : Maybe String
    , impairment : Maybe String
    , comments : Maybe String
    , currentMonth : Maybe Int
    , currentYear : Maybe Int
    , monthDropState : Dropdown.DropState
    , yearDropState : Dropdown.DropState
    }


init : Int -> Cmd Msg
init patientId =
    decodeClinicalSummary
        |> Http.get ("/People/ClinicalSummary?patientId=" ++ String.fromInt patientId)
        |> Http.send LoadData


type Msg
    = LoadData (Result Http.Error ClinicalSummaryResponseData)
    | GetDate Time.Posix
    | UpdateSummary String
    | UpdateCarePlan String
    | UpdateCodeLegalStatus String
    | UpdateImpairment String
    | UpdateComments String
    | UpdateClinicalSummary String
    | Save
    | SaveCompleted (Result Http.Error String)
    | GenerateCarePlanLetter
    | GenerateCarePlanLetterCompleted (Result Http.Error String)
    | UpdateMonth ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateYear ( Dropdown.DropState, Maybe Int, Cmd Msg )


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
            ( { model
                | id = newData.id
                , comments = newData.comments
                , carePlan = newData.carePlan
                , codeLegalStatus = newData.codeLegalStatus
                , impairment = newData.impairment
                , summary = newData.summary
                , facilityId = newData.facilityId
              }
            , Task.perform GetDate Time.now
            )

        LoadData (Err t) ->
            ( model
            , Functions.displayError t
            )

        GetDate dt ->
            ( { model
                | currentMonth = Just (Functions.monthIndex dt)
                , currentYear = Just (Time.toYear Time.utc dt)
              }
            , Cmd.none
            )

        UpdateClinicalSummary t ->
            ( { model | summary = Just t }
            , Cmd.none
            )

        GenerateCarePlanLetter ->
            ( model
            , Functions.getStringRequestWithParams
                "/People/GetCarePlanFromTasks"
                [ ( "patientId", String.fromInt patientId )
                , ( "year", model.currentYear |> Maybe.withDefault 0 |> String.fromInt )
                , ( "month", model.currentMonth |> Maybe.map (\t -> t + 1) |> Maybe.withDefault 0 |> String.fromInt )
                ]
                |> Http.send GenerateCarePlanLetterCompleted
            )

        GenerateCarePlanLetterCompleted (Ok response) ->
            ( { model | summary = Functions.getResponseProp response "carePlan" }
            , Cmd.none
            )

        GenerateCarePlanLetterCompleted (Err t) ->
            ( model
            , displayErrorMessage (Functions.getError t)
            )

        Save ->
            ( model
            , Functions.postStringRequestWithObject
                "/People/UpdateClinicalSummary"
                [ ( "Id", maybeVal Encode.int <| model.id )
                , ( "PatientId", Encode.int <| patientId )
                , ( "Summary", maybeVal Encode.string <| model.summary )
                , ( "CarePlan", maybeVal Encode.string <| model.carePlan )
                , ( "Impairment", maybeVal Encode.string <| model.impairment )
                , ( "CodeLegalStatus", maybeVal Encode.string <| model.codeLegalStatus )
                , ( "Comments", maybeVal Encode.string <| model.comments )
                ]
                |> Http.send SaveCompleted
            )

        UpdateMonth ( newDropState, newId, newMsg ) ->
            ( { model | monthDropState = newDropState, currentMonth = newId }
            , newMsg
            )

        UpdateYear ( newDropState, newId, newMsg ) ->
            ( { model | yearDropState = newDropState, currentYear = newId }
            , newMsg
            )

        SaveCompleted (Ok _) ->
            ( model
            , displaySuccessMessage "Clinical Summary Saved Successfully!"
            )

        SaveCompleted (Err t) ->
            ( model
            , displayErrorMessage (Functions.getError t)
            )

        UpdateSummary str ->
            ( { model | summary = Just str }
            , Cmd.none
            )

        UpdateCarePlan str ->
            ( { model | carePlan = Just str }
            , Cmd.none
            )

        UpdateCodeLegalStatus str ->
            ( { model | codeLegalStatus = Just str }
            , Cmd.none
            )

        UpdateImpairment str ->
            ( { model | impairment = Just str }
            , Cmd.none
            )

        UpdateComments str ->
            ( { model | comments = Just str }
            , Cmd.none
            )


formInputs : Model -> List (InputControlType Msg)
formInputs model =
    [ AreaInput "Clinical Summary" Optional model.summary UpdateSummary
    , ControlElement "" (generateSummaryDiv model)
    , AreaInput "Instructions and Care Plan" Optional model.carePlan UpdateCarePlan
    , AreaInput "Code/Legal Status" Optional model.codeLegalStatus UpdateCodeLegalStatus
    , AreaInput "Impairment" Optional model.impairment UpdateImpairment
    , AreaInput "Comments" Optional model.comments UpdateComments
    , ControlElement "" (button [ class "btn btn-sm btn-primary", onClick Save ] [ text "Update" ])
    ]


generateSummaryDiv : Model -> Html Msg
generateSummaryDiv model =
    let
        inline widthPercent topPadding =
            [ style "display" "inline-block"
            , style "width" widthPercent
            , style "padding-top" topPadding
            , class "col-md-2 padding-h-0"
            ]
    in
    div []
        [ div (inline "34%" "5px") [ text "Generate Summary from Tasks Outcome:" ]
        , div (inline "18%" "")
            [ Dropdown.view model.monthDropState UpdateMonth monthDropdown model.currentMonth ]
        , div (inline "18%" "")
            [ Dropdown.view model.yearDropState UpdateYear yearDropdown model.currentYear ]
        , div (inline "20%" "") [ button [ class "btn btn-sm btn-default", onClick GenerateCarePlanLetter ] [ text "Generate" ] ]
        ]


decodeClinicalSummary : Decode.Decoder ClinicalSummaryResponseData
decodeClinicalSummary =
    Decode.succeed ClinicalSummaryResponseData
        |> required "Id" (Decode.maybe Decode.int)
        |> required "FacilityId" (Decode.maybe Decode.int)
        |> required "Summary" (Decode.maybe Decode.string)
        |> required "CarePlan" (Decode.maybe Decode.string)
        |> required "CodeLegalStatus" (Decode.maybe Decode.string)
        |> required "Impairment" (Decode.maybe Decode.string)
        |> required "Comments" (Decode.maybe Decode.string)


type alias ClinicalSummaryResponseData =
    { id : Maybe Int
    , facilityId : Maybe Int
    , summary : Maybe String
    , carePlan : Maybe String
    , codeLegalStatus : Maybe String
    , impairment : Maybe String
    , comments : Maybe String
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
    , currentMonth = Nothing
    , currentYear = Nothing
    , monthDropState = Dropdown.init "monthDropdown" False
    , yearDropState = Dropdown.init "yearDropdown" False
    }

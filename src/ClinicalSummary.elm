module ClinicalSummary exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Date exposing (Date)
import Task
import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Common.Dropdown as Dropdown
import Common.Html exposing (InputControlType(ControlElement, AreaInput), makeControls)
import Common.Types exposing (RequiredType(Optional), monthDropdown, yearDropdown)
import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)


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
        |> Http.get ("/People/ClinicalSummary?patientId=" ++ toString patientId)
        |> Http.send LoadData


type Msg
    = LoadData (Result Http.Error ClinicalSummaryResponseData)
    | GetDate Date
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
            { model
                | id = newData.id
                , comments = newData.comments
                , carePlan = newData.carePlan
                , codeLegalStatus = newData.codeLegalStatus
                , impairment = newData.impairment
                , summary = newData.summary
                , facilityId = newData.facilityId
            }
                ! [ Task.perform GetDate Date.now ]

        LoadData (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        GetDate dt ->
            { model
                | currentMonth = Just (dt |> Functions.getMonthIndex)
                , currentYear = Just (Date.year dt)
            }
                ! []

        UpdateClinicalSummary t ->
            { model | summary = Just t } ! []

        GenerateCarePlanLetter ->
            model
                ! [ Functions.getStringRequestWithParams
                        "/People/GetCarePlanFromTasks"
                        [ ( "patientId", toString patientId )
                        , ( "year", model.currentYear |> Maybe.withDefault 0 |> toString )
                        , ( "month", model.currentMonth |> Maybe.map (\t -> t + 1) |> Maybe.withDefault 0 |> toString )
                        ]
                        |> Http.send GenerateCarePlanLetterCompleted
                  ]

        GenerateCarePlanLetterCompleted (Ok newData) ->
            { model
                | summary =
                    newData
                        |> Decode.decodeString (Decode.at [ "AdditionalData", "carePlan" ] Decode.string)
                        |> Result.toMaybe
            }
                ! []

        GenerateCarePlanLetterCompleted (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        Save ->
            model
                ! [ Functions.postStringRequestWithObject
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
                  ]

        UpdateMonth ( newDropState, newId, newMsg ) ->
            { model | monthDropState = newDropState, currentMonth = newId } ! [ newMsg ]

        UpdateYear ( newDropState, newId, newMsg ) ->
            { model | yearDropState = newDropState, currentYear = newId } ! [ newMsg ]

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
            , div (inline "18%" "")
                [ Dropdown.view model.monthDropState UpdateMonth monthDropdown model.currentMonth ]
            , div (inline "18%" "")
                [ Dropdown.view model.yearDropState UpdateYear yearDropdown model.currentYear ]
            , div (inline "20%" "") [ button [ class "btn btn-sm btn-default", onClick GenerateCarePlanLetter ] [ text "Generate" ] ]
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

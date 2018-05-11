port module PastMedicalHistory exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal, sendMenuMessage, setUnsavedChanges)
import Common.Html exposing (InputControlType(AreaInput, ControlElement, DropInput, TextInput), defaultConfig, fullWidth, getValidationErrors, makeControls)
import Common.Table as Table
import Common.Types exposing (AddEditDataSource, DropdownItem, MenuMessage, RequiredType(Optional, Required))
import Html exposing (Html, button, div, h4, input, text)
import Html.Attributes exposing (class, disabled, style, type_, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode


port initPastMedicalHistory : SfData -> Cmd msg


port updatePastMedicalHistory : (SfData -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.dialogConfirmed DeletePastMedicalHistoryConfirmed
        , updatePastMedicalHistory UpdatePastMedicalHistory
        ]


type State
    = Grid
    | AddEdit NewRecord


type alias Model =
    { rows : List Row
    , tableState : Table.State
    , state : State
    , showValidationErrors : Bool
    }


type alias SfData =
    { providerId : Maybe Int
    , providerDropdown : List DropdownItem
    }


type alias Row =
    { id : Int
    , description : Maybe String
    , year : Maybe String
    , treatment : Maybe String
    , facility : Maybe String
    , provider : Maybe String
    , notes : Maybe String
    , providerId : Maybe Int
    , problemId : Maybe Int
    }


type alias NewRecord =
    { id : Maybe Int
    , description : Maybe String
    , year : Maybe String
    , facility : Maybe String
    , notes : Maybe String
    , treatment : Maybe String
    , problemId : Maybe Int
    , addEditDataSource : AddEditDataSource
    , sfData : SfData
    }


init : Int -> Cmd Msg
init patientId =
    load patientId


type Msg
    = LoadData (Result Http.Error (List Row))
    | Cancel
    | Save NewRecord
    | SaveCompleted (Result Http.Error String)
    | Add AddEditDataSource
    | Edit AddEditDataSource Row
    | SetTableState Table.State
    | DeletePrompt Row
    | DeletePastMedicalHistoryConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | SendMenuMessage Int
      -- Updates
    | NoOp
    | UpdateDescription NewRecord String
    | UpdateYear NewRecord String
    | UpdateFacility NewRecord String
    | UpdateNotes NewRecord String
    | UpdatePastMedicalHistory SfData


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        LoadData (Ok newData) ->
            ( { model | rows = newData }
            , Cmd.none
            )

        LoadData (Err t) ->
            ( model
            , displayErrorMessage (toString t)
            )

        Cancel ->
            ( { model | state = Grid }
            , Cmd.none
            )

        Save row ->
            if List.length (getValidationErrors (formInputs row)) > 0 then
                ( { model | showValidationErrors = True }
                , Cmd.none
                )

            else
                ( model
                , Cmd.batch
                    [ "/People/AddUpdatePastMedicalHistories"
                        |> Functions.postRequest (encodeNewRow row patientId)
                        |> Http.send SaveCompleted
                    , setUnsavedChanges False
                    ]
                )

        SaveCompleted (Ok _) ->
            ( { model | state = Grid }
            , Cmd.batch [ displaySuccessMessage "Past Medical History Saved Successfully!", load patientId ]
            )

        SaveCompleted (Err t) ->
            ( { model | state = Grid }
            , Cmd.batch [ displayErrorMessage (toString t), load patientId ]
            )

        Add addEditDataSource ->
            ( { model | state = AddEdit (newRecord addEditDataSource Nothing) }
            , initPastMedicalHistory <| SfData Nothing addEditDataSource.providers
            )

        Edit addEditDataSource row ->
            ( { model | state = AddEdit (newRecord addEditDataSource (Just row)) }
            , initPastMedicalHistory <| SfData row.providerId addEditDataSource.providers
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        DeletePrompt row ->
            ( model
            , Functions.deleteDialogShow row.id
            )

        DeletePastMedicalHistoryConfirmed rowId ->
            ( { model | rows = model.rows |> List.filter (\t -> t.id /= rowId) }
            , Http.send DeleteCompleted <|
                Http.getString ("/People/DeletePastMedicalHistory?id=" ++ toString rowId)
            )

        DeleteCompleted (Ok responseMsg) ->
            case Functions.getResponseError responseMsg of
                Just t ->
                    ( model
                    , Cmd.batch [ Functions.displayErrorMessage t, load patientId ]
                    )

                Nothing ->
                    ( model
                    , Functions.displaySuccessMessage "Record deleted successfully!"
                    )

        DeleteCompleted (Err t) ->
            ( model
            , Functions.displayErrorMessage (toString t)
            )

        SendMenuMessage recordId ->
            ( model
            , sendMenuMessage (MenuMessage "PastMedicalHistoryDelete" recordId Nothing Nothing)
            )

        NoOp ->
            ( model
            , Cmd.none
            )

        -- Updates
        UpdateDescription newRecord str ->
            ( { model | state = AddEdit { newRecord | description = Just str } }
            , Cmd.none
            )

        UpdateYear newRecord str ->
            ( { model | state = AddEdit { newRecord | year = Just str } }
            , Cmd.none
            )

        UpdateFacility newRecord str ->
            ( { model | state = AddEdit { newRecord | facility = Just str } }
            , Cmd.none
            )

        UpdateNotes newRecord str ->
            ( { model | state = AddEdit { newRecord | notes = Just str } }
            , Cmd.none
            )

        UpdatePastMedicalHistory sfData ->
            case model.state of
                Grid ->
                    ( model
                    , Cmd.none
                    )

                AddEdit newRecord ->
                    ( { model | state = AddEdit { newRecord | sfData = sfData } }
                    , Cmd.none
                    )


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    case model.state of
        Grid ->
            div []
                [ h4 [] [ text "Past Medical History" ]
                , Table.view model.tableState model.rows (gridConfig addEditDataSource) Nothing
                ]

        AddEdit newRecord ->
            let
                errors =
                    getValidationErrors (formInputs newRecord)

                validationErrorsDiv =
                    if model.showValidationErrors == True && List.length errors > 0 then
                        div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)

                    else
                        div [] []
            in
            div [ class "form-horizontal" ]
                [ validationErrorsDiv
                , h4 [] [ text "Past Medical History" ]
                , makeControls defaultConfig (formInputs newRecord)
                , div [ class "form-group" ]
                    [ div [ class fullWidth ]
                        [ button [ type_ "button", onClick (Save newRecord), class "btn btn-sm btn-success" ] [ text "Save" ]
                        , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default margin-left-5" ] [ text "Cancel" ]
                        ]
                    ]
                ]


noteStyle : Html.Attribute msg
noteStyle =
    style [ ( "color", "#969696" ), ( "font-size", "12px" ) ]


formInputs : NewRecord -> List (InputControlType Msg)
formInputs newRecord =
    [ ControlElement "" (div [ noteStyle ] [ text "*Records added from the problem list cannot be edited." ])
    , AreaInput "Description" Required newRecord.description (UpdateDescription newRecord)
    , TextInput "Year" Optional newRecord.year (UpdateYear newRecord)
    , DropInput "Provider" Optional newRecord.sfData.providerId "MainProviderId"
    , TextInput "Facility" Optional newRecord.facility (UpdateFacility newRecord)
    , TextInput "Notes" Optional newRecord.notes (UpdateNotes newRecord)
    , ControlElement "Treatment"
        (input
            [ type_ "textbox"
            , class "e-textbox"
            , disabled True
            , value (Maybe.withDefault "" newRecord.treatment)
            ]
            []
        )
    , ControlElement "" (div [ noteStyle ] [ text "*Treatment is deprecated." ])
    ]


gridConfig : Maybe AddEditDataSource -> Table.Config Row Msg
gridConfig addEditDataSource =
    { domTableId = "PastMedicalHistoryTable"
    , toolbar =
        case addEditDataSource of
            Just t ->
                [ ( "e-addnew e-loaded", Add t ) ]

            Nothing ->
                [ ( "e-addnew e-disable", NoOp ) ]
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Description" .description
        , Table.stringColumn "Year" .year
        , Table.stringColumn "Facility" .facility
        , Table.stringColumn "Provider" .provider
        , Table.stringColumn "Notes" .notes
        , Table.dropdownColumn <|
            case addEditDataSource of
                Just t ->
                    [ ( "e-edit", "Edit", Edit t )
                    , ( "e-contextdelete", "Delete", DeletePrompt )
                    ]

                Nothing ->
                    []
        ]
    }


decodeRow : Decode.Decoder Row
decodeRow =
    decode Row
        |> required "Id" Decode.int
        |> required "Description" (Decode.maybe Decode.string)
        |> required "Year" (Decode.maybe Decode.string)
        |> required "Treatment" (Decode.maybe Decode.string)
        |> required "Facility" (Decode.maybe Decode.string)
        |> required "Provider" (Decode.maybe Decode.string)
        |> required "Notes" (Decode.maybe Decode.string)
        |> required "ProviderId" (Decode.maybe Decode.int)
        |> required "ProblemId" (Decode.maybe Decode.int)


encodeNewRow : NewRecord -> Int -> Encode.Value
encodeNewRow newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Description", maybeVal Encode.string <| newRecord.description )
        , ( "Year", maybeVal Encode.string <| newRecord.year )
        , ( "Treatment", maybeVal Encode.string <| newRecord.treatment )
        , ( "Facility", maybeVal Encode.string <| newRecord.facility )
        , ( "Notes", maybeVal Encode.string <| newRecord.notes )
        , ( "ProviderId", maybeVal Encode.int <| newRecord.sfData.providerId )
        , ( "ProblemId", maybeVal Encode.int <| newRecord.problemId )
        ]


newRecord : AddEditDataSource -> Maybe Row -> NewRecord
newRecord addEditDataSource row =
    case row of
        Just row ->
            { id = Just row.id
            , description = row.description
            , year = row.year
            , facility = row.facility
            , notes = row.notes
            , treatment = row.treatment
            , problemId = row.problemId
            , sfData = SfData row.providerId addEditDataSource.providers
            , addEditDataSource = addEditDataSource
            }

        Nothing ->
            { id = Nothing
            , description = Nothing
            , year = Nothing
            , facility = Nothing
            , notes = Nothing
            , treatment = Nothing
            , problemId = Nothing
            , sfData = SfData Nothing addEditDataSource.providers
            , addEditDataSource = addEditDataSource
            }


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.init ""
    , state = Grid
    , showValidationErrors = False
    }


load : Int -> Cmd Msg
load patientId =
    Decode.list decodeRow
        |> Http.get ("/People/PastMedicalHistoriesGrid?patientId=" ++ toString patientId)
        |> Http.send LoadData

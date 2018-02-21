module PastMedicalHistory exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Html exposing (Html, text, div, button, input, h4)
import Html.Attributes exposing (class, style, type_, disabled, value)
import Html.Events exposing (onClick)
import Common.Html exposing (InputControlType(TextInput, AreaInput, Dropdown, HtmlElement), makeControls, defaultConfig, getValidationErrors, fullWidth)
import Common.Types exposing (RequiredType(Optional, Required), AddEditDataSource, MenuMessage, DropdownItem)
import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal, sendMenuMessage, setUnsavedChanges)
import Common.Dropdown as Dropdown
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)
import Common.Table as Table exposing (stringColumn, dateColumn, intColumn, dateTimeColumn, dropdownColumn, hrefColumn, hrefColumnExtra, checkColumn)
import Http


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.deleteConfirmed DeletePastMedicalHistoryConfirmed
        ]


type State
    = Grid
    | AddEdit NewRecord


type alias Model =
    { rows : List PastMedicalHistoryRow
    , tableState : Table.State
    , state : State
    , showValidationErrors : Bool
    }


init : Int -> Cmd Msg
init patientId =
    load patientId


type Msg
    = LoadData (Result Http.Error (List PastMedicalHistoryRow))
    | Cancel
    | Save NewRecord
    | SaveCompleted (Result Http.Error String)
    | Add AddEditDataSource
    | Edit AddEditDataSource (Maybe Int)
    | SetTableState Table.State
    | DeletePrompt Int
    | DeletePastMedicalHistoryConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | SendMenuMessage Int
      -- Updates
    | NoOp
    | UpdateDescription NewRecord (Maybe String)
    | UpdateYear NewRecord (Maybe String)
    | UpdateFacility NewRecord (Maybe String)
    | UpdateProvider NewRecord Dropdown.Msg
    | UpdateNotes NewRecord (Maybe String)


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        LoadData (Ok newData) ->
            { model | rows = newData } ! []

        LoadData (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        Cancel ->
            { model | state = Grid } ! []

        Save row ->
            if List.length (getValidationErrors (formInputs row)) > 0 then
                { model | showValidationErrors = True } ! []
            else
                model
                    ! [ "People/AddUpdatePastMedicalHistories"
                            |> Functions.postRequest (encodeNewRow row patientId)
                            |> Http.send SaveCompleted
                      , setUnsavedChanges False
                      ]

        SaveCompleted (Ok _) ->
            { model | state = Grid } ! [ displaySuccessMessage "Past Medical History Saved Successfully!", load patientId ]

        SaveCompleted (Err t) ->
            { model | state = Grid } ! [ displayErrorMessage (toString t), load patientId ]

        Add addEditDataSource ->
            { model | state = AddEdit (newRecord addEditDataSource Nothing) } ! []

        Edit addEditDataSource rowId ->
            let
                row =
                    model.rows |> List.filter (\t -> t.id == rowId) |> List.head
            in
                { model | state = AddEdit (newRecord addEditDataSource row) } ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        DeletePrompt rowId ->
            model ! [ Functions.deletePrompt rowId ]

        DeletePastMedicalHistoryConfirmed rowId ->
            { model | rows = model.rows |> List.filter (\t -> t.id /= Just rowId) }
                ! [ deletePastMedicalHistoryRequest rowId ]

        DeleteCompleted (Ok responseMsg) ->
            case Functions.getResponseError responseMsg of
                Just t ->
                    model ! [ Functions.displayErrorMessage t, load patientId ]

                Nothing ->
                    model ! [ Functions.displaySuccessMessage "Record deleted successfully!" ]

        DeleteCompleted (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SendMenuMessage recordId ->
            model ! [ sendMenuMessage (MenuMessage "PastMedicalHistoryDelete" recordId Nothing Nothing) ]

        NoOp ->
            model ! []

        -- Updates
        UpdateDescription newRecord str ->
            { model | state = AddEdit { newRecord | description = str } } ! []

        UpdateYear newRecord str ->
            { model | state = AddEdit { newRecord | year = str } } ! []

        UpdateFacility newRecord str ->
            { model | state = AddEdit { newRecord | facility = str } } ! []

        UpdateProvider newRecord dropdownMsg ->
            let
                ( newDropState, newId, newMsg ) =
                    Dropdown.update dropdownMsg newRecord.providerDropState newRecord.providerId newRecord.addEditDataSource.providers
            in
                { model | state = AddEdit { newRecord | providerDropState = newDropState, providerId = newId } } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateNotes newRecord str ->
            { model | state = AddEdit { newRecord | notes = str } } ! []


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    case model.state of
        Grid ->
            div []
                [ h4 [] [ text "Past Medical History" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view model.tableState model.rows (gridConfig addEditDataSource) Nothing ]
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



-- getColumns : Maybe AddEditDataSource -> Table.State -> List (Table.Column PastMedicalHistoryRow Msg)
-- getColumns addEditDataSource state =
-- TODO, verify menu items are correct
-- let
--     menuItems row =
--         [ case addEditDataSource of
--             Just t ->
--                 ( "e-edit", "Edit", onClick (Edit t row.id) )
--             Nothing ->
--                 ( "", "No Datasrc", class "disabled" )
--         , ( "e-contextdelete", "Delete", onClick (DeletePrompt row.id) )
--         ]
-- in


noteStyle : Html.Attribute msg
noteStyle =
    style [ ( "color", "#969696" ), ( "font-size", "12px" ) ]


formInputs : NewRecord -> List (InputControlType Msg)
formInputs newRecord =
    [ HtmlElement "" (div [ noteStyle ] [ text "*Records added from the problem list cannot be edited." ])
    , AreaInput "Description" Required newRecord.description (UpdateDescription newRecord)
    , TextInput "Year" Optional newRecord.year (UpdateYear newRecord)
    , Dropdown "Provider" Optional newRecord.providerDropdown (UpdateProvider newRecord)
    , TextInput "Facility" Optional newRecord.facility (UpdateFacility newRecord)
    , TextInput "Notes" Optional newRecord.notes (UpdateNotes newRecord)
    , HtmlElement "Treatment" (input [ type_ "textbox", class "e-textbox", disabled True, value newRecord.treatment ] [])
    , HtmlElement "" (div [ noteStyle ] [ text "*Treatment is deprecated." ])
    ]


gridConfig : Maybe AddEditDataSource -> Table.Config PastMedicalHistoryRow Msg
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
        [ stringColumn "Description" .description
        , stringColumn "Year" .year
        , stringColumn "Facility" .facility
        , stringColumn "Provider" .provider
        , stringColumn "Notes" .notes

        -- TODO, verify dropdown is present
        -- , Table.dropdownColumn (\t -> Table.dropdownDetails (menuItems t) t.id state SetTableState)
        ]
    }


decodePastMedicalHistoryRow : Decode.Decoder PastMedicalHistoryRow
decodePastMedicalHistoryRow =
    decode PastMedicalHistoryRow
        |> required "Id" (Decode.maybe Decode.int)
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
        , ( "ProviderId", maybeVal Encode.int <| newRecord.providerId )
        , ( "ProblemId", maybeVal Encode.int <| newRecord.problemId )
        ]


type alias PastMedicalHistoryRow =
    { id : Maybe Int
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
    , providerId : Maybe Int
    , providerDropState : Dropdown.DropState
    , addEditDataSource : AddEditDataSource
    }


newRecord : AddEditDataSource -> Maybe PastMedicalHistoryRow -> NewRecord
newRecord addEditDataSource pastMedicalHistoryRow =
    case pastMedicalHistoryRow of
        Just row ->
            { id = row.id
            , description = row.description
            , year = row.year
            , facility = row.facility
            , notes = row.notes
            , treatment = row.treatment
            , problemId = row.problemId
            , providerId = row.providerId
            , providerDropState = Dropdown.init "providerDropdown"
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
            , providerId = Nothing
            , providerDropState = Dropdown.init "providerDropdown"
            , addEditDataSource = addEditDataSource
            }


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.init ""
    , state = Grid
    , showValidationErrors = False
    }


deletePastMedicalHistoryRequest : Int -> Cmd Msg
deletePastMedicalHistoryRequest rowId =
    Http.send DeleteCompleted <| Http.getString ("/People/DeletePastMedicalHistory?id=" ++ toString rowId)


load : Int -> Cmd Msg
load patientId =
    Decode.list decodePastMedicalHistoryRow
        |> Http.get ("/People/PastMedicalHistoriesGrid?patientId=" ++ toString patientId)
        |> Http.send LoadData

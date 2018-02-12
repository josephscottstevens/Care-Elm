module PastMedicalHistory exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Html exposing (Html, text, div, button, input, h4)
import Html.Attributes exposing (class, style, type_, disabled, value)
import Html.Events exposing (onClick)
import Common.Html exposing (InputControlType(TextInput, AreaInput, Dropdown, HtmlElement), makeControls, defaultConfig, getValidationErrors, fullWidth)
import Common.Types exposing (RequiredType(Optional, Required), AddEditDataSource, MenuMessage, DropdownItem)
import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal, sendMenuMessage, setUnsavedChanges)
import Common.Grid exposing (standardTableAttrs, standardTheadNoFilters)
import Common.Dropdown as Dropdown
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)
import Common.Table as Table exposing (defaultCustomizations)
import Http


subscriptions : Sub Msg
subscriptions =
    Functions.deleteConfirmed DeletePastMedicalHistoryConfirmed


init : Int -> Cmd Msg
init patientId =
    load patientId


type State
    = Grid
    | AddEdit EditData


type alias Model =
    { rows : List PastMedicalHistoryRow
    , state : State
    , tableState : Table.State
    , showValidationErrors : Bool
    }


type alias EditData =
    { id : Int
    , description : String
    , year : String
    , facility : String
    , notes : String
    , treatment : String
    , problemId : Maybe Int
    , providerId : Maybe Int
    , providerDropState : Dropdown.DropState
    , addEditDataSource : AddEditDataSource
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model maybeAddEditDataSource =
    case model.state of
        Grid ->
            div []
                [ h4 [] [ text "Past Medical History" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config maybeAddEditDataSource model.tableState) model.tableState model.rows ]
                ]

        AddEdit editData ->
            let
                errors =
                    getValidationErrors (formInputs editData)

                validationErrorsDiv =
                    if model.showValidationErrors == True && List.length errors > 0 then
                        div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
                    else
                        div [] []
            in
                div [ class "form-horizontal" ]
                    [ validationErrorsDiv
                    , h4 [] [ text "Past Medical History" ]
                    , makeControls defaultConfig (formInputs editData)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", onClick (Save editData), class "btn btn-sm btn-success" ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default margin-left-5" ] [ text "Cancel" ]
                            ]
                        ]
                    ]


getColumns : Maybe AddEditDataSource -> Table.State -> List (Table.Column PastMedicalHistoryRow Msg)
getColumns addEditDataSource state =
    let
        menuItems row =
            [ case addEditDataSource of
                Just t ->
                    ( "e-edit", "Edit", onClick (Edit t row.id) )

                Nothing ->
                    ( "", "No Datasrc", class "disabled" )
            , ( "e-contextdelete", "Delete", onClick (DeletePrompt row.id) )
            ]
    in
        [ Table.stringColumn "Description" (\t -> t.description)
        , Table.stringColumn "Year" (\t -> t.year)
        , Table.stringColumn "Facility" (\t -> t.facility)
        , Table.stringColumn "Provider" (\t -> t.provider)
        , Table.stringColumn "Notes" (\t -> t.notes)
        , Table.dropdownColumn (\t -> Table.dropdownDetails (menuItems t) t.id state SetTableState)
        ]


noteStyle : Html.Attribute msg
noteStyle =
    style [ ( "color", "#969696" ), ( "font-size", "12px" ) ]


formInputs : EditData -> List (InputControlType Msg)
formInputs editData =
    [ HtmlElement "" (div [ noteStyle ] [ text "*Records added from the problem list cannot be edited." ])
    , AreaInput "Description" Required editData.description (UpdateDescription editData)
    , TextInput "Year" Optional editData.year (UpdateYear editData)
    , Dropdown "Provider" Optional editData.providerDropState (UpdateProvider editData) editData.addEditDataSource.providers editData.providerId
    , TextInput "Facility" Optional editData.facility (UpdateFacility editData)
    , TextInput "Notes" Optional editData.notes (UpdateNotes editData)
    , HtmlElement "Treatment" (input [ type_ "textbox", class "e-textbox", disabled True, value editData.treatment ] [])
    , HtmlElement "" (div [ noteStyle ] [ text "*Treatment is deprecated." ])
    ]


type Msg
    = LoadData (Result Http.Error (List PastMedicalHistoryRow))
    | Cancel
    | Save EditData
    | SaveCompleted (Result Http.Error String)
    | Add AddEditDataSource
    | Edit AddEditDataSource Int
    | SetTableState Table.State
    | DeletePrompt Int
    | DeletePastMedicalHistoryConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | SendMenuMessage Int
      -- Updates
    | UpdateDescription EditData String
    | UpdateYear EditData String
    | UpdateFacility EditData String
    | UpdateProvider EditData Dropdown.Msg
    | UpdateNotes EditData String


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    let
        updateEdit t =
            { model | state = AddEdit t }
    in
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
                                |> Functions.postRequest (encodeEditData row patientId)
                                |> Http.send SaveCompleted
                          , setUnsavedChanges False
                          ]

            SaveCompleted (Ok _) ->
                { model | state = Grid } ! [ displaySuccessMessage "Past Medical History Saved Successfully!", load patientId ]

            SaveCompleted (Err t) ->
                { model | state = Grid } ! [ displayErrorMessage (toString t), load patientId ]

            Add addEditDataSource ->
                { model | state = AddEdit (initEditData addEditDataSource Nothing) } ! []

            Edit addEditDataSource rowId ->
                let
                    row =
                        model.rows |> List.filter (\t -> t.id == rowId) |> List.head
                in
                    { model | state = AddEdit (initEditData addEditDataSource row) } ! []

            SetTableState newState ->
                { model | tableState = newState } ! []

            DeletePrompt rowId ->
                model ! [ Functions.deletePrompt rowId ]

            DeletePastMedicalHistoryConfirmed rowId ->
                { model | rows = model.rows |> List.filter (\t -> t.id /= rowId) }
                    ! [ Http.getString ("/People/DeletePastMedicalHistory?id=" ++ toString rowId)
                            |> Http.send DeleteCompleted
                      ]

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

            -- Updates
            UpdateDescription editData str ->
                updateEdit { editData | description = str } ! []

            UpdateYear editData str ->
                updateEdit { editData | year = str } ! []

            UpdateFacility editData str ->
                updateEdit { editData | facility = str } ! []

            UpdateProvider editData dropdownMsg ->
                let
                    ( newDropState, newId, newMsg ) =
                        Dropdown.update dropdownMsg editData.providerDropState editData.providerId editData.addEditDataSource.providers
                in
                    updateEdit { editData | providerDropState = newDropState, providerId = newId }
                        ! [ newMsg, Functions.setUnsavedChanges True ]

            UpdateNotes editData str ->
                updateEdit { editData | notes = str } ! []


config : Maybe AddEditDataSource -> Table.State -> Table.Config PastMedicalHistoryRow Msg
config addEditDataSource state =
    let
        buttons =
            case addEditDataSource of
                Just t ->
                    [ ( "e-addnew", onClick (Add t) ) ]

                Nothing ->
                    []
    in
        Table.customConfig
            { toId = \t -> toString t.id
            , toMsg = SetTableState
            , columns = getColumns addEditDataSource state
            , customizations =
                { defaultCustomizations
                    | tableAttrs = standardTableAttrs "RecordTable"
                    , thead = standardTheadNoFilters
                    , theadButtons = buttons
                }
            }


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
    }


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


encodeEditData : EditData -> Int -> Encode.Value
encodeEditData newRecord patientId =
    Encode.object
        [ ( "Id", Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Description", Encode.string <| newRecord.description )
        , ( "Year", Encode.string <| newRecord.year )
        , ( "Treatment", Encode.string <| newRecord.treatment )
        , ( "Facility", Encode.string <| newRecord.facility )
        , ( "Notes", Encode.string <| newRecord.notes )
        , ( "ProviderId", maybeVal Encode.int <| newRecord.providerId )
        , ( "ProblemId", maybeVal Encode.int <| newRecord.problemId )
        ]


load : Int -> Cmd Msg
load patientId =
    Decode.list decodePastMedicalHistoryRow
        |> Http.get ("/People/PastMedicalHistoriesGrid?patientId=" ++ toString patientId)
        |> Http.send LoadData


initEditData : AddEditDataSource -> Maybe PastMedicalHistoryRow -> EditData
initEditData addEditDataSource pastMedicalHistoryRow =
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
            { id = -1
            , description = ""
            , year = ""
            , facility = ""
            , notes = ""
            , treatment = ""
            , problemId = Nothing
            , providerId = Nothing
            , providerDropState = Dropdown.init "providerDropdown"
            , addEditDataSource = addEditDataSource
            }


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.initialSort ""
    , showValidationErrors = False
    , state = Grid
    }

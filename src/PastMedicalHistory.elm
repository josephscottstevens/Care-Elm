port module PastMedicalHistory exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Html exposing (Html, text, div, button, h4, input)
import Html.Attributes exposing (class, id, style, type_, disabled, value)
import Html.Events exposing (onClick)
import Common.Html exposing (InputControlType(TextInput, AreaInput, Dropdown, HtmlElement), makeControls, defaultConfig, getValidationErrors, fullWidth)
import Common.Types exposing (RequiredType(Optional, Required), AddEditDataSource, MenuMessage, DropdownItem)
import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, maybeVal)
import Common.Grid exposing (checkColumn, standardTableAttrs, standardTheadNoFilters, rowDropDownDiv)
import Common.Ports exposing (sendMenuMessage)
import Common.Dropdown as Dropdown
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Table exposing (defaultCustomizations)
import Common.Mouse as Mouse
import Http


port deletePastMedicalHistory : Int -> Cmd msg


port deletePastMedicalHistoryConfirmed : (Int -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ deletePastMedicalHistoryConfirmed DeletePastMedicalHistoryConfirmed
        , Mouse.clicks Blur
        ]


type State
    = Grid
    | AddEdit NewRecord


type alias Model =
    { rows : List PastMedicalHistoryRow
    , tableState : Table.State
    , state : State
    }


init : Int -> Cmd Msg
init patientId =
    Decode.list decodePastMedicalHistoryRow
        |> Http.get ("/People/PastMedicalHistoriesGrid?patientId=" ++ toString patientId)
        |> Http.send LoadData


type Msg
    = LoadData (Result Http.Error (List PastMedicalHistoryRow))
    | Cancel
    | Save NewRecord
    | SaveCompleted (Result Http.Error String)
    | Add AddEditDataSource
    | Edit AddEditDataSource Int
    | Blur Mouse.Position
    | SetTableState Table.State
    | DropDownToggle Int
    | DeletePastMedicalHistoryConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | SendMenuMessage Int
      -- Updates
    | UpdateDescription NewRecord String
    | UpdateYear NewRecord String
    | UpdateFacility NewRecord String
    | UpdateProvider NewRecord Dropdown.Msg
    | UpdateNotes NewRecord String


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
            model
                ! [ "People/AddUpdatePastMedicalHistories"
                        |> Functions.postRequest (encodeNewRow row patientId)
                        |> Http.send SaveCompleted
                  ]

        SaveCompleted (Ok _) ->
            { model | state = Grid } ! [ displaySuccessMessage "Clinical Summary Saved Successfully!", init patientId ]

        SaveCompleted (Err t) ->
            { model | state = Grid } ! [ displayErrorMessage (toString t) ]

        Add addEditDataSource ->
            { model | state = AddEdit (newRecord addEditDataSource Nothing) } ! []

        Edit addEditDataSource rowId ->
            let
                row =
                    model.rows |> List.filter (\t -> t.id == rowId) |> List.head
            in
                { model | state = AddEdit (newRecord addEditDataSource row) } ! []

        Blur position ->
            case model.state of
                Grid ->
                    { model | rows = Functions.closeDropdowns model.rows position.target } ! []

                AddEdit newRecord ->
                    { model | state = AddEdit { newRecord | providerDropdown = Dropdown.close newRecord.providerDropdown } } ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        DropDownToggle recordId ->
            { model | rows = Functions.flipDropdownOpen model.rows recordId } ! []

        DeletePastMedicalHistoryConfirmed rowId ->
            { model | rows = model.rows |> List.filter (\t -> t.id /= rowId) }
                ! [ deletePastMedicalHistoryRequest rowId ]

        DeleteCompleted (Ok responseMsg) ->
            case Functions.getResponseError responseMsg of
                Just t ->
                    model ! [ Functions.displayErrorMessage t ]

                Nothing ->
                    model ! [ Functions.displaySuccessMessage "Record deleted successfully!" ]

        DeleteCompleted (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SendMenuMessage recordId ->
            model ! [ sendMenuMessage (MenuMessage "PastMedicalHistoryDelete" recordId Nothing Nothing) ]

        -- Updates
        UpdateDescription newRecord str ->
            { model | state = AddEdit { newRecord | description = str } } ! []

        UpdateYear newRecord str ->
            { model | state = AddEdit { newRecord | year = str } } ! []

        UpdateFacility newRecord str ->
            { model | state = AddEdit { newRecord | facility = str } } ! []

        UpdateProvider newRecord dropdownMsg ->
            { model | state = AddEdit { newRecord | providerDropdown = Dropdown.update dropdownMsg newRecord.providerDropdown } } ! []

        UpdateNotes newRecord str ->
            { model | state = AddEdit { newRecord | notes = str } } ! []


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    case model.state of
        Grid ->
            div []
                [ case addEditDataSource of
                    Just t ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick <| Add t ] [ text "New Record" ]

                    Nothing ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config addEditDataSource) model.tableState model.rows ]
                ]

        AddEdit newRecord ->
            let
                errors =
                    getValidationErrors (formInputs newRecord)

                validationErrorsDiv =
                    if newRecord.showValidationErrors == True && List.length errors > 0 then
                        div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
                    else
                        div [] []

                saveBtnClass =
                    class "btn btn-sm btn-success margin-left-5 pull-right"
            in
                div [ class "form-horizontal" ]
                    [ validationErrorsDiv
                    , makeControls defaultConfig (formInputs newRecord)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", id "Save", onClick (Save newRecord), saveBtnClass ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default pull-right" ] [ text "Cancel" ]
                            ]
                        ]
                    ]


getColumns : Maybe AddEditDataSource -> List (Table.Column PastMedicalHistoryRow Msg)
getColumns addEditDataSource =
    [ Table.stringColumn "Description" (\t -> t.description)
    , Table.stringColumn "Year" (\t -> t.year)
    , Table.stringColumn "Facility" (\t -> t.facility)
    , Table.stringColumn "Provider" (\t -> t.provider)
    , Table.stringColumn "Notes" (\t -> t.notes)
    , rowDropDownColumn addEditDataSource
    ]


formInputs : NewRecord -> List (InputControlType Msg)
formInputs newRecord =
    [ AreaInput "Description" Required newRecord.description (UpdateDescription newRecord)
    , TextInput "Year" Required newRecord.year (UpdateYear newRecord)
    , Dropdown "Provider" Required newRecord.providerDropdown (UpdateProvider newRecord)
    , TextInput "Facility" Required newRecord.facility (UpdateFacility newRecord)
    , TextInput "Notes" Required newRecord.notes (UpdateNotes newRecord)
    , HtmlElement "Treatment" (input [ type_ "textbox", class "e-textbox", disabled True, value newRecord.treatment ] [])
    , HtmlElement "" (div [ style [ ( "color", "#969696" ), ( "font-size", "12px" ) ] ] [ text "*Treatment is deprecated." ])
    ]


rowDropDownColumn : Maybe AddEditDataSource -> Table.Column PastMedicalHistoryRow Msg
rowDropDownColumn addEditDataSource =
    Table.veryCustomColumn
        { name = ""
        , viewData = \t -> rowDropDownDiv t.dropdownOpen (onClick (DropDownToggle t.id)) (dropdownItems t.id addEditDataSource)
        , sorter = Table.unsortable
        }


dropdownItems : Int -> Maybe AddEditDataSource -> List ( String, String, Html.Attribute Msg )
dropdownItems rowId addEditDataSource =
    [ case addEditDataSource of
        Just t ->
            ( "e-edit", "Edit", onClick (Edit t rowId) )

        Nothing ->
            ( "", "No Datasrc", class "disabled" )
    , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId) )
    ]


config : Maybe AddEditDataSource -> Table.Config PastMedicalHistoryRow Msg
config addEditDataSource =
    Table.customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns addEditDataSource
        , customizations =
            { defaultCustomizations | tableAttrs = standardTableAttrs "RecordTable", thead = standardTheadNoFilters }
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
        |> hardcoded False


encodeNewRow : NewRecord -> Int -> Encode.Value
encodeNewRow newRecord patientId =
    Encode.object
        [ ( "Id", Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Description", Encode.string <| newRecord.description )
        , ( "Year", Encode.string <| newRecord.year )
        , ( "Treatment", Encode.string <| newRecord.treatment )
        , ( "Facility", Encode.string <| newRecord.facility )
        , ( "Notes", Encode.string <| newRecord.notes )
        , ( "ProviderId", maybeVal Encode.int <| newRecord.providerDropdown.dropdownItem.id )
        , ( "ProblemId", maybeVal Encode.int <| newRecord.problemId )
        ]


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


newRecord : AddEditDataSource -> Maybe PastMedicalHistoryRow -> NewRecord
newRecord addEditDataSource pastMedicalHistoryRow =
    case pastMedicalHistoryRow of
        Just row ->
            { showValidationErrors = False
            , id = row.id
            , description = row.description
            , year = row.year
            , facility = row.facility
            , notes = row.notes
            , treatment = row.treatment
            , problemId = row.problemId
            , providerDropdown = Dropdown.init addEditDataSource.providers row.providerId row.provider
            }

        Nothing ->
            { showValidationErrors = False
            , id = -1
            , description = ""
            , year = ""
            , facility = ""
            , notes = ""
            , treatment = ""
            , problemId = Nothing
            , providerDropdown = Dropdown.init addEditDataSource.providers Nothing ""
            }


type alias NewRecord =
    { showValidationErrors : Bool
    , id : Int
    , description : String
    , year : String
    , facility : String
    , notes : String
    , treatment : String
    , problemId : Maybe Int
    , providerDropdown : Dropdown.Dropdown
    }


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.initialSort ""
    , state = Grid
    }


deletePastMedicalHistoryRequest : Int -> Cmd Msg
deletePastMedicalHistoryRequest rowId =
    Http.send DeleteCompleted <| Http.getString ("/People/DeleteHospitilization?id=" ++ toString rowId)

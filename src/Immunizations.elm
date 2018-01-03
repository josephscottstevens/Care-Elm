module Immunizations exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Table as Table exposing (defaultCustomizations)
import Common.Grid exposing (standardTableAttrs, standardTheadNoFilters)
import Common.Types exposing (MenuMessage, RequiredType(Optional, Required), AddEditDataSource)
import Common.Functions as Functions exposing (defaultString, sendMenuMessage, setUnsavedChanges, maybeVal)
import Common.Html exposing (InputControlType(TextInput), getValidationErrors, defaultConfig, fullWidth, makeControls)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.deleteConfirmed DeleteConfirmed
        ]


init : Int -> Cmd Msg
init patientId =
    load patientId


type alias Model =
    { rows : List Row
    , tableState : Table.State
    , editData : Maybe EditData
    , showValidationErrors : Bool
    }


type alias EditData =
    { id : Maybe Int
    , vaccination : String
    , year : String
    , facility : String
    , notes : String
    }


type alias Row =
    { id : Int
    , vaccination : String
    , year : Maybe String
    , facility : Maybe String
    , notes : Maybe String
    }


formInputs : EditData -> List (InputControlType Msg)
formInputs editData =
    [ TextInput "Vaccination" Required editData.vaccination (UpdateVaccination editData)
    , TextInput "Year" Optional editData.year (UpdateYear editData)
    , TextInput "Facility" Optional editData.facility (UpdateFacility editData)
    , TextInput "Notes" Optional editData.notes (UpdateNotes editData)
    ]


view : Model -> Maybe AddEditDataSource -> Html Msg
view model _ =
    case model.editData of
        Nothing ->
            div []
                [ h4 [] [ text "Immunizations & Preventative Screenings" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config model.tableState) model.tableState model.rows ]
                ]

        Just editData ->
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
                    , makeControls defaultConfig (formInputs editData)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", onClick (Save editData), class "btn btn-sm btn-success" ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default margin-left-5" ] [ text "Cancel" ]
                            ]
                        ]
                    ]


type Msg
    = Load (Result Http.Error (List Row))
    | SetTableState Table.State
    | DeletePrompt Int
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | Add
    | Edit Row
    | SendMenuMessage Int String
    | Save EditData
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateVaccination EditData String
    | UpdateYear EditData String
    | UpdateFacility EditData String
    | UpdateNotes EditData String


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    let
        updateAddNew t =
            t ! [ setUnsavedChanges True ]
    in
        case msg of
            Load (Ok t) ->
                { model | rows = t } ! [ Functions.setLoadingStatus False ]

            Load (Err t) ->
                model ! [ Functions.displayErrorMessage (toString t) ]

            SetTableState newState ->
                { model | tableState = newState } ! []

            SendMenuMessage recordId messageType ->
                model ! [ sendMenuMessage (MenuMessage messageType recordId Nothing Nothing) ]

            DeletePrompt rowId ->
                model ! [ Functions.deletePrompt rowId ]

            DeleteConfirmed rowId ->
                let
                    rows =
                        model.rows |> List.filter (\t -> t.id /= rowId)
                in
                    { model | rows = rows }
                        ! [ Http.getString ("/People/ImmunizationsDelete?id=" ++ toString rowId)
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

            Add ->
                { model | editData = Just (getEditData Nothing) } ! []

            Edit row ->
                { model | editData = Just (getEditData (Just row)) } ! []

            -- edit
            Save editData ->
                if List.length (getValidationErrors (formInputs editData)) > 0 then
                    { model | showValidationErrors = True } ! []
                else
                    let
                        body =
                            encodeEditData editData patientId
                    in
                        model
                            ! [ Functions.postRequest body "/People/ImmunizationsAddEdit"
                                    |> Http.send SaveCompleted
                              , setUnsavedChanges False
                              ]

            SaveCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        model ! [ Functions.displayErrorMessage t ]

                    Nothing ->
                        { model | editData = Nothing }
                            ! [ Functions.displaySuccessMessage "Save completed successfully!"
                              , load patientId
                              ]

            SaveCompleted (Err t) ->
                model ! [ Functions.displayErrorMessage (toString t) ]

            Cancel ->
                { model | editData = Nothing } ! [ setUnsavedChanges False ]

            UpdateVaccination editData t ->
                updateAddNew { model | editData = Just { editData | vaccination = t } }

            UpdateYear editData t ->
                updateAddNew { model | editData = Just { editData | year = t } }

            UpdateFacility editData t ->
                updateAddNew { model | editData = Just { editData | facility = t } }

            UpdateNotes editData t ->
                updateAddNew { model | editData = Just { editData | notes = t } }


getColumns : Table.State -> List (Table.Column Row Msg)
getColumns state =
    let
        dropDownItems row =
            [ ( "e-edit", "Edit", onClick (Edit row) )
            , ( "e-contextdelete", "Delete", onClick (DeletePrompt row.id) )
            ]
    in
        [ Table.stringColumn "Vaccination" (\t -> t.vaccination)
        , Table.stringColumn "Year" (\t -> defaultString t.year)
        , Table.stringColumn "Facilit" (\t -> defaultString t.facility)
        , Table.stringColumn "Notes" (\t -> defaultString t.notes)
        , Table.dropdownColumn (\t -> Table.dropdownDetails (dropDownItems t) t.id state SetTableState)
        ]


config : Table.State -> Table.Config Row Msg
config state =
    let
        buttons =
            [ ( "e-addnew", onClick Add ) ]
    in
        Table.customConfig
            { toId = \t -> toString t.id
            , toMsg = SetTableState
            , columns = getColumns state
            , customizations =
                { defaultCustomizations
                    | tableAttrs = standardTableAttrs "RecordTable"
                    , thead = standardTheadNoFilters
                    , theadButtons = buttons
                }
            }


emptyModel : Model
emptyModel =
    { editData = Nothing
    , rows = []
    , tableState = Table.initialSort "Date"
    , showValidationErrors = False
    }


getEditData : Maybe Row -> EditData
getEditData maybeRow =
    case maybeRow of
        Just row ->
            { id = Just row.id
            , vaccination = row.vaccination
            , year = defaultString row.year
            , facility = defaultString row.facility
            , notes = defaultString row.notes
            }

        Nothing ->
            { id = Nothing
            , vaccination = ""
            , year = ""
            , facility = ""
            , notes = ""
            }


encodeEditData : EditData -> Int -> Encode.Value
encodeEditData newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Vaccination", Encode.string <| newRecord.vaccination )
        , ( "Year", Encode.string <| newRecord.year )
        , ( "Facility", Encode.string <| newRecord.facility )
        , ( "Notes", Encode.string <| newRecord.notes )
        ]


decodeHospitilizationsRow : Decode.Decoder Row
decodeHospitilizationsRow =
    Pipeline.decode Row
        |> Pipeline.required "Id" Decode.int
        |> Pipeline.required "Vaccination" Decode.string
        |> Pipeline.required "Year" (Decode.maybe Decode.string)
        |> Pipeline.required "Facility" (Decode.maybe Decode.string)
        |> Pipeline.required "Notes" (Decode.maybe Decode.string)


load : Int -> Cmd Msg
load patientId =
    Decode.list decodeHospitilizationsRow
        |> Http.get ("/People/ImmunizationsGrid?patientId=" ++ toString patientId)
        |> Http.send Load

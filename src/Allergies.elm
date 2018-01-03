module Allergies exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Table as Table exposing (defaultCustomizations)
import Common.Grid exposing (checkColumn, standardTableAttrs, standardTheadNoFilters)
import Common.Types exposing (MenuMessage, RequiredType(Optional, Required), DropdownItem, AddEditDataSource)
import Common.Functions as Functions exposing (defaultString, defaultDate, sendMenuMessage, setUnsavedChanges, maybeVal, maybeToDateString)
import Common.Route as Route
import Common.Html
    exposing
        ( InputControlType(CheckInput, DropInputWithButton, AreaInput, TextInput, DateInput, KnockInput, DropInput)
        , getValidationErrors
        , defaultConfig
        , fullWidth
        , makeControls
        )
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
    , allergy : String
    , reaction : String
    }


type alias Row =
    { id : Int
    , allergy : String
    , reaction : Maybe String
    }


formInputs : EditData -> List (InputControlType Msg)
formInputs editData =
    [ TextInput "Allergy" Required editData.allergy (UpdateAllergy editData)
    , TextInput "Reaction" Optional editData.reaction (UpdateReaction editData)
    ]


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    case model.editData of
        Nothing ->
            div []
                [ div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config addEditDataSource model.tableState) model.tableState model.rows ]
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
    | Add AddEditDataSource
    | Edit Row AddEditDataSource
    | SendMenuMessage Int String
    | Save EditData
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateAllergy EditData String
    | UpdateReaction EditData String


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    let
        updateAddNew t =
            t ! [ setUnsavedChanges True ]
    in
        case msg of
            Load (Ok t) ->
                getLoadedState model t ! [ Functions.setLoadingStatus False ]

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
                        ! [ Http.getString ("/People/AllergiesDelete?id=" ++ toString rowId)
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

            Add addEditDataSource ->
                let
                    editData =
                        getEditData addEditDataSource Nothing
                in
                    { model | editData = Just editData } ! []

            Edit row addEditDataSource ->
                let
                    editData =
                        getEditData addEditDataSource (Just row)
                in
                    { model | editData = Just editData } ! []

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
                            ! [ Functions.postRequest body "/People/AllergiesAddEdit"
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

            UpdateAllergy editData t ->
                updateAddNew { model | editData = Just { editData | allergy = t } }

            UpdateReaction editData t ->
                updateAddNew { model | editData = Just { editData | reaction = t } }


getColumns : Maybe AddEditDataSource -> Table.State -> List (Table.Column Row Msg)
getColumns addEditDataSource state =
    let
        dropDownItems row =
            case addEditDataSource of
                Just t ->
                    [ ( "e-edit", "Edit", onClick (Edit row t) )
                    , ( "e-contextdelete", "Delete", onClick (DeletePrompt row.id) )
                    ]

                Nothing ->
                    []
    in
        [ Table.stringColumn "Allergy" (\t -> t.allergy)
        , Table.stringColumn "Reaction" (\t -> defaultString t.reaction)
        , Table.dropdownColumn (\t -> Table.dropdownDetails (dropDownItems t) t.id state SetTableState)
        ]


config : Maybe AddEditDataSource -> Table.State -> Table.Config Row Msg
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


decodeHospitilizationsRow : Decode.Decoder Row
decodeHospitilizationsRow =
    Pipeline.decode Row
        |> Pipeline.required "Id" Decode.int
        |> Pipeline.required "Allergy" Decode.string
        |> Pipeline.required "Reaction" (Decode.maybe Decode.string)


delete : a -> (Result Http.Error String -> msg) -> Cmd msg
delete rowId deleteCompleted =
    Http.send deleteCompleted <| Http.getString ("/People/AllergiesDelete?id=" ++ toString rowId)


getLoadedState : Model -> List Row -> Model
getLoadedState model hospitilizationsRow =
    { model | rows = hospitilizationsRow }


emptyModel : Model
emptyModel =
    { editData = Nothing
    , rows = []
    , tableState = Table.initialSort "Date"
    , showValidationErrors = False
    }


getEditData : AddEditDataSource -> Maybe Row -> EditData
getEditData addEditDataSource maybeRow =
    case maybeRow of
        Just row ->
            { id = Just row.id
            , allergy = row.allergy
            , reaction = Maybe.withDefault "" row.reaction
            }

        Nothing ->
            { id = Nothing
            , allergy = ""
            , reaction = ""
            }


encodeEditData : EditData -> Int -> Encode.Value
encodeEditData newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Allergy", Encode.string <| newRecord.allergy )
        , ( "Reaction", Encode.string <| newRecord.reaction )
        ]


load : Int -> Cmd Msg
load patientId =
    Decode.list decodeHospitilizationsRow
        |> Http.get ("/People/AllergiesGrid?patientId=" ++ toString patientId)
        |> Http.send Load

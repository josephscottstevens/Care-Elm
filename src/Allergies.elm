module Allergies exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions exposing (maybeVal, sendMenuMessage, setUnsavedChanges)
import Common.Html exposing (InputControlType(TextInput), defaultConfig, fullWidth, getValidationErrors, makeControls)
import Common.Table as Table
import Common.Types exposing (AddEditDataSource, MenuMessage, RequiredType(Optional, Required))
import Html exposing (Html, button, div, h4, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.dialogConfirmed DeleteConfirmed
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
    , allergy : Maybe String
    , reaction : Maybe String
    }


type alias Row =
    { id : Int
    , allergy : Maybe String
    , reaction : Maybe String
    }


formInputs : EditData -> List (InputControlType Msg)
formInputs editData =
    [ TextInput "Allergy" Required editData.allergy (UpdateAllergy editData)
    , TextInput "Reaction" Optional editData.reaction (UpdateReaction editData)
    ]


view : Model -> Maybe AddEditDataSource -> Html Msg
view model _ =
    div []
        [ h4 [] [ text "Allergies" ]
        , case model.editData of
            Nothing ->
                Table.view model.tableState model.rows gridConfig Nothing

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
        ]


type Msg
    = Load (Result Http.Error (List Row))
    | SetTableState Table.State
    | DeletePrompt Row
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | Add
    | Edit Row
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
            { model | rows = t } ! [ Functions.setLoadingStatus False ]

        Load (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId messageType ->
            model ! [ sendMenuMessage (MenuMessage messageType recordId Nothing Nothing) ]

        DeletePrompt row ->
            model ! [ Functions.deleteDialogShow row.id ]

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
            updateAddNew { model | editData = Just { editData | allergy = Just t } }

        UpdateReaction editData t ->
            updateAddNew { model | editData = Just { editData | reaction = Just t } }


gridConfig : Table.Config Row Msg
gridConfig =
    { domTableId = "AllergyTable"
    , toolbar =
        [ ( "e-addnew", Add ) ]
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Allergy" .allergy
        , Table.stringColumn "Reaction" .reaction
        , Table.dropdownColumn
            [ ( "e-edit", "Edit", Edit )
            , ( "e-contextdelete", "Delete", DeletePrompt )
            ]
        ]
    }


emptyModel : Model
emptyModel =
    { editData = Nothing
    , rows = []
    , tableState = Table.init "Date"
    , showValidationErrors = False
    }


getEditData : Maybe Row -> EditData
getEditData maybeRow =
    case maybeRow of
        Just row ->
            { id = Just row.id
            , allergy = row.allergy
            , reaction = row.reaction
            }

        Nothing ->
            { id = Nothing
            , allergy = Nothing
            , reaction = Nothing
            }


encodeEditData : EditData -> Int -> Encode.Value
encodeEditData newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Allergy", maybeVal Encode.string <| newRecord.allergy )
        , ( "Reaction", maybeVal Encode.string <| newRecord.reaction )
        ]


decodeHospitilizationsRow : Decode.Decoder Row
decodeHospitilizationsRow =
    Pipeline.decode Row
        |> Pipeline.required "Id" Decode.int
        |> Pipeline.required "Allergy" (Decode.maybe Decode.string)
        |> Pipeline.required "Reaction" (Decode.maybe Decode.string)


load : Int -> Cmd Msg
load patientId =
    Decode.list decodeHospitilizationsRow
        |> Http.get ("/People/AllergiesGrid?patientId=" ++ toString patientId)
        |> Http.send Load

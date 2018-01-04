port module LastKnownVitals exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Table as Table exposing (defaultCustomizations)
import Common.Grid exposing (standardTableAttrs, standardTheadNoFilters)
import Common.Types exposing (MenuMessage, RequiredType(Optional, Required), AddEditDataSource)
import Common.Functions as Functions exposing (sendMenuMessage, setUnsavedChanges, maybeVal, defaultDate)
import Common.Html exposing (InputControlType(TextInput, DateInput), getValidationErrors, defaultConfig, fullWidth, makeControls)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


port initLastKnownVitals : Maybe String -> Cmd msg


port updateLastKnownVitals : (Maybe String -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.deleteConfirmed DeleteConfirmed
        , updateLastKnownVitals UpdateDate
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
    , bp : String
    , hr : String
    , r : String
    , t : String
    , o2 : String
    , wt : String
    , ht : String
    , bmi : String
    , date : Maybe String
    }


type alias Row =
    { id : Int
    , bp : String
    , hr : String
    , r : String
    , t : String
    , o2 : String
    , wt : String
    , ht : String
    , bmi : String
    , date : Maybe String
    }


formInputs : EditData -> List (InputControlType Msg)
formInputs editData =
    [ TextInput "BP" Required editData.bp (UpdateBp editData)
    , TextInput "HR" Required editData.hr (UpdateHr editData)
    , TextInput "R" Required editData.r (UpdateR editData)
    , TextInput "T" Required editData.t (UpdateT editData)
    , TextInput "O2" Required editData.o2 (UpdateO2 editData)
    , TextInput "WT" Required editData.wt (UpdateWt editData)
    , TextInput "HT" Required editData.ht (UpdateHt editData)
    , TextInput "BMI" Required editData.bmi (UpdateBmi editData)
    , DateInput "Date" Optional (defaultDate editData.date) "DateId"
    ]


view : Model -> Maybe AddEditDataSource -> Html Msg
view model _ =
    case model.editData of
        Nothing ->
            div []
                [ h4 [] [ text "LastKnownVitals" ]
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
    | UpdateBp EditData String
    | UpdateHr EditData String
    | UpdateR EditData String
    | UpdateT EditData String
    | UpdateO2 EditData String
    | UpdateWt EditData String
    | UpdateHt EditData String
    | UpdateBmi EditData String
    | UpdateDate (Maybe String)


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
                        ! [ Http.getString ("/People/LastKnownVitalsDelete?id=" ++ toString rowId)
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
                { model | editData = Just (getEditData Nothing) } ! [ initLastKnownVitals Nothing ]

            Edit row ->
                let
                    editData =
                        getEditData (Just row)
                in
                    { model | editData = Just editData } ! [ initLastKnownVitals editData.date ]

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
                            ! [ Functions.postRequest body "/People/LastKnownVitalsAddEdit"
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

            UpdateBp editData t ->
                updateAddNew { model | editData = Just { editData | bp = t } }

            UpdateHr editData t ->
                updateAddNew { model | editData = Just { editData | hr = t } }

            UpdateR editData t ->
                updateAddNew { model | editData = Just { editData | r = t } }

            UpdateT editData t2 ->
                updateAddNew { model | editData = Just { editData | t = t2 } }

            UpdateO2 editData t ->
                updateAddNew { model | editData = Just { editData | o2 = t } }

            UpdateWt editData t ->
                updateAddNew { model | editData = Just { editData | wt = t } }

            UpdateHt editData t ->
                updateAddNew { model | editData = Just { editData | ht = t } }

            UpdateBmi editData t ->
                updateAddNew { model | editData = Just { editData | bmi = t } }

            UpdateDate t ->
                case model.editData of
                    Just editData ->
                        updateAddNew { model | editData = Just { editData | date = t } }

                    Nothing ->
                        Debug.crash "whoops"


getColumns : Table.State -> List (Table.Column Row Msg)
getColumns state =
    let
        dropDownItems row =
            [ ( "e-edit", "Edit", onClick (Edit row) )
            , ( "e-contextdelete", "Delete", onClick (DeletePrompt row.id) )
            ]
    in
        [ Table.stringColumn "BP" (\t -> t.bp)
        , Table.stringColumn "HR" (\t -> t.hr)
        , Table.stringColumn "R" (\t -> t.r)
        , Table.stringColumn "T" (\t2 -> t2.t)
        , Table.stringColumn "O2" (\t -> t.o2)
        , Table.stringColumn "WT" (\t -> t.wt)
        , Table.stringColumn "HT" (\t -> t.ht)
        , Table.stringColumn "BMI" (\t -> t.bmi)
        , Table.stringColumn "Date" (\t -> defaultDate t.date)
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
            , bp = row.bp
            , hr = row.hr
            , r = row.r
            , t = row.t
            , o2 = row.o2
            , wt = row.wt
            , ht = row.ht
            , bmi = row.bmi
            , date = row.date
            }

        Nothing ->
            { id = Nothing
            , bp = ""
            , hr = ""
            , r = ""
            , t = ""
            , o2 = ""
            , wt = ""
            , ht = ""
            , bmi = ""
            , date = Nothing
            }


encodeEditData : EditData -> Int -> Encode.Value
encodeEditData newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "BP", Encode.string <| newRecord.bp )
        , ( "HR", Encode.string <| newRecord.hr )
        , ( "R", Encode.string <| newRecord.r )
        , ( "T", Encode.string <| newRecord.t )
        , ( "O2", Encode.string <| newRecord.o2 )
        , ( "WT", Encode.string <| newRecord.wt )
        , ( "HT", Encode.string <| newRecord.ht )
        , ( "BMI", Encode.string <| newRecord.bmi )
        , ( "Date", maybeVal Encode.string <| Functions.maybeToDateString <| newRecord.date )
        ]


decodeHospitilizationsRow : Decode.Decoder Row
decodeHospitilizationsRow =
    Pipeline.decode Row
        |> Pipeline.required "Id" Decode.int
        |> Pipeline.required "BP" Decode.string
        |> Pipeline.required "HR" Decode.string
        |> Pipeline.required "R" Decode.string
        |> Pipeline.required "T" Decode.string
        |> Pipeline.required "O2" Decode.string
        |> Pipeline.required "WT" Decode.string
        |> Pipeline.required "HT" Decode.string
        |> Pipeline.required "BMI" Decode.string
        |> Pipeline.required "Date" (Decode.maybe Decode.string)


load : Int -> Cmd Msg
load patientId =
    Decode.list decodeHospitilizationsRow
        |> Http.get ("/People/LastKnownVitalsGrid?patientId=" ++ toString patientId)
        |> Http.send Load

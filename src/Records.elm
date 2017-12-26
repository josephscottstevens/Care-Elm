port module Records exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, h4)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Common.Table as Table exposing (stringColumn, defaultCustomizations)
import Common.Grid exposing (hrefColumn, checkColumn)
import Common.Types as Common
import Common.Functions as Functions exposing (displaySuccessMessage, displayErrorMessage)
import Common.Ports exposing (dropDownToggle, sendMenuMessage)
import Common.Route as Route
import Http
import Json.Decode as Decode exposing (Decoder, maybe)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)


port deleteConfirmed : (Int -> msg) -> Sub msg


port editTask : Int -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ dropDownToggle DropDownToggle
        , deleteConfirmed DeleteConfirmed
        ]


init : Common.RecordType -> Int -> Cmd Msg
init recordType patientId =
    getRecords recordType patientId
        |> Http.send Load


type alias Model =
    { recordType : Common.RecordType
    , rows : List RecordRow
    , tableState : Table.State
    , dropDownState : Int
    }


emptyModel : Common.RecordType -> Model
emptyModel recordType =
    { recordType = recordType
    , rows = []
    , tableState = Table.initialSort "Date"
    , dropDownState = -1
    }


view : Model -> Maybe Common.AddEditDataSource -> Html Msg
view model addEditDataSource =
    div []
        [ h4 [] [ text (Functions.getDesc model.recordType) ]
        , div [ class "e-grid e-js e-waitingpopup" ]
            [ Table.view (config addEditDataSource model.recordType) model.tableState model.rows ]
        ]


type Msg
    = Load (Result Http.Error (List RecordRow))
    | SetTableState Table.State
    | Add
    | DropDownToggle Int
    | SendMenuMessage Int Common.RecordType String
    | EditTask Int
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model _ =
    case msg of
        Load (Ok t) ->
            { model | rows = t } ! [ Functions.setLoadingStatus False ]

        Load (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        Add ->
            model ! [ Route.modifyUrl (Route.RecordAddNew model.recordType) ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId recordType messageType ->
            { model | rows = flipConsent model.rows recordId recordType }
                ! [ sendMenuMessage (getMenuMessage model.rows recordType recordId messageType) ]

        DropDownToggle recordId ->
            { model | rows = flipDropDownOpen model.rows recordId } ! []

        DeleteConfirmed rowId ->
            let
                updatedRecords =
                    model.rows |> List.filter (\t -> t.id /= rowId)
            in
                { model | rows = updatedRecords } ! [ deleteRequest rowId DeleteCompleted ]

        DeleteCompleted (Ok responseMsg) ->
            case Functions.getResponseError responseMsg of
                Just t ->
                    model ! [ displayErrorMessage t ]

                Nothing ->
                    model ! [ displaySuccessMessage "Record deleted successfully!" ]

        DeleteCompleted (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        EditTask taskId ->
            model ! [ editTask taskId ]


getColumns : Common.RecordType -> List (Table.Column RecordRow Msg)
getColumns recordType =
    let
        commonColumns =
            [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
            , stringColumn "Doctor of Visit" (\t -> Functions.defaultString t.provider)
            , stringColumn "Specialty" (\t -> Functions.defaultString t.specialty)
            , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
            ]

        firstColumns =
            case recordType of
                Common.PrimaryCare ->
                    commonColumns

                Common.Specialty ->
                    commonColumns

                Common.Labs ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> Functions.defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Lab" (\t -> Functions.defaultString t.title)
                    , stringColumn "Provider" (\t -> Functions.defaultString t.provider)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Radiology ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> Functions.defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Study" (\t -> Functions.defaultString t.title)
                    , stringColumn "Provider" (\t -> Functions.defaultString t.provider)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Hospitalizations ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Hospitalization ID" (\t -> Functions.defaultIntToString t.hospitalizationId)
                    , stringColumn "Admin Date" (\t -> Functions.defaultDateTime t.dateOfAdmission)
                    , stringColumn "Discharge Date" (\t -> Functions.defaultDateTime t.dateOfDischarge)
                    , stringColumn "Service Type" (\t -> Functions.defaultString t.hospitalizationServiceType)
                    , stringColumn "Discharge Recommendations" (\t -> Functions.defaultString t.recommendations)
                    , stringColumn "Discharge Physician" (\t -> Functions.defaultString t.dischargePhysician)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Legal ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.CallRecordings ->
                    [ stringColumn "Date" (\t -> Functions.dateTime t.recordingDate)
                    , hrefColumn "Recording" "Open" (\t -> Functions.defaultString t.recording)
                    , hrefCustom
                    , checkColumn "During Enrollment" (\t -> t.enrollment)
                    , checkColumn "Consent" (\t -> t.hasVerbalConsent)
                    , stringColumn "User" (\t -> Functions.defaultString t.staffName)
                    ]

                Common.PreviousHistories ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "File Name" (\t -> Functions.defaultString t.fileName)
                    , stringColumn "Report Date" (\t -> Functions.defaultDate t.reportDate)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Enrollment ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Misc ->
                    commonColumns

        lastColumns =
            [ rowDropDownColumn recordType
            ]
    in
        List.append firstColumns lastColumns


rowDropDownColumn : Common.RecordType -> Table.Column RecordRow Msg
rowDropDownColumn recordType =
    Table.veryCustomColumn
        { name = ""
        , viewData = \t -> Common.Grid.rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropDownItems recordType t.id)
        , sorter = Table.unsortable
        }


config : Maybe Common.AddEditDataSource -> Common.RecordType -> Table.Config RecordRow Msg
config addEditDataSource recordType =
    let
        buttons =
            case addEditDataSource of
                Just _ ->
                    [ ( "e-addnew", onClick Add ) ]

                Nothing ->
                    []
    in
        Table.customConfig
            { toId = \t -> toString t.id
            , toMsg = SetTableState
            , columns = getColumns recordType
            , customizations =
                { defaultCustomizations
                    | tableAttrs = Common.Grid.standardTableAttrs "RecordTable"
                    , thead = Common.Grid.standardTheadNoFilters
                    , theadButtons = buttons
                }
            }


hrefCustom : Table.Column RecordRow Msg
hrefCustom =
    Table.veryCustomColumn
        { name = "Task"
        , viewData = \t -> hrefCustomDetails t.taskId t.taskTitle
        , sorter = Table.unsortable
        }


hrefCustomDetails : Maybe Int -> Maybe String -> Table.HtmlDetails Msg
hrefCustomDetails taskId taskTitle =
    Table.HtmlDetails []
        [ case ( taskId, taskTitle ) of
            ( Just t, Just y ) ->
                div [ class "RecordTableHref", onClick (EditTask t) ] [ text y ]

            _ ->
                div [] []
        ]


dropDownItems : Common.RecordType -> Int -> List ( String, String, Html.Attribute Msg )
dropDownItems recordType rowId =
    case recordType of
        Common.CallRecordings ->
            [ ( "e-edit", "Mark As Consent", onClick (SendMenuMessage rowId recordType "MarkAsConsent") ) ]

        _ ->
            [ ( "e-sync", "Transfer", onClick (SendMenuMessage rowId recordType "Transfer") )
            , ( "e-download", "View File", onClick (SendMenuMessage rowId recordType "ViewFile") )
            , ( "e-mail", "Send By Email", onClick (SendMenuMessage rowId recordType "SendByEmail") )
            , ( "e-print_01", "Send By Fax", onClick (SendMenuMessage rowId recordType "SendByFax") )
            , ( "e-save", "Save To Client Portal", onClick (SendMenuMessage rowId recordType "SaveToClientPortal") )
            , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId recordType "Delete") )
            ]


decodeRecordRow : Decoder RecordRow
decodeRecordRow =
    decode RecordRow
        |> required "Id" Decode.int
        |> required "Date" (maybe Decode.string)
        |> required "Specialty" (maybe Decode.string)
        |> required "Comments" (maybe Decode.string)
        |> required "TransferedTo" (maybe Decode.string)
        |> required "PatientId" Decode.int
        |> required "Title" (maybe Decode.string)
        |> required "DateAccessed" (maybe Decode.string)
        |> required "Provider" (maybe Decode.string)
        |> required "RecordType" (maybe Decode.string)
        |> required "DateOfAdmission" (maybe Decode.string)
        |> required "DateOfDischarge" (maybe Decode.string)
        |> required "DischargePhysician" (maybe Decode.string)
        |> required "DischargeDiagnosis" (maybe Decode.string)
        |> required "HospitalizationServiceType" (maybe Decode.string)
        |> required "HospitalizationId" (maybe Decode.int)
        |> required "ReportDate" (maybe Decode.string)
        |> required "FileName" (maybe Decode.string)
        |> required "Recommendations" (maybe Decode.string)
        |> required "TaskId" (maybe Decode.int)
        |> required "TaskTitle" (maybe Decode.string)
        |> required "Recording" (maybe Decode.string)
        |> required "RecordingDate" Decode.string
        |> required "RecordingDuration" Decode.int
        |> required "Enrollment" Decode.bool
        |> required "StaffId" Decode.int
        |> required "StaffName" (maybe Decode.string)
        |> required "HasVerbalConsent" Decode.bool
        |> hardcoded False


getRecords : Common.RecordType -> Int -> Http.Request (List RecordRow)
getRecords recordType patientId =
    let
        recordTypeId =
            Functions.getId recordType

        url =
            "/People/PatientRecordsGrid?patientId=" ++ toString patientId ++ "&recordTypeId=" ++ toString recordTypeId
    in
        Decode.field "list" (Decode.list decodeRecordRow)
            |> Http.get url


deleteRequest : a -> (Result Http.Error String -> msg) -> Cmd msg
deleteRequest rowId deleteCompleted =
    Http.send deleteCompleted <| Http.getString ("/People/DeleteRecord?recordId=" ++ toString rowId)


type alias RecordRow =
    { id : Int
    , date : Maybe String
    , specialty : Maybe String
    , comments : Maybe String
    , transferedTo : Maybe String
    , patientId : Int
    , title : Maybe String
    , dateAccessed : Maybe String
    , provider : Maybe String
    , recordType : Maybe String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dischargePhysician : Maybe String
    , dischargeDiagnosis : Maybe String
    , hospitalizationServiceType : Maybe String
    , hospitalizationId : Maybe Int
    , reportDate : Maybe String
    , fileName : Maybe String
    , recommendations : Maybe String
    , taskId : Maybe Int
    , taskTitle : Maybe String
    , recording : Maybe String
    , recordingDate : String
    , recordingDuration : Int
    , enrollment : Bool
    , staffId : Int
    , staffName : Maybe String
    , hasVerbalConsent : Bool
    , dropDownOpen : Bool
    }


getMenuMessage : List RecordRow -> Common.RecordType -> Int -> String -> Common.MenuMessage
getMenuMessage rows recordType recordId messageType =
    let
        maybeVerbalConsent =
            rows
                |> List.filter (\t -> t.id == recordId)
                |> List.head
                |> Maybe.map (\t -> not t.hasVerbalConsent)

        recordTypeId =
            Just <| Functions.getId recordType
    in
        Common.MenuMessage messageType recordId recordTypeId maybeVerbalConsent


flipConsent : List RecordRow -> Int -> Common.RecordType -> List RecordRow
flipConsent rows recordId recordType =
    case recordType of
        Common.CallRecordings ->
            rows
                |> List.map
                    (\t ->
                        if t.id == recordId then
                            { t | hasVerbalConsent = not t.hasVerbalConsent }
                        else
                            t
                    )

        _ ->
            rows


flipDropDownOpen : List RecordRow -> Int -> List RecordRow
flipDropDownOpen rows recordId =
    rows
        |> List.map
            (\t ->
                if t.id == recordId then
                    { t | dropDownOpen = not t.dropDownOpen }
                else
                    { t | dropDownOpen = False }
            )

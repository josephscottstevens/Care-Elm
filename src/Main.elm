module Main exposing (main)

import Html exposing (Html, text, div)
import ClinicalSummary
import Records
import RecordAddNew.Main as RecordAddNew
import PastMedicalHistory
import Hospitilizations
import HospitilizationsAddEdit.Main as HospitilizationsAddEdit
import Billing.Types
import HospitilizationsAddEdit.Types
import RecordAddNew.Types
import Common.Functions as Functions
import Common.Types exposing (AddEditDataSource)
import Common.Route as Route exposing (Route)
import Navigation exposing (Location)
import Http exposing (Error)
import Json.Decode exposing (maybe, int, list)
import Json.Decode.Pipeline exposing (required, decode)


type alias Model =
    { patientId : Int
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    , location : Location
    }


type Page
    = None
    | Billing
    | ClinicalSummary ClinicalSummary.Model
    | Records Records.Model
    | RecordAddNew RecordAddNew.Types.Model
    | PastMedicalHistory PastMedicalHistory.Model
    | Hospitilizations Hospitilizations.Model
    | HospitilizationsAddEdit HospitilizationsAddEdit.Types.Model
    | Error String


init : Location -> ( Model, Cmd Msg )
init location =
    case Route.getPatientId location of
        Just patientId ->
            setRoute (Route.fromLocation location)
                { patientId = patientId
                , page = None
                , addEditDataSource = Nothing
                , location = location
                }

        Nothing ->
            { patientId = 0
            , page = Error "Cannot load page without patientId"
            , addEditDataSource = Nothing
            , location = location
            }
                ! [ Functions.setLoadingStatus False ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model.page
        ]


view : Model -> Html Msg
view model =
    case model.page of
        None ->
            div [] [ text "no view here" ]

        Billing ->
            div [] []

        ClinicalSummary subModel ->
            Html.map ClinicalSummaryMsg (ClinicalSummary.view subModel model.patientId)

        Records subModel ->
            Html.map RecordsMsg (Records.view subModel model.addEditDataSource)

        RecordAddNew subModel ->
            Html.map RecordAddNewMsg (RecordAddNew.view subModel)

        PastMedicalHistory subModel ->
            Html.map PastMedicalHistoryMsg (PastMedicalHistory.view subModel model.addEditDataSource)

        Hospitilizations subModel ->
            Html.map HospitilizationsMsg (Hospitilizations.view subModel model.addEditDataSource)

        HospitilizationsAddEdit subModel ->
            Html.map HospitilizationsAddEditMsg (HospitilizationsAddEdit.view subModel)

        Error str ->
            div [] [ text str ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        None ->
            Sub.none

        Billing ->
            Sub.none

        ClinicalSummary _ ->
            Sub.map ClinicalSummaryMsg ClinicalSummary.subscriptions

        Records t ->
            Sub.map RecordsMsg (Records.subscriptions t.rows)

        RecordAddNew _ ->
            Sub.map RecordAddNewMsg RecordAddNew.subscriptions

        PastMedicalHistory _ ->
            Sub.map PastMedicalHistoryMsg PastMedicalHistory.subscriptions

        Hospitilizations t ->
            Sub.map HospitilizationsMsg (Hospitilizations.subscriptions t.rows)

        HospitilizationsAddEdit _ ->
            Sub.map HospitilizationsAddEditMsg HospitilizationsAddEdit.subscriptions

        Error _ ->
            Sub.none



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | BillingMsg Billing.Types.Msg
    | ClinicalSummaryMsg ClinicalSummary.Msg
    | RecordsMsg Records.Msg
    | RecordAddNewMsg RecordAddNew.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | PastMedicalHistoryMsg PastMedicalHistory.Msg
    | HospitilizationsMsg Hospitilizations.Msg
    | HospitilizationsAddEditMsg HospitilizationsAddEdit.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        getDropdownsCmd =
            case model.addEditDataSource of
                Just _ ->
                    Cmd.none

                Nothing ->
                    getDropDowns model.patientId AddEditDataSourceLoaded

        cmds t =
            [ getDropdownsCmd, Functions.setLoadingStatus False ] ++ t
    in
        case maybeRoute of
            Just Route.ClinicalSummary ->
                { model | page = ClinicalSummary ClinicalSummary.emptyModel }
                    ! cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ]

            Just Route.Hospitilizations ->
                { model | page = Hospitilizations Hospitilizations.emptyModel }
                    ! cmds [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ]

            Just Route.PastMedicalHistory ->
                case model.addEditDataSource of
                    Just addEditDataSource ->
                        { model | page = PastMedicalHistory PastMedicalHistory.emptyModel }
                            ! cmds [ Cmd.map PastMedicalHistoryMsg (PastMedicalHistory.init addEditDataSource model.patientId) ]

                    Nothing ->
                        Debug.crash "yikes"

            Just Route.HospitilizationsAdd ->
                case model.addEditDataSource of
                    Just t ->
                        { model | page = HospitilizationsAddEdit (HospitilizationsAddEdit.Types.emptyModel Nothing) }
                            ! cmds [ Cmd.map HospitilizationsAddEditMsg (HospitilizationsAddEdit.init t Nothing) ]

                    Nothing ->
                        -- aka, if user refreshes on the add screen, can't do much since there is no data source for dropdowns
                        { model | page = Hospitilizations Hospitilizations.emptyModel }
                            ! cmds [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ]

            Just (Route.HospitilizationsEdit rowId) ->
                let
                    x =
                        case model.page of
                            Hospitilizations hospModel ->
                                hospModel.rows
                                    |> List.filter (\t -> t.id == rowId)
                                    |> List.head

                            _ ->
                                Debug.crash "invalid hosptilization edit state"
                in
                    case model.addEditDataSource of
                        Just t ->
                            { model
                                | page = HospitilizationsAddEdit (HospitilizationsAddEdit.Types.emptyModel x)
                            }
                                ! cmds [ Cmd.map HospitilizationsAddEditMsg (HospitilizationsAddEdit.init t x) ]

                        Nothing ->
                            model ! [ getDropDowns model.patientId AddEditDataSourceLoaded ]

            Just (Route.Records t) ->
                { model | page = Records (Records.emptyModel t) }
                    ! cmds [ Cmd.map RecordsMsg (Records.init t model.patientId) ]

            Just (Route.RecordAddNew t) ->
                case model.addEditDataSource of
                    Just addEditDataSource ->
                        { model | page = RecordAddNew (RecordAddNew.Types.emptyModel t addEditDataSource) }
                            ! cmds [ Cmd.map RecordAddNewMsg (RecordAddNew.init addEditDataSource t) ]

                    Nothing ->
                        -- aka, if user refreshes on the add\edit screen, can't do much since there is no data source for dropdowns
                        { model | page = Records (Records.emptyModel t) }
                            ! cmds [ Cmd.map RecordsMsg (Records.init t model.patientId) ]

            Nothing ->
                { model | page = Error "no route provided" } ! []

            _ ->
                { model | page = Error "unknown page" } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel model.patientId
            in
                { model | page = toModel newModel } ! [ Cmd.map toMsg newCmd ]
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( AddEditDataSourceLoaded response, _ ) ->
                case response of
                    Ok t ->
                        { model | addEditDataSource = Just t } ! []

                    Err t ->
                        { model | page = Error (toString t) } ! []

            ( PastMedicalHistoryMsg subMsg, PastMedicalHistory subModel ) ->
                toPage PastMedicalHistory PastMedicalHistoryMsg PastMedicalHistory.update subMsg subModel

            ( HospitilizationsMsg subMsg, Hospitilizations subModel ) ->
                toPage Hospitilizations HospitilizationsMsg Hospitilizations.update subMsg subModel

            ( HospitilizationsAddEditMsg subMsg, HospitilizationsAddEdit subModel ) ->
                toPage HospitilizationsAddEdit HospitilizationsAddEditMsg HospitilizationsAddEdit.update subMsg subModel

            ( ClinicalSummaryMsg subMsg, ClinicalSummary subModel ) ->
                toPage ClinicalSummary ClinicalSummaryMsg ClinicalSummary.update subMsg subModel

            ( RecordsMsg subMsg, Records subModel ) ->
                toPage Records RecordsMsg Records.update subMsg subModel

            ( RecordAddNewMsg subMsg, RecordAddNew subModel ) ->
                toPage RecordAddNew RecordAddNewMsg RecordAddNew.update subMsg subModel

            _ ->
                { model | page = Error <| "Missing Page\\Message " ++ toString page ++ " !!!__-__!!! " ++ toString msg } ! []


getDropDowns : Int -> (Result Http.Error AddEditDataSource -> msg) -> Cmd msg
getDropDowns patientId t =
    decode AddEditDataSource
        |> required "facilityId" (maybe int)
        |> required "patientId" int
        |> required "facilityDropdown" (list Functions.decodeDropdownItem)
        |> required "providersDropdown" (list Functions.decodeDropdownItem)
        |> required "recordTypeDropdown" (list Functions.decodeDropdownItem)
        |> required "userDropDown" (list Functions.decodeDropdownItem)
        |> required "taskDropDown" (list Functions.decodeDropdownItem)
        |> required "hospitilizationServiceTypeDropdown" (list Functions.decodeDropdownItem)
        |> required "hospitalizationDischargePhysicianDropdown" (list Functions.decodeDropdownItem)
        |> required "hospitilizations" (list Functions.decodeDropdownItem)
        |> Http.get ("/People/PatientRecordsDropdowns?patientId=" ++ toString patientId)
        |> Http.send t


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module Main exposing (..)

import Html exposing (Html, text, div)
import ClinicalSummary
import Records.Main as Records
import RecordAddNew.Main as RecordAddNew
import Hospitilizations.Main as Hospitilizations
import HospitilizationsAddEdit.Main as HospitilizationsAddEdit
import Billing.Types
import Hospitilizations.Types
import HospitilizationsAddEdit.Types
import Records.Types
import RecordAddNew.Types
import Common.Functions exposing (..)
import Common.Types exposing (..)
import Functions exposing (..)
import Navigation exposing (Location)
import Route exposing (Route)
import Http exposing (Error)
import Task
import Ports exposing (clinicalSummaryInit)


type alias Model =
    { patientId : Int
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    }


type Page
    = None
    | Billing
    | ClinicalSummary ClinicalSummary.Model
    | Records Records.Types.Model
    | RecordAddNew RecordAddNew.Types.Model
    | Hospitilizations Hospitilizations.Types.Model
    | HospitilizationsAddEdit HospitilizationsAddEdit.Types.Model
    | Error String


init : Location -> ( Model, Cmd Msg )
init location =
    let
        patientId =
            Route.getPatientId location.search
    in
        case patientId of
            Just t ->
                setRoute (Route.fromLocation location)
                    { patientId = t
                    , page = None
                    , addEditDataSource = Nothing
                    }

            Nothing ->
                { patientId = 0
                , page = Error "Cannot load page without patientId"
                , addEditDataSource = Nothing
                }
                    ! [ setLoadingStatus False ]


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
            Sub.none

        Records _ ->
            Sub.map RecordsMsg Records.subscriptions

        RecordAddNew _ ->
            Sub.map RecordAddNewMsg RecordAddNew.subscriptions

        Hospitilizations _ ->
            Sub.map HospitilizationsMsg Hospitilizations.subscriptions

        HospitilizationsAddEdit _ ->
            Sub.map HospitilizationsAddEditMsg HospitilizationsAddEdit.subscriptions

        Error t ->
            Sub.none



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | BillingMsg Billing.Types.Msg
    | ClinicalSummaryMsg ClinicalSummary.Msg
    | ClinicalSummaryLoaded (Result Http.Error ClinicalSummary.Model)
    | RecordsMsg Records.Types.Msg
    | RecordsLoaded (Result Http.Error Records.Types.Model)
    | RecordAddNewMsg RecordAddNew.Types.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | HospitilizationsMsg Hospitilizations.Types.Msg
    | HospitilizationsAddEditMsg HospitilizationsAddEdit.Types.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        extraCmd =
            case model.addEditDataSource of
                Just _ ->
                    Cmd.none

                Nothing ->
                    getDropDowns model.patientId AddEditDataSourceLoaded

        transition toMsg task =
            [ Task.attempt toMsg task, setLoadingStatus False, extraCmd ]
    in
        case maybeRoute of
            Just Route.ClinicalSummary ->
                { model | page = ClinicalSummary ClinicalSummary.emptyModel }
                    ! (clinicalSummaryInit (SomeDropDowns monthDropdown yearDropdown)
                        :: transition ClinicalSummaryLoaded (ClinicalSummary.init model.patientId)
                      )

            Just Route.Hospitilizations ->
                { model | page = Hospitilizations (Hospitilizations.Types.emptyModel model.patientId) }
                    ! [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ]

            Just (Route.Records t) ->
                { model | page = Records (Records.Types.emptyModel t model.patientId) }
                    ! transition RecordsLoaded (Records.init t model.patientId)

            Just (Route.RecordAddNew t) ->
                { model | page = RecordAddNew (RecordAddNew.Types.emptyModel t model.addEditDataSource model.patientId) }
                    ! case model.addEditDataSource of
                        Just addEditDataSource ->
                            [ Cmd.map RecordAddNewMsg (RecordAddNew.init addEditDataSource t) ]

                        Nothing ->
                            [ getDropDowns model.patientId AddEditDataSourceLoaded ]

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
                    subUpdate subMsg subModel
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

            ( HospitilizationsMsg subMsg, Hospitilizations subModel ) ->
                toPage Hospitilizations HospitilizationsMsg Hospitilizations.update subMsg subModel

            ( HospitilizationsAddEditMsg subMsg, HospitilizationsAddEdit subModel ) ->
                toPage HospitilizationsAddEdit HospitilizationsAddEditMsg HospitilizationsAddEdit.update subMsg subModel

            ( ClinicalSummaryMsg subMsg, ClinicalSummary subModel ) ->
                toPage ClinicalSummary ClinicalSummaryMsg ClinicalSummary.update subMsg subModel

            ( ClinicalSummaryLoaded (Err err), _ ) ->
                { model | page = Error (toString err) } ! []

            ( ClinicalSummaryLoaded (Ok subModel), _ ) ->
                { model | page = ClinicalSummary subModel } ! []

            ( RecordsLoaded (Ok subModel), _ ) ->
                { model | page = Records subModel } ! []

            ( RecordsLoaded (Err err), _ ) ->
                { model | page = Error (toString err) } ! []

            ( RecordsMsg subMsg, Records subModel ) ->
                toPage Records RecordsMsg Records.update subMsg subModel

            ( RecordAddNewMsg subMsg, RecordAddNew subModel ) ->
                toPage RecordAddNew RecordAddNewMsg RecordAddNew.update subMsg subModel

            _ ->
                { model | page = Error <| "Missing Page Message" ++ toString msg ++ " - " ++ (toString page) } ! []


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

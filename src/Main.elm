module Main exposing (..)

import Html exposing (Html, text, div)
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


type alias Model =
    { patientId : Int
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    }


type Page
    = None
    | Billing
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
            Just (Route.Records t) ->
                { model | page = Records (Records.Types.emptyModel t model.patientId) }
                    ! transition RecordsLoaded (Records.init t model.patientId)

            Just (Route.RecordAddNew t) ->
                { model | page = RecordAddNew (RecordAddNew.Types.emptyModel t model.addEditDataSource model.patientId) }
                    ! [ Cmd.map RecordAddNewMsg (RecordAddNew.init model.addEditDataSource t) ]

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

            ( RecordsLoaded (Ok subModel), _ ) ->
                { model | page = Records subModel } ! []

            ( RecordsLoaded (Err err), _ ) ->
                { model | page = Error (toString err) } ! []

            ( RecordsMsg subMsg, Records subModel ) ->
                toPage Records RecordsMsg Records.update subMsg subModel

            ( RecordAddNewMsg msg, RecordAddNew subModel ) ->
                { model | page = RecordAddNew subModel } ! []

            _ ->
                { model | page = Error <| toString msg ++ " - " ++ toString page } ! []


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

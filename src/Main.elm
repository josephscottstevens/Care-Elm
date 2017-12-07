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
import Ports exposing (..)
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
    | Records RecordType Records.Types.Model
    | RecordAddNew RecordType RecordAddNew.Types.Model
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

            --! [ getDropDowns t AddEditDataSourceLoaded ]
            Nothing ->
                { patientId = 0
                , page = Error "Cannot load page without patientId"
                , addEditDataSource = Nothing
                }
                    ! [ setLoadingStatus False ]


subscriptions : Model -> Sub Msg
subscriptions _ =
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

        Records recordType subModel ->
            Html.map RecordsMsg (Records.view subModel recordType model.addEditDataSource)

        RecordAddNew recordType subModel ->
            Html.map RecordAddNewMsg (RecordAddNew.view subModel recordType)

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

        Records _ _ ->
            Sub.map RecordsMsg Records.subscriptions

        RecordAddNew _ _ ->
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
      -- | UrlChange Navigation.Location
    | KnockoutUrlChange String


model : Model
model =
    Model 0 None Nothing


transition : (Result x a -> msg) -> Task.Task x a -> ( Model, Cmd msg )
transition toMsg task =
    ( model, Task.attempt toMsg task )


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
            model ! [ Task.attempt toMsg task, setLoadingStatus False, extraCmd ]
    in
        case maybeRoute of
            Just (Route.Records PrimaryCare) ->
                transition RecordsLoaded (Records.init PrimaryCare model.patientId)

            Nothing ->
                { model | page = Error "no route provided" } ! []

            _ ->
                { model | page = Error "unknown page" } ! []



-- pageErrored : Model -> Page -> String -> ( Model, Cmd msg )
-- pageErrored model activePage errorMessage =
--     let
--         error =
--             Errored.pageLoadError activePage errorMessage
--     in
--         { model | pageState = Error error } => Cmd.none


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

        -- errored =
        --     pageErrored model
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
                { model | page = Records PrimaryCare subModel } ! []

            ( RecordsLoaded (Err err), _ ) ->
                { model | page = Error (toString err) } ! []

            ( KnockoutUrlChange location, _ ) ->
                model ! []

            _ ->
                model ! []



-- (Page.Billing ->
--     newModel ! [ displayErrorMessage "Billing Not implemented" ]
-- (Page.Records recordType ->
--     newModel ! [ Cmd.map RecordsMsg (Records.init recordType model.patientId) ]
-- (Page.RecordAddNew recordType ->
--     let
--         ( t, cmd ) =
--             RecordAddNew.init model.addEditDataSource recordType
--     in
--         { newModel | recordAddNewState = t } ! [ Cmd.map RecordAddNewMsg cmd ]
-- (Page.Hospitilizations ->
--     Page ! [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ]
-- (Page.HospitilizationsAddEdit hospitilizationId ->
--     let
--         hospitilizationsRow =
--             getHospitilizationsRow model.hospitalizationsState.hospitilizations hospitilizationId
--         ( t, cmd ) =
--             HospitilizationsAddEdit.init model.addEditDataSource hospitilizationsRow model.patientId
--     in
--         { newModel | hospitilizationsAddEditState = t } ! [ Cmd.map HospitilizationsAddEditMsg cmd ]
-- (Page.None ->
--     newModel ! []
-- (Page.Error t ->
--     newModel ! [ displayErrorMessage t ]


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

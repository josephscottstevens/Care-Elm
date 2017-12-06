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


type Page
    = None
    | Billing
    | Records RecordType
    | RecordAddNew RecordType
    | Hospitilizations
    | HospitilizationsAddEdit (Maybe Int)
    | Error String


type alias Model =
    { patientId : Int
    , page : Page
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        patientId =
            Route.getPatientId location.search

        model =
            Model (defaultInt patientId) None
    in
        case patientId of
            Just t ->
                model ! [ getDropDowns t AddEditDataSourceLoaded, setLoadingStatus False ]

            Nothing ->
                { model | page = Route.Error "Cannot load page without patientId" } ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map RecordsMsg Records.subscriptions
        , Sub.map RecordAddNewMsg RecordAddNew.subscriptions
        , Sub.map HospitilizationsMsg Hospitilizations.subscriptions
        , Sub.map HospitilizationsAddEditMsg HospitilizationsAddEdit.subscriptions
        , isApp KnockoutUrlChange
        ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    case model.page of
        Route.None ->
            div [] []

        Route.Billing ->
            div [] []

        Route.Records recordType ->
            Html.map RecordsMsg (Records.view model.recordsState recordType model.addEditDataSource)

        Route.RecordAddNew recordType ->
            Html.map RecordAddNewMsg (RecordAddNew.view model.recordAddNewState recordType)

        Route.Hospitilizations ->
            Html.map HospitilizationsMsg (Hospitilizations.view model.hospitalizationsState model.addEditDataSource)

        Route.HospitilizationsAdd ->
            Html.map HospitilizationsAddEditMsg (HospitilizationsAddEdit.view model.hospitilizationsAddEditState)

        Route.Error str ->
            div [] [ text str ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Route.Billing ->
            Sub.none

        Route.Records recordType ->
            Records.subscriptions

        Route.RecordAddNew recordType ->
            RecordAddNew.subscriptions

        Route.Hospitilizations ->
            Hospitilizations.subscriptions

        Route.HospitilizationsAdd ->
            HospitilizationsAddEdit.subscriptions

        Route.HospitilizationsEdit hospitilizationId ->
            HospitilizationsAddEdit.subscriptions

        Route.None ->
            Sub.none

        Route.Error t ->
            Sub.none



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | BillingMsg Billing.Types.Msg
    | RecordsMsg Records.Types.Msg
    | RecordAddNewMsg RecordAddNew.Types.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | HospitilizationsMsg Hospitilizations.Types.Msg
    | HospitilizationsAddEditMsg HospitilizationsAddEdit.Types.Msg
    | UrlChange Navigation.Location
    | KnockoutUrlChange String


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
        { model | pageState = Loaded (Errored error) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = toModel newModel }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            Page.Billing ->
                newModel ! [ displayErrorMessage "Billing Not implemented" ]

            Page.Records recordType ->
                newModel ! [ Cmd.map RecordsMsg (Records.init recordType model.patientId) ]

            Page.RecordAddNew recordType ->
                let
                    ( t, cmd ) =
                        RecordAddNew.init model.addEditDataSource recordType
                in
                    { newModel | recordAddNewState = t } ! [ Cmd.map RecordAddNewMsg cmd ]

            Route.Hospitilizations ->
                Page ! [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ]

            Page.HospitilizationsAddEdit hospitilizationId ->
                let
                    hospitilizationsRow =
                        getHospitilizationsRow model.hospitalizationsState.hospitilizations hospitilizationId

                    ( t, cmd ) =
                        HospitilizationsAddEdit.init model.addEditDataSource hospitilizationsRow model.patientId
                in
                    { newModel | hospitilizationsAddEditState = t } ! [ Cmd.map HospitilizationsAddEditMsg cmd ]

            Route.None ->
                newModel ! []

            Route.Error t ->
                newModel ! [ displayErrorMessage t ]

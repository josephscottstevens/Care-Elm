module Main exposing (..)

import Model exposing (..)
import Html exposing (Html, text, div)
import Records.Main as Records
import RecordAddNew.Main as RecordAddNew
import Hospitilizations.Main as Hospitilizations
import HospitilizationsAddEdit.Main as HospitilizationsAddEdit
import Common.Functions exposing (..)
import Common.Types exposing (..)
import Functions exposing (..)
import Ports exposing (..)
import Navigation
import Common.Routes as Routes


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map RecordsMsg Records.subscriptions
        , Sub.map RecordAddNewMsg RecordAddNew.subscriptions
        , presetPageComplete PresetPageComplete
        , setPageComplete SetPageComplete
        , isApp IsApp
        ]


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            emptyModel location

        patientId =
            Routes.getPatientId location.href
    in
        model ! ([ getDropDowns patientId AddEditDataSourceLoaded, setLoadingStatus False ])


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
        None ->
            div [] []

        Billing ->
            div [] []

        Records recordType ->
            Html.map RecordsMsg (Records.view model.recordsState recordType model.addEditDataSource)

        RecordAddNew recordType ->
            Html.map RecordAddNewMsg (RecordAddNew.view model.recordAddNewState recordType)

        Hospitilizations ->
            Html.map HospitilizationsMsg (Hospitilizations.view model.hospitalizationsState model.addEditDataSource)

        HospitilizationsAddEdit ->
            Html.map HospitilizationsAddEditMsg (HospitilizationsAddEdit.view model.hospitilizationsAddEditState)

        Error str ->
            div [] [ text str ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        BillingMsg billingMsg ->
            model ! []

        RecordsMsg recordsMsg ->
            let
                ( newModel, pageCmd ) =
                    Records.update recordsMsg model.recordsState
            in
                { model | recordsState = newModel } ! [ Cmd.map RecordsMsg pageCmd ]

        RecordAddNewMsg recordAddNewMsg ->
            let
                ( newModel, pageCmd ) =
                    RecordAddNew.update recordAddNewMsg model.recordAddNewState
            in
                { model | recordAddNewState = newModel } ! [ Cmd.map RecordAddNewMsg pageCmd ]

        HospitilizationsMsg hospitilizationsMsg ->
            let
                ( newModel, pageCmd ) =
                    Hospitilizations.update hospitilizationsMsg model.hospitalizationsState
            in
                { model | hospitalizationsState = newModel } ! [ Cmd.map HospitilizationsMsg pageCmd ]

        HospitilizationsAddEditMsg hospitilizationsAddEditMsg ->
            let
                ( newModel, pageCmd ) =
                    HospitilizationsAddEdit.update hospitilizationsAddEditMsg model.hospitilizationsAddEditState
            in
                { model | hospitilizationsAddEditState = newModel } ! [ Cmd.map HospitilizationsAddEditMsg pageCmd ]

        AddEditDataSourceLoaded (Ok t) ->
            let
                newState =
                    model.recordAddNewState

                tt =
                    { newState | facilityId = t.facilityId }
            in
                { model | addEditDataSource = Just t, recordAddNewState = tt } ! []

        AddEditDataSourceLoaded (Err httpError) ->
            { model | page = Error (toString httpError) } ! []

        PresetPageComplete pageStr ->
            model ! []

        SetPageComplete _ ->
            model ! [ setLoadingStatus False ]

        UrlChange url ->
            { model | currentUrl = url } ! []

        IsApp urlHash ->
            let
                patientId =
                    Routes.getPatientId urlHash

                newPage =
                    Routes.getPage urlHash

                commands =
                    case newPage of
                        Records recordType ->
                            [ Cmd.map RecordsMsg (Records.init recordType patientId)
                            , decideApp True
                            ]

                        _ ->
                            [ decideApp False ]
            in
                { model | page = newPage } ! commands

module Main exposing (..)

import Model exposing (..)
import Html exposing (text, div)
import Records.Main as Records
import RecordAddNew.Main as RecordAddNew
import Hospitilizations.Main as Hospitilizations
import Common.Functions exposing (..)
import Common.Types exposing (..)
import Functions exposing (..)
import Ports exposing (..)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map RecordsMsg Records.subscriptions
        , Sub.map RecordAddNewMsg RecordAddNew.subscriptions
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel flags
    in
        if flags.pageFlag == "billing" then
            { model | page = Billing } ! []
        else if flags.pageFlag == "records" then
            { model | page = Records }
                ! [ Cmd.map RecordsMsg (Records.init flags)
                  , getDropDowns flags.patientId AddEditDataSourceLoaded
                  ]
        else if flags.pageFlag == "hospitilizations" then
            { model | page = Hospitilizations }
                ! [ Cmd.map HospitilizationsMsg (Hospitilizations.init flags)
                  , getDropDowns flags.patientId AddEditDataSourceLoaded
                  ]
        else
            ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html.Html Msg
view model =
    case model.page of
        None ->
            div [] []

        Billing ->
            div [] []

        Records ->
            Html.map RecordsMsg (Records.view model.recordsState model.addEditDataSource)

        RecordAddNew _ ->
            Html.map RecordAddNewMsg (RecordAddNew.view model.recordAddNewState)

        Hospitilizations ->
            Html.map HospitilizationsMsg (Hospitilizations.view model.hospitalizationsState model.addEditDataSource)

        Error str ->
            div [] [ text str ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    let
        handle maybePage t =
            case maybePage of
                Just page ->
                    case page of
                        RecordAddNew recordTypeId ->
                            case model.addEditDataSource of
                                Just addEditDataSource ->
                                    { model | page = None } ! [ initRecordAddNew (getSyncfusionMessage addEditDataSource recordTypeId False False) ]

                                Nothing ->
                                    model ! []

                        _ ->
                            model ! []

                Nothing ->
                    t
    in
        case msg of
            BillingMsg billingMsg ->
                model ! []

            RecordsMsg recordsMsg ->
                let
                    ( ( newModel, pageCmd ), nextPage ) =
                        Records.update recordsMsg model.recordsState
                in
                    -- case nextPage of
                    --     Just t ->
                    --         { model
                    --             | page = RecordAddNew Nothing
                    --             , recordAddNewState = Just (RecordAddNewTypes.emptyModel t.facilityId model.flags)
                    --         }
                    --             ! [ Cmd.map RecordAddNewMsg (RecordAddNew.init model.flags t) ]
                    --     Nothing ->
                    { model | recordsState = newModel } ! [ Cmd.map RecordsMsg pageCmd ]

            RecordAddNewMsg recordAddNewMsg ->
                -- case model.recordAddNewState of
                --     Just t ->
                let
                    ( ( newModel, pageCmd ), isDone ) =
                        RecordAddNew.update recordAddNewMsg model.recordAddNewState
                in
                    -- case isDone of
                    --     True ->
                    --         { model | page = Records } ! [ Cmd.map RecordsMsg (Records.init model.flags) ]
                    --     False ->
                    { model | recordAddNewState = newModel } ! [ Cmd.map RecordAddNewMsg pageCmd ]

            -- Nothing ->
            --     { model | page = Error "Can't display this page without a datasource" } ! []
            HospitilizationsMsg hospitilizationsMsg ->
                let
                    ( ( newModel, pageCmd ), isDone ) =
                        Hospitilizations.update hospitilizationsMsg model.hospitalizationsState
                in
                    { model | hospitalizationsState = newModel } ! [ Cmd.map HospitilizationsMsg pageCmd ]

            AddEditDataSourceLoaded (Ok t) ->
                { model | addEditDataSource = Just t } ! []

            AddEditDataSourceLoaded (Err httpError) ->
                { model | page = Error (toString httpError) } ! [ setLoadingStatus False ]

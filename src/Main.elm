port module Main exposing (..)

import Model exposing (..)
import Html exposing (text, div, button)
import Billing.Types as BillingTypes
import Records.Main as Records
import Records.Types as RecordTypes
import RecordAddNew.Main as RecordAddNew
import Utils.CommonFunctions exposing (..)
import Utils.CommonTypes exposing (..)
import Functions exposing (..)
import Html.Attributes exposing (class, type_)


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
            { model | page = BillingPage } ! []
        else if flags.pageFlag == "records" then
            { model | page = RecordsPage }
                ! [ Cmd.map RecordsMsg (Records.init flags)
                  , getDropDowns flags.recordType flags.patientId AddEditDataSourceLoaded
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
        NoPage ->
            div [] []

        BillingPage ->
            div [] []

        RecordsPage ->
            Html.map RecordsMsg (Records.view model.recordsState model.addEditDataSource)

        RecordAddNewPage ->
            Html.map RecordAddNewMsg (RecordAddNew.view model.recordAddNewState)

        Error str ->
            div [] [ text str ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        BillingMsg billingMsg ->
            model ! []

        RecordsMsg recordsMsg ->
            let
                ( ( newModel, pageCmd ), addEditDataSource ) =
                    Records.update recordsMsg model.recordsState
            in
                case addEditDataSource of
                    Just t ->
                        { model | page = RecordAddNewPage } ! [ Cmd.map RecordAddNewMsg (RecordAddNew.init t) ]

                    Nothing ->
                        { model | recordsState = newModel } ! [ Cmd.map RecordsMsg pageCmd ]

        RecordAddNewMsg recordAddNewMsg ->
            let
                ( newModel, pageCmd ) =
                    RecordAddNew.update recordAddNewMsg model.recordAddNewState
            in
                { model | recordAddNewState = newModel } ! [ Cmd.map RecordAddNewMsg pageCmd ]

        AddEditDataSourceLoaded (Ok t) ->
            { model | addEditDataSource = Just t } ! []

        AddEditDataSourceLoaded (Err httpError) ->
            { model | page = Error (toString httpError) } ! [ setLoadingStatus False ]

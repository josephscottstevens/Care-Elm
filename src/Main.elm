port module Main exposing (..)

import Model exposing (..)
import Html exposing (div)
import Billing.Main as Billing
import Records.Main as Records
import RecordAddNew.Main as RecordAddNew
import Utils.CommonTypes exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onFocus)
import Table exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map RecordsMsg (Records.subscriptions model.recordsState)
        , Sub.map RecordAddNewMsg (RecordAddNew.subscriptions model.recordAddNewState)
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel flags
    in
        if flags.pageFlag == "billing" then
            ( { model | page = BillingPage }, Cmd.map BillingMsg Billing.init )
        else if flags.pageFlag == "records" then
            ( { model | page = RecordsPage }, Cmd.map RecordsMsg (Records.init flags) )
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
            div []
                [ button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]
                , Html.map RecordsMsg (Records.view model.recordsState)
                ]

        RecordAddNewPage ->
            div []
                [ button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]
                , Html.map RecordAddNewMsg (RecordAddNew.view model.recordAddNewState)
                ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        BillingMsg billingMsg ->
            model ! []

        -- [ Cmd.map BillingMsg billingMsg ]
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

        AddNewStart ->
            { model | page = NoPage } ! []

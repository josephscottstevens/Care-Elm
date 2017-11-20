port module Main exposing (..)

import Model exposing (..)
import Html exposing (div)
import Billing.Main as Billing
import Records.Main as Records
import Utils.CommonTypes exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onFocus)
import Table exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map RecordsMsg (Records.subscriptions model.recordsState)


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


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        BillingMsg billingMsg ->
            model ! []

        -- [ Cmd.map BillingMsg billingMsg ]
        RecordsMsg recordsMsg ->
            let
                ( newRecordModel, pageCmd ) =
                    Records.update recordsMsg model.recordsState
            in
                { model | recordsState = newRecordModel } ! [ Cmd.map RecordsMsg pageCmd ]

        AddNewStart ->
            { model | page = NoPage } ! []

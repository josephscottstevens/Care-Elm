port module Main exposing (..)

import Model exposing (..)
import Html exposing (div)
import Billing.Main as Billing
import Records.Main as Records
import Utils.CommonTypes exposing (..)


port openPage : (String -> msg) -> Sub msg


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
            Html.map BillingMsg (Billing.view model.billingState)

        RecordsPage ->
            Html.map RecordsMsg (Records.view model.recordsState)


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        BillingMsg billingMsg ->
            let
                ( newBillingModel, widgetCmd ) =
                    Billing.update billingMsg model.billingState
            in
                ( { model | billingState = newBillingModel }, Cmd.map BillingMsg widgetCmd )

        RecordsMsg recordsMsg ->
            let
                ( newRecordModel, widgetCmd ) =
                    Records.update recordsMsg model.recordsState
            in
                ( { model | recordsState = newRecordModel }, Cmd.map RecordsMsg widgetCmd )

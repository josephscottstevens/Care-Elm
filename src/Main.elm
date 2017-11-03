port module Main exposing (..)

import Model exposing (..)
import Html exposing (div, text)
import Billing.Main as Billing
import Records.Main as Records


port sendTestDate : String -> Cmd msg


port openPage : (String -> msg) -> Sub msg



-- port getTestDate : (String -> msg) -> Sub msg
-- getTestDate UpdateStartDate


type alias Flags =
    { pageFlag : String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Flags -> ( Model, Cmd Msg )
init flags =
    if flags.pageFlag == "billing" then
        ( { emptyModel | page = BillingPage }, Cmd.map BillingMsg Billing.init )
    else if flags.pageFlag == "records" then
        ( { emptyModel | page = RecordsPage }, Cmd.map RecordsMsg Records.init )
    else
        ( emptyModel, Cmd.none )


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
            ( { model | billingState = Billing.update billingMsg model.billingState }, Cmd.none )

        RecordsMsg recordsMsg ->
            ( { model | recordsState = Records.update recordsMsg model.recordsState }, Cmd.none )

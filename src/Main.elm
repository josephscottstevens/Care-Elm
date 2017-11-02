port module Main exposing (..)

import Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Table
import Utils.GridPaging exposing (..)
import Utils.CommonGrid exposing (..)
import Billing.Main
import Billing.Types
import Billing.Load
import Http


port sendTestDate : String -> Cmd msg


port getTestDate : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- getTestDate UpdateStartDate


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    case model.page of
        NoPage ->
            Html.button [ onClick OpenBilling ] [ text "Billing" ]

        BillingPage ->
            div [] []


updateBilling : Billing.Types.Model -> Billing.Types.Model
updateBilling billingModel =
    { billingModel | state = Billing.Types.Grid, billingCcm = (Billing.Load.newEmployers billingModel.billingCcm) }


update : Msg -> Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    case msg of
        NoMessage ->
            ( model, Cmd.none )

        OpenBilling ->
            ( { model | page = BillingPage }, Billing.Main.getEmployment BillingLoad )

        BillingMsg billingModel billingMsg ->
            let
                newBillingModel =
                    Billing.Main.update (billingMsg) billingModel
            in
                ( { model | billingState = newBillingModel }, Cmd.none )

        BillingLoad (Ok loadedModel) ->
            ( { model | billingState = updateBilling loadedModel }, Cmd.none )

        BillingLoad (Err t) ->
            ( model, Cmd.none )

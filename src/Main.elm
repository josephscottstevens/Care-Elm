port module Main exposing (..)

import Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Table
import Utils.GridPaging exposing (..)
import Utils.CommonGrid exposing (..)
import Billing.Main


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
            div [] []

        BillingPage ->
            div [] []



-- BillingPage billingModel ->
--     Billing.Main.view billingModel


update : Msg -> Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    case msg of
        NoMessage ->
            ( model, Cmd.none )

        BillingMsg billingModel billingMsg ->
            ( { model | page = BillingPage }, Cmd.none )



-- BillingMsg t ->
--     ( { model = Billing.Main.update t }, Cmd.none )

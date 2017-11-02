port module Main exposing (..)

import Model exposing (..)
import Html
import Html.Events
import Billing.Main
import Billing.Types
import Billing.Load


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


view : Model -> Html.Html Msg
view model =
    case model.page of
        NoPage ->
            Html.button [ Html.Events.onClick OpenBilling ] [ Html.text "Billing" ]

        BillingPage ->
            Html.div []
                [ Html.map BillingMsg (Billing.Main.view model.billingState)
                ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        OpenBilling ->
            ( model, Billing.Load.getEmployment BillingLoad )

        BillingMsg billingMsg ->
            let
                newBillingModel =
                    Billing.Main.update billingMsg model.billingState
            in
                ( { model | billingState = newBillingModel }, Cmd.none )

        BillingLoad (Ok loadedModel) ->
            let
                updateBilling =
                    { loadedModel | state = Billing.Types.Grid, billingCcm = (Billing.Load.newEmployers loadedModel.billingCcm) }
            in
                ( { model | page = BillingPage, billingState = updateBilling }, Cmd.none )

        BillingLoad (Err t) ->
            ( model, Cmd.none )

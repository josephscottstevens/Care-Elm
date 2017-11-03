port module Main exposing (..)

import Model exposing (..)
import Html exposing (div, text)
import Html.Events exposing (onClick)
import Billing.Main
import Billing.Load
import Billing.Types


port sendTestDate : String -> Cmd msg


port openPage : (String -> msg) -> Sub msg



-- port getTestDate : (String -> msg) -> Sub msg
-- getTestDate UpdateStartDate


type alias Flags =
    { page : String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    openPage OpenPage


init : Flags -> ( Model, Cmd Msg )
init flags =
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
            Html.button [ Html.Events.onClick (OpenPage "billing") ] [ Html.text "Billing" ]

        BillingPage ->
            Html.map BillingMsg (Billing.Main.view model.billingState)



--Html.map BillingMsg (Billing.Main.view model.billingState)


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        OpenPage t ->
            ( { model | page = BillingPage }, Cmd.map BillingMsg Billing.Main.init )

        BillingMsg billingMsg ->
            ( { model | billingState = Billing.Main.update billingMsg model.billingState }, Cmd.none )

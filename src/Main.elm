port module Main exposing (main)

import Html exposing (Html, div)
import Records.Main
import Records.Model
import Demographics
import Utils.CommonTypes exposing (..)


port openPage : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DemographicsMsg Demographics.subscriptions


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel flags
    in
        if flags.pageFlag == "records" then
            ( { model | page = RecordsPage }, Cmd.map RecordsMsg (Records.Main.init flags) )
        else if flags.pageFlag == "demographics" then
            ( { model | page = DemographicsPage }, Cmd.map DemographicsMsg (Demographics.init flags) )
        else
            ( model, Cmd.none )


type Page
    = NoPage
    | RecordsPage
    | DemographicsPage


type alias Model =
    { page : Page
    , recordsState : Records.Model.Model
    , demographicsState : Demographics.Model
    }


type Msg
    = RecordsMsg Records.Model.Msg
    | DemographicsMsg Demographics.Msg


main : Program Flags Model Msg
main =
    Html.programWithFlags
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

        RecordsPage ->
            Html.map RecordsMsg (Records.Main.view model.recordsState)

        DemographicsPage ->
            Html.map DemographicsMsg (Demographics.view model.demographicsState)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecordsMsg recordsMsg ->
            let
                ( newRecordModel, widgetCmd ) =
                    Records.Main.update recordsMsg model.recordsState
            in
                ( { model | recordsState = newRecordModel }, Cmd.map RecordsMsg widgetCmd )

        DemographicsMsg demographicsMsg ->
            let
                ( newDemographicsModel, widgetCmd ) =
                    Demographics.update demographicsMsg model.demographicsState
            in
                ( { model | demographicsState = newDemographicsModel }, Cmd.map DemographicsMsg widgetCmd )


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , recordsState = Records.Model.emptyModel flags
    , demographicsState = Demographics.emptyModel flags.patientId
    }

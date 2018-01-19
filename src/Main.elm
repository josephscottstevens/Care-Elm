port module Main exposing (main)

import Html exposing (Html, div)
import Records
import Demographics
import Common.Types exposing (..)


port openPage : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        RecordsPage ->
            Sub.map RecordsMsg Records.subscriptions

        DemographicsPage ->
            Sub.map DemographicsMsg Demographics.subscriptions

        NoPage ->
            Sub.none


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel flags
    in
        if flags.pageFlag == "records" then
            ( { model | page = RecordsPage }, Cmd.map RecordsMsg (Records.init flags.recordType flags.patientId) )
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
    , recordsState : Records.Model
    , demographicsState : Demographics.Model
    , flags : Flags
    }


type Msg
    = RecordsMsg Records.Msg
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
            Html.map RecordsMsg (Records.view model.recordsState model.flags.recordType)

        DemographicsPage ->
            Html.map DemographicsMsg (Demographics.view model.demographicsState)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecordsMsg recordsMsg ->
            let
                ( newRecordModel, widgetCmd ) =
                    Records.update recordsMsg model.recordsState model.flags.patientId model.flags.recordType
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
    , recordsState = Records.emptyModel
    , demographicsState = Demographics.emptyModel flags.patientId
    , flags = flags
    }

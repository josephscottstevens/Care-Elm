port module Main exposing (main)

import Html exposing (Html)
import Records
import Demographics
import Common.Types exposing (..)
import Common.Route as Route


port openPage : (String -> msg) -> Sub msg


port cancelPage : (String -> msg) -> Sub msg


getSub : Model -> Sub Msg
getSub model =
    case model.page of
        RecordsPage ->
            Sub.map RecordsMsg Records.subscriptions

        DemographicsPage ->
            Sub.map DemographicsMsg (Demographics.subscriptions model.demographicsState model.flags.patientId)

        NoPage ->
            Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ getSub model
        , cancelPage CancelPage
        ]


init : ( Model, Cmd Msg )
init =
    let
        flags =
            { pageFlag = "", patientId = 1, recordType = Nothing }

        model =
            emptyModel flags
    in
        if flags.pageFlag == "records" then
            ( { model | page = RecordsPage }, Cmd.map RecordsMsg (Records.init flags.recordType flags.patientId) )
        else if flags.pageFlag == "demographics" then
            ( { model
                | page = DemographicsPage
                , demographicsState = Demographics.init model.demographicsState flags.patientId
              }
            , Cmd.none
            )
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
    | CancelPage String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecordsMsg recordsMsg ->
            let
                ( newRecordModel, widgetCmd ) =
                    Records.update recordsMsg model.recordsState model.flags.patientId
            in
                ( { model | recordsState = newRecordModel }, Cmd.map RecordsMsg widgetCmd )

        DemographicsMsg demographicsMsg ->
            let
                ( newDemographicsModel, widgetCmd ) =
                    Demographics.update demographicsMsg model.demographicsState
            in
                ( { model | demographicsState = newDemographicsModel }, Cmd.map DemographicsMsg widgetCmd )

        CancelPage _ ->
            let
                demographicsModel =
                    model.demographicsState
            in
                { model | demographicsState = { demographicsModel | demographicsUrl = Nothing } } ! []


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , recordsState = Records.emptyModel
    , demographicsState = Demographics.emptyModel flags.patientId
    , flags = flags
    }

port module Main exposing (..)

import Model exposing (..)
import Html exposing (div)
import Billing.Main as Billing
import Records.Main as Records
import RecordAddNew.Main as RecordAddNew
import Utils.CommonFunctions exposing (..)
import Utils.CommonTypes exposing (..)
import Functions exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onFocus)


port updatePage : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map RecordsMsg (Records.subscriptions model.recordsState)
        , Sub.map RecordAddNewMsg (RecordAddNew.subscriptions model.recordAddNewState)
        , updatePage UpdatePage
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel flags
    in
        if flags.pageFlag == "billing" then
            ( { model | state = BillingPage }, Cmd.map BillingMsg Billing.init )
        else if flags.pageFlag == "records" then
            { model | state = RecordsPage } ! [ Cmd.map RecordsMsg (Records.init flags.patientId flags.recordType), getDropDowns flags.recordType flags.patientId AddEditDataSourceLoaded ]
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
    case model.state of
        NoPage ->
            div [] []

        BillingPage ->
            div [] []

        RecordsPage ->
            div []
                [ case model.addEditDataSource of
                    Just t ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick (AddNewStart t) ] [ text "New Record" ]

                    Nothing ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
                , Html.map RecordsMsg (Records.view model.recordsState)
                ]

        RecordAddNewPage addEditDataSource ->
            div [ class "form-horizontal" ]
                [ Html.map RecordAddNewMsg (RecordAddNew.view model.recordAddNewState) ]

        Error str ->
            div [] [ text str ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        BillingMsg billingMsg ->
            model ! []

        RecordsMsg recordsMsg ->
            let
                ( newModel, pageCmd ) =
                    Records.update recordsMsg model.recordsState
            in
                { model | recordsState = newModel } ! [ Cmd.map RecordsMsg pageCmd ]

        RecordAddNewMsg recordAddNewMsg ->
            let
                ( newModel, pageCmd ) =
                    RecordAddNew.update recordAddNewMsg model.recordAddNewState
            in
                { model | recordAddNewState = newModel } ! [ Cmd.map RecordAddNewMsg pageCmd ]

        AddNewStart addEditDataSource ->
            let
                newState =
                    RecordAddNew.updateAddNewState model.recordAddNewState addEditDataSource
            in
                { model | state = RecordAddNewPage addEditDataSource, recordAddNewState = newState }
                    ! [ Cmd.map RecordAddNewMsg (RecordAddNew.init addEditDataSource) ]

        AddEditDataSourceLoaded (Ok t) ->
            { model | addEditDataSource = Just t } ! []

        AddEditDataSourceLoaded (Err httpError) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        UpdatePage pageName ->
            { model | state = RecordsPage } ! []



-- Cmd.map RecordsMsg (Records.init flags.patientId flags.recordType)

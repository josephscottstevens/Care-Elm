port module Main exposing (..)

import Model exposing (..)
import Html exposing (text, div, button)
import Billing.Main as Billing
import Billing.Types as BillingTypes
import Records.Main as Records
import Records.Types as RecordTypes
import RecordAddNew.Main as RecordAddNew
import Utils.CommonFunctions exposing (..)
import Utils.CommonTypes exposing (..)
import Functions exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)


port updatePage : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        RecordsPage recordState ->
            Sub.map RecordsMsg (Records.subscriptions recordState)

        _ ->
            Sub.batch
                [ Sub.map RecordAddNewMsg (RecordAddNew.subscriptions model.recordAddNewState)
                , updatePage UpdatePage
                ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel flags

        recordModel =
            (RecordTypes.emptyModel flags)
    in
        if flags.pageFlag == "billing" then
            { model | state = BillingPage BillingTypes.emptyModel } ! []
        else if flags.pageFlag == "records" then
            { model | state = RecordsPage recordModel }
                ! [ Cmd.map RecordsMsg (Records.init flags.patientId flags.recordType)
                  , getDropDowns flags.recordType flags.patientId AddEditDataSourceLoaded
                  ]
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

        BillingPage billingModel ->
            div [] []

        RecordsPage recordModel ->
            div []
                [ case model.addEditDataSource of
                    Just t ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick (AddNewStart t) ] [ text "New Record" ]

                    Nothing ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
                , Html.map RecordsMsg (Records.view recordModel)
                ]

        RecordAddNewPage ->
            div [ class "form-horizontal" ]
                [ Html.map RecordAddNewMsg (RecordAddNew.view model.recordAddNewState) ]

        Error str ->
            div [] [ text str ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        BillingMsg billingMsg billingModel ->
            { model | state = BillingPage billingModel } ! []

        RecordsMsg recordsMsg ->
            case model.state of
                RecordsPage recordState ->
                    let
                        ( newModel, pageCmd ) =
                            Records.update recordsMsg recordState
                    in
                        { model | state = RecordsPage recordState } ! [ Cmd.map RecordsMsg pageCmd ]

                _ ->
                    model ! []

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
                { model | state = RecordAddNewPage, recordAddNewState = newState }
                    ! [ Cmd.map RecordAddNewMsg (RecordAddNew.init addEditDataSource) ]

        AddEditDataSourceLoaded (Ok t) ->
            { model | addEditDataSource = Just t } ! []

        AddEditDataSourceLoaded (Err httpError) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        UpdatePage pageName ->
            { model | state = NoPage } ! [ Cmd.map RecordsMsg (Records.init model.patientId model.recordTypeId) ]

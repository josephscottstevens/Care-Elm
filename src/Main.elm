port module Main exposing (..)

import Model exposing (..)
import Html exposing (text, div, button)
import Billing.Types as BillingTypes
import Records.Main as Records
import Records.Types as RecordTypes
import RecordAddNew.Main as RecordAddNew
import RecordAddNew.Types as RecordAddNewTypes
import Utils.CommonFunctions exposing (..)
import Utils.CommonTypes exposing (..)
import Functions exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)


port updatePage : (( String, Maybe Int ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map RecordsMsg Records.subscriptions
        , Sub.map RecordAddNewMsg RecordAddNew.subscriptions
        , updatePage UpdatePage
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel flags
    in
        if flags.pageFlag == "billing" then
            { model | state = BillingPage BillingTypes.emptyModel } ! []
        else if flags.pageFlag == "records" then
            { model | state = RecordsPage (RecordTypes.emptyModel flags) }
                ! [ Cmd.map RecordsMsg (Records.init flags)
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

        BillingPage _ ->
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

        RecordAddNewPage recordAddNewModel ->
            div [ class "form-horizontal" ]
                [ Html.map RecordAddNewMsg (RecordAddNew.view recordAddNewModel) ]

        Error str ->
            div [] [ text str ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case ( msg, model.state ) of
        ( BillingMsg _, BillingPage _ ) ->
            model ! []

        ( RecordsMsg recordsMsg, RecordsPage recordModel ) ->
            let
                ( newModel, pageCmd ) =
                    Records.update recordsMsg recordModel
            in
                { model | state = RecordsPage newModel } ! [ Cmd.map RecordsMsg pageCmd ]

        ( RecordAddNewMsg recordAddNewMsg, RecordAddNewPage recordAddNewModel ) ->
            let
                ( newModel, pageCmd ) =
                    RecordAddNew.update recordAddNewMsg recordAddNewModel
            in
                { model | state = RecordAddNewPage newModel } ! [ Cmd.map RecordAddNewMsg pageCmd ]

        ( AddNewStart addEditDataSource, _ ) ->
            let
                newState =
                    RecordAddNew.updateAddNewState addEditDataSource model.flags
            in
                { model | state = RecordAddNewPage newState }
                    ! [ Cmd.map RecordAddNewMsg (RecordAddNew.init addEditDataSource) ]

        ( AddEditDataSourceLoaded (Ok t), _ ) ->
            { model | addEditDataSource = Just t } ! []

        ( AddEditDataSourceLoaded (Err httpError), _ ) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        ( UpdatePage ( pageName, dropId ), _ ) ->
            case pageName of
                "Records" ->
                    { model | state = RecordsPage (RecordTypes.emptyModel model.flags) } ! [ Cmd.map RecordsMsg (Records.init model.flags) ]

                "RecordAddEdit" ->
                    case model.addEditDataSource of
                        Just t ->
                            if dropId == t.recordTypeId then
                                model ! [ Cmd.map RecordAddNewMsg (RecordAddNew.init { t | recordTypeId = dropId, setFocus = True }), setLoadingStatus False ]
                            else
                                { model
                                    | state = RecordAddNewPage (RecordAddNewTypes.emptyModel model.flags)
                                    , addEditDataSource = Just { t | recordTypeId = dropId }
                                }
                                    ! []

                        Nothing ->
                            model ! []

                _ ->
                    model ! []

        ( _, _ ) ->
            model ! []

module Billing.Main exposing (..)

import Billing.Load exposing (..)
import Billing.Types exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Table
import Utils.GridPaging exposing (..)
import Utils.CommonGrid exposing (..)


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getEmployment )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStart employer ->
            ( { model | state = Edit employer }, Cmd.none )

        -- , sendTestDate employer.dOB
        -- EditSave employer ->
        --     ( { model | state = Grid, employment = (updateEmployers model.enrollment employer) }, Cmd.none )
        EditCancel ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok model) ->
            ( { model | state = Grid, billingCcm = (newEmployers model.billingCcm) }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        SetPagingState page ->
            let
                newPageIndex =
                    getNewState page model.currentPage (filteredCcmLength model)
            in
                ( { model | currentPage = newPageIndex }, Cmd.none )

        -- UpdateState emp newState ->
        --     ( { model | state = Edit { emp | state = newState } }, Cmd.none )
        -- UpdateCity emp newCity ->
        --     ( { model | state = Edit { emp | city = newCity } }, Cmd.none )
        -- UpdateStartDate newDob ->
        --     case model.state of
        --         Edit emp ->
        --             ( { model | state = Edit { emp | dob = newDob } }, Cmd.none )
        --         _ ->
        --             ( model, Cmd.none )
        SetQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }, Cmd.none )

        Reset ->
            ( emptyModel, getEmployment )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ button [ class "btn btn-default", onClick Reset ] [ text "reset" ]
                , input [ class "form-control", placeholder "Search by Facility", onInput SetQuery, value model.query ] []
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view config model.tableState ((filteredCcm model) |> List.drop (model.currentPage * pagesPerBlock) |> List.take itemsPerPage)
                    ]
                , pagingView model.currentPage (filteredCcmLength model)
                ]

        Edit emp ->
            div []
                [ input [ placeholder "Date of birth", type_ "text", class "e-textbox", id "testDate", value emp.dob ] []

                -- , input [ placeholder "City", class "e-textbox", onInput (UpdateCity emp), value emp.city ] []
                -- , input [ placeholder "State", class "e-textbox", onInput (UpdateState emp), value emp.state ] []
                -- , button [ class "btn btn-default", onClick (EditSave emp) ] [ text "save" ]
                , button [ class "btn btn-default", onClick EditCancel ] [ text "cancel" ]
                ]

        Error err ->
            div [] [ text (toString err) ]


filteredCcm : Model -> List BillingCcm
filteredCcm model =
    let
        lowerQuery =
            String.toLower model.query
    in
        model.billingCcm
            |> List.filter (String.contains lowerQuery << String.toLower << .facility)


filteredCcmLength : Model -> Int
filteredCcmLength model =
    List.length (filteredCcm model)


config : Table.Config BillingCcm Msg
config =
    Table.customConfig
        { toId = .patientName
        , toMsg = SetTableState
        , columns =
            [ checkColumn ""
            , Table.stringColumn "Facility" .facility
            , Table.stringColumn "Billing Date" .billingDate
            , Table.stringColumn "Main Provider" .mainProvider
            , Table.stringColumn "Patient Name" .patientName
            , Table.stringColumn "DOB" .dob
            , Table.stringColumn "Id No" (\t -> defaultString t.patientFacilityIdNo)
            , Table.stringColumn "AssignedTo" (\t -> defaultString t.assignedTo)
            , editColumn (\t -> onClick (EditStart t))
            ]
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations BillingCcm msg
defaultCustomizations =
    { tableAttrs = [ id "employersTable", class "e-table e-hidelines" ]
    , caption = Nothing
    , thead = simpleThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = simpleRowAttrs .iD
    }

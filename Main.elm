port module Main exposing (..)

import Load exposing (..)
import Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Grid exposing (..)
import Table
import GridPaging


port sendTestDate : String -> Cmd msg


port getTestDate : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- getTestDate UpdateStartDate


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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
                    GridPaging.getNewState page model.currentPage (filteredCcmLength model)
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
                    [ Table.view config model.tableState ((filteredCcm model) |> List.drop (model.currentPage * GridPaging.pagesPerBlock) |> List.take GridPaging.itemsPerPage)
                    ]
                , GridPaging.view model.currentPage (filteredCcmLength model)
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

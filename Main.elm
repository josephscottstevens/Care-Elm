port module Main exposing (..)

import Load exposing (..)
import Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Grid exposing (..)
import Table


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
            -- interesting, so ! [] is shorthand for ,( ... Cmd.none )
            ( { model | tableState = newState }, Cmd.none )

        Reset ->
            ( emptyModel, getEmployment )


pagerDiv : List BillingCcm -> Html msg
pagerDiv filteredEmployers =
    let
        currentPage =
            0

        itemsPerPage =
            10

        totalPages =
            (List.length filteredEmployers) // itemsPerPage

        rng =
            List.range 0 totalPages
                |> List.drop currentPage
                |> List.take 8
                |> List.map (\t -> toString (t + 1))
                |> List.map (\t -> text (t ++ " "))

        employersCount =
            toString (List.length filteredEmployers)
    in
        div [ class "e-pager e-js e-pager" ]
            [ div [ class "e-pagercontainer" ]
                [ div [ class "e-icon e-mediaback e-firstpagedisabled e-disable" ] []
                , div [ class "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable" ] []
                , a [ class "e-link e-nextprevitemdisabled e-disable e-spacing e-PP" ] []
                , div [ class "e-numericcontainer e-default" ]
                    []
                , div [ class "" ] []
                , div [ class "e-nextpage e-icon e-arrowheadright-2x e-default" ] []
                , div [ class "e-lastpage e-icon e-mediaforward e-default" ] []
                ]
            , div [ class "e-parentmsgbar", style [ ( "text-align", "right" ) ] ]
                [ span [ class "e-pagermsg" ] [ text "1 of 808 pages (16153 items)" ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        filteredEmployers =
            model.billingCcm
                |> List.filter (String.contains lowerQuery << String.toLower << .patientName)
    in
        case model.state of
            Initial ->
                div [] [ text "loading" ]

            Grid ->
                div []
                    [ button [ class "btn btn-default", onClick Reset ] [ text "reset" ]
                    , input [ class "form-control", placeholder "Search by Address", onInput SetQuery, value model.query ] []
                    , Table.view config model.tableState (filteredEmployers |> List.take 12)
                    , pagerDiv filteredEmployers
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

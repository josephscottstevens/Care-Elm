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


itemsPerPage =
    10


pagesPerBlock =
    8


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

        UpdatePage page ->
            let
                totalRows =
                    List.length (filteredCcm model)

                totalPages =
                    totalRows // itemsPerPage

                newPageIndex =
                    case page of
                        First ->
                            0

                        Previous ->
                            if model.currentPage > 0 then
                                model.currentPage - 1
                            else
                                0

                        PreviousBlock ->
                            0

                        Index t ->
                            t

                        NextBlock ->
                            0

                        Next ->
                            model.currentPage + 1

                        Last ->
                            totalPages - 1
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
            -- interesting, so ! [] is shorthand for ,( ... Cmd.none )
            ( { model | tableState = newState }, Cmd.none )

        Reset ->
            ( emptyModel, getEmployment )


filteredCcm : Model -> List BillingCcm
filteredCcm model =
    let
        lowerQuery =
            String.toLower model.query
    in
        model.billingCcm
            |> List.filter (String.contains lowerQuery << String.toLower << .patientName)


pagerDiv : List a -> Int -> Html Msg
pagerDiv filteredEmployers currentPage =
    let
        totalRows =
            List.length filteredEmployers

        totalPages =
            (totalRows // itemsPerPage) - 1

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == currentPage then
                        "e-currentitem e-active"
                    else
                        "e-default"
            in
                div [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText), onClick (UpdatePage (Index pageIndex)) ] [ text (toString (pageIndex + 1)) ]

        rng =
            List.range 0 totalPages
                |> List.drop ((currentPage // pagesPerBlock) * pagesPerBlock)
                |> List.take pagesPerBlock
                |> List.map activeOrNot

        firstPageClass =
            if currentPage >= pagesPerBlock then
                "e-icon e-mediaback e-firstpage e-default"
            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if currentPage > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"
            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if currentPage >= pagesPerBlock then
                "e-link e-spacing e-PP e-numericitem e-default"
            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if currentPage < totalPages - pagesPerBlock then
                "e-link e-NP e-spacing e-numericitem e-default"
            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if currentPage < totalPages then
                "e-nextpage e-icon e-arrowheadright-2x e-default"
            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if currentPage < totalPages - pagesPerBlock then
                "e-lastpage e-icon e-mediaforward e-default"
            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        employersCount =
            toString (List.length filteredEmployers)

        pagerText =
            let
                currentPageText =
                    toString (currentPage + 1)

                totalPagesText =
                    toString (totalPages + 1)

                totalItemsText =
                    toString totalRows
            in
                currentPageText ++ " of " ++ totalPagesText ++ " pages (" ++ totalItemsText ++ " items)"
    in
        div [ class "e-pager e-js e-pager" ]
            [ div [ class "e-pagercontainer" ]
                [ div [ class firstPageClass, onClick (UpdatePage First) ] []
                , div [ class leftPageClass, onClick (UpdatePage Previous) ] []
                , a [ class leftPageBlockClass, onClick (UpdatePage PreviousBlock) ] [ text "..." ]
                , div [ class "e-numericcontainer e-default" ] rng
                , a [ class rightPageBlockClass, onClick (UpdatePage NextBlock) ] [ text "..." ]
                , div [ class rightPageClass, onClick (UpdatePage Next) ] []
                , div [ class lastPageClass, onClick (UpdatePage Last) ] []
                ]
            , div [ class "e-parentmsgbar", style [ ( "text-align", "right" ) ] ]
                [ span [ class "e-pagermsg" ] [ text pagerText ]
                ]
            ]


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ button [ class "btn btn-default", onClick Reset ] [ text "reset" ]
                , input [ class "form-control", placeholder "Search by Address", onInput SetQuery, value model.query ] []
                , Table.view config model.tableState ((filteredCcm model) |> List.take 12)
                , pagerDiv (filteredCcm model) model.currentPage
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

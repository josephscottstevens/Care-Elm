module Utils.GridPaging exposing (itemsPerPage, pagesPerBlock, getNewState, pagingView)

import Html exposing (Html, Attribute, div, text, span, a)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Billing.Types exposing (..)


itemsPerPage : Int
itemsPerPage =
    10


pagesPerBlock : Int
pagesPerBlock =
    8


getNewState : Page -> Int -> Int -> Int
getNewState page currentPage totalVisiblePages =
    let
        totalPages =
            totalVisiblePages // itemsPerPage
    in
        case page of
            First ->
                0

            Previous ->
                if currentPage > 0 then
                    currentPage - 1
                else
                    0

            PreviousBlock ->
                0

            Index t ->
                t

            NextBlock ->
                0

            Next ->
                currentPage + 1

            Last ->
                totalPages - 1


pagingView : Int -> Int -> Html Msg
pagingView currentPage totalVisiblePages =
    let
        totalPages =
            (totalVisiblePages // itemsPerPage) - 1

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == currentPage then
                        "e-currentitem e-active"
                    else
                        "e-default"
            in
                div
                    [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText), onClick (SetPagingState (Index pageIndex)) ]
                    [ text (toString (pageIndex + 1)) ]

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

        pagerText =
            let
                currentPageText =
                    toString (currentPage + 1)

                totalPagesText =
                    toString (totalPages + 1)

                totalItemsText =
                    toString totalVisiblePages
            in
                currentPageText ++ " of " ++ totalPagesText ++ " pages (" ++ totalItemsText ++ " items)"
    in
        div [ class "e-pager e-js e-pager" ]
            [ div [ class "e-pagercontainer" ]
                [ div [ class firstPageClass, onClick (SetPagingState First) ] []
                , div [ class leftPageClass, onClick (SetPagingState Previous) ] []
                , a [ class leftPageBlockClass, onClick (SetPagingState PreviousBlock) ] [ text "..." ]
                , div [ class "e-numericcontainer e-default" ] rng
                , a [ class rightPageBlockClass, onClick (SetPagingState NextBlock) ] [ text "..." ]
                , div [ class rightPageClass, onClick (SetPagingState Next) ] []
                , div [ class lastPageClass, onClick (SetPagingState Last) ] []
                ]
            , div [ class "e-parentmsgbar", style [ ( "text-align", "right" ) ] ]
                [ span [ class "e-pagermsg" ] [ text pagerText ]
                ]
            ]

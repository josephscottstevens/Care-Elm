module GridPaging exposing (..)

import Html exposing (Html, Attribute, div, text, span)
import Html.Attributes as Attr
import Json.Decode as Json
import Html.Events as E


type PageState
    = PageState Int


type GridPagingConfig msg
    = GridPagingConfig
        { itemsPerPage : Int
        , pagesPerBlock : Int
        , toMsg : PageState -> msg
        }


initialPageState : PageState
initialPageState =
    PageState 0


type Page
    = First
    | Previous
    | PreviousBlock
    | Index Int
    | NextBlock
    | Next
    | Last


onClick : Int -> (PageState -> msg) -> Attribute msg
onClick currentPage toMsg =
    E.on "click" <|
        Json.map toMsg <|
            Json.map PageState (Json.succeed currentPage)


getNewState : Page -> Int -> List -> Int
getNewState page currentPage lst =
    let
        totalRows =
            10

        --List.length (filteredCcm model)
        totalPages =
            5

        --totalRows // itemsPerPage
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


view : GridPagingConfig msg -> PageState -> List data -> Html msg
view t state data =
    let
        currentPage =
            0

        filteredList =
            []

        itemsPerPage =
            0

        pagesPerBlock =
            0

        totalRows =
            List.length filteredList

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
                div [] []

        -- div [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText), onClick (Index pageIndex) ] [ text (toString (pageIndex + 1)) ]
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
            toString (List.length filteredList)

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
        div [ Attr.class "e-pager e-js e-pager" ]
            [ div [ Attr.class "e-pagercontainer" ]
                [--     div [ class firstPageClass, onClick First ] []
                 -- , div [ class leftPageClass, onClick Previous ] []
                 -- , a [ class leftPageBlockClass, onClick PreviousBlock ] [ text "..." ]
                 -- , div [ class "e-numericcontainer e-default" ] rng
                 -- , a [ class rightPageBlockClass, onClick NextBlock ] [ text "..." ]
                 -- , div [ class rightPageClass, onClick Next ] []
                 -- , div [ class lastPageClass, onClick Last ] []
                ]
            , div [ Attr.class "e-parentmsgbar", Attr.style [ ( "text-align", "right" ) ] ]
                [ span [ Attr.class "e-pagermsg" ] [ text pagerText ]
                ]
            ]

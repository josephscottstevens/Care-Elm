module GridPaging exposing (..)

import Html exposing (Html, Attribute, div, text, span)
import Html.Attributes as Attr
import Json.Decode as Json
import Html.Events as E


type SortState
    = SortState Int


type alias GridPagingConfig =
    { itemsPerPage : Int
    , pagesPerBlock : Int
    , currentPage : Int
    }


type Page
    = First
    | Previous
    | PreviousBlock
    | Index Int
    | NextBlock
    | Next
    | Last


onClick : Page -> Int -> (SortState -> msg) -> Attribute msg
onClick page currentPage toMsg =
    E.on "click" <|
        Json.map toMsg <|
            Json.map SortState (Json.succeed currentPage)



-- onClick name isReversed toMsg =
--     E.on "click" <|
--         Json.map toMsg <|
--             Json.map SortState (Json.succeed 0)
-- toSortSetPage : Page -> SortState -> msg
-- toSortSetPage page sortState =
--     onClick


pagerDiv : GridPagingConfig -> SortState -> List data -> Html msg
pagerDiv { itemsPerPage, pagesPerBlock, currentPage } state filteredList =
    let
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

module HtmlHelper exposing (..)

import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)


gridStyle : Html.Attribute msg
gridStyle =
    style
        [ ( "display", "grid" )
        , ( "grid-template-columns", "1fr" )
        , ( "grid-template-rows", "repeat(-1, auto)" )
        , ( "grid-gap", "1px" )
        , ( "padding", "40px" )
        , ( "text-align", "center" )
        ]


rowStyle : Html.Attribute msg
rowStyle =
    style
        [ ( "display", "grid" )
        , ( "grid-template-columns", "80px 1fr 1fr 1fr 1fr 1fr 1fr" )
        , ( "grid-template-rows", "auto auto" )
        , ( "grid-gap", "5px" )
        , ( "grid-column-start", "1" )
        , ( "box-shadow", "0 0 0 1px gray" )
        ]


cellStyle : Html.Attribute msg
cellStyle =
    style [ ( "padding", "10px" ) ]


priorityList : Html msg
priorityList =
    select []
        [ option [ value "High" ] [ text "High" ]
        , option [ value "Medium" ] [ text "Medium" ]
        , option [ value "Low" ] [ text "Low" ]
        ]

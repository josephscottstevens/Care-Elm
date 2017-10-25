module HtmlHelper exposing (..)

import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)


controlStyle : Html.Attribute msg
controlStyle =
    style [ ( "margin", "5px" ) ]


gridStyle : Html.Attribute msg
gridStyle =
    style
        [ ( "display", "grid" )
        , ( "grid-template-columns", "1fr" )
        , ( "grid-template-rows", "repeat(-1, auto)" )
        , ( "padding", "40px" )
        , ( "text-align", "center" )
        ]


rowStyle : Html.Attribute msg
rowStyle =
    style
        [ ( "display", "grid" )
        , ( "grid-template-columns", "80px 1fr 1fr 1fr 1fr 1fr 1fr 1fr" )
        , ( "grid-template-rows", "auto auto" )
        , ( "grid-row-gap", "1px" )
        , ( "box-sizing", "border-box" )
        , ( "box-shadow", "0 0 1px grey" )
        ]


cellStyle : List (Html.Attribute msg)
cellStyle =
    [ style
        [ ( "padding", "10px" )
        ]
    ]


tableStyle : List (Html.Attribute msg)
tableStyle =
    [ style
        [ ( "width", "100%" )
        , ( "color", "#333" )
        , ( "font-family", "Helvetica, Arial, sans-serif" )
        , ( "border-collapse", "collapse" )
        , ( "border-spacing", "0" )
        ]
    ]


tdStyle : List (Html.Attribute msg)
tdStyle =
    [ style
        [ ( "border", "1px solid #CCC" )
        , ( "background", "#FAFAFA" )
        , ( "text-align", "center" )
        ]
    ]


thStyle : List (Html.Attribute msg)
thStyle =
    [ style
        [ ( "border", "1px solid #CCC" )
        , ( "background", "#F3F3F3" )
        , ( "text-align", "center" )
        , ( "font-weight", "bold" )
        , ( "height", "50px" )
        ]
    ]

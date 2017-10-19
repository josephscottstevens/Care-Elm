module HtmlHelper exposing (..)

import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Model exposing (..)


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
        , ( "grid-template-columns", "80px 1fr 1fr 1fr 1fr 1fr 1fr" )
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


headerStyle : List (Html.Attribute msg)
headerStyle =
    List.append cellStyle
        [ style
            [ ( "font-weight", "600" )
            , ( "background-color", "#f1f1f1" )
            , ( "cursor", "pointer" )
            ]
        ]


employmentHeaders : Html msg
employmentHeaders =
    div [ rowStyle ]
        [ div headerStyle [ text "Id" ]
        , div headerStyle [ text "Priority" ]
        , div headerStyle [ text "Title" ]
        , div headerStyle [ text "Name" ]
        , div headerStyle [ text "InitiatedOn" ]
        , div headerStyle [ text "Due At" ]
        , div headerStyle [ text "Closed" ]
        ]


employmentRows : List Employer -> List (Html msg)
employmentRows emp =
    emp
        |> List.map
            (\t ->
                div [ rowStyle ]
                    [ div cellStyle [ text t.occupation ]
                    , div cellStyle [ text t.employer ]
                    , div cellStyle [ text t.startDate ]
                    , div cellStyle [ text t.endDate ]
                    , div cellStyle [ text t.contactPerson ]
                    , div cellStyle [ text t.status ]
                    , div cellStyle [ text t.state ]
                    ]
            )

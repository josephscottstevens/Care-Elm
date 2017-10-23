module HtmlHelper exposing (..)

import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)
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
        [ div headerStyle [ text " " ]
        , div headerStyle [ text "Occupation" ]
        , div headerStyle [ text "Employer" ]
        , div headerStyle [ text "Start Date" ]
        , div headerStyle [ text "End Date" ]
        , div headerStyle [ text "Contact Person" ]
        , div headerStyle [ text "Status" ]
        , div headerStyle [ text "State" ]
        ]


employmentRows : List Employer -> List (Html Msg)
employmentRows emp =
    emp
        |> List.map
            (\t ->
                div [ rowStyle ]
                    [ button [ class "btn btn-default", controlStyle, onClick (EditStart t) ] [ text "edit" ]
                    , div cellStyle [ text t.occupation ]
                    , div cellStyle [ text t.employer ]
                    , div cellStyle [ text t.startDate ]
                    , div cellStyle [ text t.endDate ]
                    , div cellStyle [ text t.contactPerson ]
                    , div cellStyle [ text t.status ]
                    , div cellStyle [ text t.state ]
                    ]
            )

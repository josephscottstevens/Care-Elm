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


employmentHeaders : Html Msg
employmentHeaders =
    div [ rowStyle ]
        [ button [ onClick SortByZip ] [ text "sort" ]
        , div headerStyle [ text " " ]
        , div headerStyle [ text "Address Line 1" ]
        , div headerStyle [ text "Address Line 2" ]
        , div headerStyle [ text "Phone" ]
        , div headerStyle [ text "Date of birth" ]
        , div headerStyle [ text "City" ]
        , div headerStyle [ text "State" ]
        , div headerStyle [ text "Zip" ]
        ]


sortFunc : (Employer -> comparable) -> SortMode -> List Employer -> List Employer
sortFunc col sortMode employers =
    case sortMode of
        SortNone ->
            employers

        SortDesc ->
            employers |> List.sortBy col

        SortAsc ->
            employers |> List.sortBy col


employmentRows : List Employer -> SortMode -> List (Html Msg)
employmentRows employers sortMode =
    employers
        |> sortFunc .zipCode sortMode
        |> List.take 20
        |> List.map
            (\t ->
                div [ rowStyle ]
                    [ button [ class "btn btn-default", controlStyle, onClick (EditStart t) ] [ text "edit" ]
                    , div cellStyle [ text t.addressLine1 ]
                    , div cellStyle [ text t.addressLine2 ]
                    , div cellStyle [ text t.phone ]
                    , div cellStyle [ text t.dob ]
                    , div cellStyle [ text t.city ]
                    , div cellStyle [ text t.state ]
                    , div cellStyle [ text t.zipCode ]
                    ]
            )

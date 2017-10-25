module Grid exposing (customGrid)

import Model exposing (..)
import HtmlHelper exposing (..)
import Html exposing (Html, text, div, program, button)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Table


customGrid : { a | employers : List Employer, tableState : Table.State } -> Html Msg
customGrid model =
    Table.view config model.tableState (List.take 15 model.employers)


config : Table.Config Employer Msg
config =
    Table.customConfig
        { toId = .city
        , toMsg = SetTableState
        , columns =
            [ editColumn
            , editColumn2
            , Table.stringColumn "Date of birth" .dob
            , Table.stringColumn "Address Line 1" .addressLine1
            , Table.stringColumn "Address Line 2" .addressLine2
            , Table.stringColumn "City" .city
            , Table.stringColumn "State" .state
            , Table.stringColumn "Zip Code" .zipCode
            , Table.stringColumn "Phone" .phone
            ]
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations data msg
defaultCustomizations =
    { tableAttrs = tableStyle
    , caption = Nothing
    , thead = simpleThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = simpleRowAttrs
    }


simpleTheadHelp : ( String, Table.Status, Html.Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, onClick ) =
    let
        content =
            case status of
                Table.Unsortable ->
                    [ Html.text name ]

                Table.Sortable selected ->
                    [ Html.text name ]

                Table.Reversible Nothing ->
                    [ Html.text name ]

                Table.Reversible (Just isReversed) ->
                    [ Html.text name
                    , if isReversed then
                        div [ class "glyphicon glyphicon-menu-up" ] []
                      else
                        div [ class "glyphicon glyphicon-menu-down" ] []
                    ]
    in
        Html.th (onClick :: thStyle) content


darkGrey : String -> Html msg
darkGrey symbol =
    Html.span [ style [ ( "color", "#555" ) ] ] [ Html.text (" " ++ symbol) ]


lightGrey : String -> Html msg
lightGrey symbol =
    Html.span [ style [ ( "color", "#ccc" ) ] ] [ Html.text (" " ++ symbol) ]


simpleThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleRowAttrs : data -> List (Html.Attribute msg)
simpleRowAttrs _ =
    []


editColumn : Table.Column Employer Msg
editColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = editButton
        , sorter = Table.unsortable
        }


editColumn2 : Table.Column Employer Msg
editColumn2 =
    Table.veryCustomColumn
        { name = ""
        , viewData = editButton2
        , sorter = Table.unsortable
        }


editButton : Employer -> Table.HtmlDetails Msg
editButton emp =
    Table.HtmlDetails []
        [ button [ class "btn btn-default", controlStyle, onClick (EditStart emp) ] [ text "Edit" ]
        ]


editButton2 : Employer -> Table.HtmlDetails Msg
editButton2 emp =
    Table.HtmlDetails []
        [ button [ class "btn btn-default", controlStyle, onClick (EditStart emp) ] [ text emp.addressLine1 ]
        ]

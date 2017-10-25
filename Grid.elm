module Grid exposing (customGrid, pagingControl)

import Model exposing (..)
import Styles exposing (..)
import Html exposing (Html, text, div, program, button, input, span)
import Html.Attributes exposing (style, class, type_)
import Html.Events exposing (onClick)
import Table


customGrid : { a | employers : List Employer, tableState : Table.State } -> Html Msg
customGrid model =
    Table.view config model.tableState (List.take 12 model.employers)


config : Table.Config Employer Msg
config =
    Table.customConfig
        { toId = .city
        , toMsg = SetTableState
        , columns =
            [ -- checkColumn "Reviewed"
              Table.stringColumn "Date of birth" .dob
            , Table.stringColumn "Address Line 1" (\t -> t.addressLine1)
            , Table.stringColumn "Address Line 2" .addressLine2
            , Table.stringColumn "City" .city
            , Table.stringColumn "State" .state
            , Table.stringColumn "Zip Code" .zipCode
            , Table.stringColumn "Phone" .phone
            , editColumn
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
        , viewData = editColumnCell
        , sorter = Table.unsortable
        }


editColumnCell : Employer -> Table.HtmlDetails Msg
editColumnCell emp =
    Table.HtmlDetails []
        [ button [ class "btn btn-default", controlStyle, onClick (EditStart emp) ] [ text "Edit" ]
        ]


checkColumn : String -> Table.Column Employer Msg
checkColumn name =
    Table.veryCustomColumn
        { name = name
        , viewData = checkColumnCell
        , sorter = Table.unsortable
        }


checkColumnCell : Employer -> Table.HtmlDetails Msg
checkColumnCell emp =
    Table.HtmlDetails []
        [ input [ type_ "checkbox" ] []
        ]


pagingControl : Model -> Html Msg
pagingControl model =
    let
        len =
            (List.length model.employers) // 12

        rng =
            List.range 1 (len + 1)
                |> List.map (\t -> text (toString t ++ " "))
    in
        span [] rng

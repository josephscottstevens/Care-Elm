module Common.Dialog exposing (ConfirmDialog, dialogConfirm, viewConfirm)

import Html exposing (Html, text, div, span, button, input, label, textarea)
import Html.Attributes exposing (class, type_, id, value, for, name, style, checked, tabindex, title, hidden)
import Html.Events exposing (onInput, onCheck, onClick)


type alias ConfirmDialog data =
    { data : data
    , headerText : String
    , message : String
    }



-- , confirmData : Maybe (ConfirmDialog Row)


dialogConfirm : Bool -> { c | confirmData : b } -> List (Cmd msg) -> ( { c | confirmData : Maybe a }, Cmd msg )
dialogConfirm confirmed model cmds =
    { model | confirmData = Nothing }
        ! if confirmed then
            cmds
          else
            []


viewConfirm : Maybe (ConfirmDialog data) -> (Bool -> data -> msg) -> Html msg
viewConfirm maybeData confirmMsg =
    div [] <|
        case maybeData of
            Just { data, headerText, message } ->
                [ div
                    [ class "e-dialog e-widget e-box e-dialog-wrap e-shadow"
                    , style
                        [ ( "z-index", " 2147483647" )
                        , ( "width", " 500px" )
                        , ( "min-width", " 200px" )
                        , ( "height", " auto" )
                        , ( "min-height", " 120px" )
                        , ( "max-height", " 300px" )
                        , ( "top", " 301px" )
                        , ( "left", " 509px" )
                        , ( "position", " absolute" )
                        ]
                    ]
                    [ div [ class "e-titlebar e-header e-dialog e-draggable e-js" ]
                        [ span [ class "e-title", style [ ( "max-width", "435.6px" ) ] ] [ text headerText ]
                        , div [ class "e-dialog-icon e-icon e-close", tabindex 0, title "Close" ] []
                        ]
                    , div [ class "e-dialog-scroller e-scroller e-js e-widget", style [ ( "height", "auto" ), ( "width", "498px" ) ] ]
                        [ div
                            [ style
                                [ ( "height", " auto" )
                                , ( "display", " block" )
                                , ( "min-height", " 71px" )
                                , ( "width", " 498px" )
                                , ( "max-height", " 300px" )
                                ]
                            ]
                            [ div [ class "col-xs-12 padding-top-10 confirm-message" ] [ text message ]
                            , div [ class "col-xs-12 padding-top-10 padding-bottom-10" ]
                                [ div [ class "col-xs-12 padding-right-0" ]
                                    [ input [ type_ "button", class "btn btn-sm btn-default pull-right margin-left-5 confirm-cancel", value "Cancel", onClick (confirmMsg False data) ] []
                                    , input [ type_ "button", class "btn btn-sm btn-danger pull-right confirm-submit", value "Continue", onClick (confirmMsg True data) ] []
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ class "e-overlay"
                    , style
                        [ ( "z-index", " 2147483646" )
                        , ( "top", " 0px" )
                        , ( "left", " 0px" )
                        , ( "position", " fixed" )
                        ]
                    ]
                    []
                ]

            Nothing ->
                []

module Common.Dialog exposing (Dialog, DialogOptions, defaultDialogOptions, viewDialog, update)

import Html exposing (Html, text, div, span, button, input, label, textarea)
import Html.Attributes exposing (class, type_, id, value, for, name, style, checked, tabindex, title, hidden)
import Html.Events exposing (onInput, onCheck, onClick)


type alias Dialog data msg =
    { data : data
    , onConfirm : data -> msg
    , onCancel : data -> msg
    , headerText : String
    , dialogContent : data -> Html msg
    }


type alias DialogOptions =
    { width : String
    , minWidth : String
    , height : String
    , minHeight : String
    , top : String
    , left : String
    }


defaultDialogOptions : DialogOptions
defaultDialogOptions =
    { width = "500px"
    , minWidth = "200px"
    , height = "auto"
    , minHeight = "120px"
    , top = "301px"
    , left = "509px"
    }


viewDialog : Maybe (Dialog data msg) -> DialogOptions -> Html msg
viewDialog maybeData dialogOptions =
    div [] <|
        case maybeData of
            Just { data, onConfirm, onCancel, headerText, dialogContent } ->
                [ div
                    [ class "e-dialog e-widget e-box e-dialog-wrap e-shadow"
                    , style
                        [ ( "z-index", "2147483647" )
                        , ( "width", dialogOptions.width )
                        , ( "min-width", dialogOptions.minWidth )
                        , ( "height", "auto" )
                        , ( "min-height", dialogOptions.minHeight )

                        -- , ( "max-height", "300px" )
                        , ( "top", dialogOptions.top )
                        , ( "left", dialogOptions.left )
                        , ( "position", "absolute" )
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
                            [ div [ class "col-xs-12 padding-top-10 confirm-message" ]
                                [ dialogContent data
                                ]
                            , div [ class "col-xs-12 padding-top-10 padding-bottom-10" ]
                                [ div [ class "col-xs-12 padding-right-0" ]
                                    [ input
                                        [ type_ "button"
                                        , class "btn btn-sm btn-default pull-right margin-left-5 confirm-cancel"
                                        , value "Cancel"
                                        , onClick (onCancel data)
                                        ]
                                        []
                                    , input
                                        [ type_ "button"
                                        , class "btn btn-sm btn-danger pull-right confirm-submit"
                                        , value "Continue"
                                        , onClick (onConfirm data)
                                        ]
                                        []
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


update : Maybe (Dialog data msg) -> data -> Maybe (Dialog data msg)
update maybeDialog data =
    case maybeDialog of
        Just dialog ->
            Just { dialog | data = data }

        Nothing ->
            Nothing

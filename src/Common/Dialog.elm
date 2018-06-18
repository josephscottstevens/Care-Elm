module Common.Dialog exposing (Dialog, DialogOptions, RootDialog, defaultDialogOptions, simpleDialogOptions, update, viewDialog)

import Html exposing (Html, button, div, input, label, span, text, textarea)
import Html.Attributes exposing (checked, class, for, hidden, id, name, style, tabindex, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Window


type alias Dialog data msg =
    { data : data
    , onConfirm : data -> msg
    , onCancel : msg
    , headerText : String
    , dialogContent : data -> Html msg
    , dialogOptions : DialogOptions
    }


type alias RootDialog =
    { windowSize : Window.Size
    , windowScrollY : Float
    , top : Int
    , left : Int
    }


type alias DialogOptions =
    { width : Int
    , height : Int
    }


defaultWidth : Int
defaultWidth =
    500


defaultHeight : Int
defaultHeight =
    156


toPx : Int -> String
toPx size =
    toString size ++ "px"


calcLeft : Window.Size -> Int -> Int
calcLeft windowSize dialogWidth =
    windowSize.width // 2 - dialogWidth // 2


calcTop : Window.Size -> Int -> Int
calcTop windowSize dialogHeight =
    windowSize.height // 2 - dialogHeight // 2 - 118


defaultDialogOptions : DialogOptions
defaultDialogOptions =
    { width = defaultWidth
    , height = defaultHeight
    }


simpleDialogOptions : Int -> Int -> DialogOptions
simpleDialogOptions width height =
    { width = width
    , height = height
    }


viewDialog : Dialog data msg -> RootDialog -> Html msg
viewDialog { data, onConfirm, onCancel, headerText, dialogContent, dialogOptions } rootDialog =
    div []
        [ div
            [ class "e-dialog e-widget e-box e-dialog-wrap e-shadow"
            , style
                [ ( "z-index", "2147483647" )
                , ( "width", toPx dialogOptions.width )
                , ( "min-width", "200px" )
                , ( "height", toPx dialogOptions.height )
                , ( "min-height", "120px" )
                , ( "top", (calcTop rootDialog.windowSize dialogOptions.height - rootDialog.top) |> toPx )
                , ( "left", (calcLeft rootDialog.windowSize dialogOptions.width - rootDialog.left) |> toPx )
                , ( "position", "absolute" )
                ]
            ]
            [ div [ class "e-titlebar e-header e-dialog e-draggable e-js" ]
                [ span
                    [ class "e-title"
                    , style [ ( "max-width", dialogOptions.width - 65 |> toPx ) ]
                    ]
                    [ text headerText ]
                , div [ class "e-dialog-icon e-icon e-close", tabindex 0, title "Close", onClick onCancel ] []
                ]
            , div
                [ class "e-dialog-scroller e-scroller e-js e-widget"
                , style [ ( "height", "auto" ), ( "width", dialogOptions.width - 2 |> toPx ) ]
                ]
                [ div
                    [ style
                        [ ( "height", dialogOptions.height - 30 |> toPx )
                        , ( "display", "block" )
                        , ( "min-height", "71px" )
                        , ( "width", dialogOptions.width - 2 |> toPx )

                        -- , ( "max-height", " 300px" )
                        ]
                    ]
                    [ div
                        [ class "col-xs-12 padding-top-10 confirm-message"
                        , style [ ( "height", dialogOptions.height - 40 - 71 |> toPx ) ]
                        ]
                        [ dialogContent data
                        ]
                    , div [ class "col-xs-12 padding-top-10 padding-bottom-10" ]
                        [ div [ class "col-xs-12 padding-right-0" ]
                            [ input
                                [ type_ "button"
                                , class "btn btn-sm btn-default pull-right margin-left-5 confirm-cancel"
                                , value "Cancel"
                                , onClick onCancel
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


update : Maybe (Dialog data msg) -> data -> Maybe (Dialog data msg)
update maybeDialog data =
    case maybeDialog of
        Just dialog ->
            Just { dialog | data = data }

        Nothing ->
            Nothing

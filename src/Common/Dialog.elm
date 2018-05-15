module Common.Dialog exposing (Dialog, DialogOptions, RootDialog, defaultDialogOptions, simpleDialogOptions, update, viewDialog)

import Common.Types exposing (WindowSize)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, style, tabindex, title, type_, value)
import Html.Events exposing (onClick)



-- import Window


type alias Dialog data msg =
    { data : data
    , onConfirm : data -> msg
    , onCancel : msg
    , headerText : String
    , dialogContent : data -> Html msg
    , dialogOptions : DialogOptions
    }


type alias RootDialog =
    { windowSize : WindowSize
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
    String.fromInt size ++ "px"


calcLeft : ( Int, Int ) -> Int -> Int
calcLeft ( w, h ) dialogWidth =
    w // 2 - dialogWidth // 2


calcTop : ( Int, Int ) -> Int -> Int
calcTop ( w, h ) dialogHeight =
    h // 2 - dialogHeight // 2 - 118


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
            , style "z-index" "2147483647"
            , style "width" (toPx dialogOptions.width)
            , style "min-width" "200px"
            , style "height" (toPx dialogOptions.height)
            , style "min-height" "120px"

            -- TODO
            -- , ( "top", (calcTop rootDialog.windowSize dialogOptions.height - rootDialog.top) |> toPx )
            -- , ( "left", (calcLeft rootDialog.windowSize dialogOptions.width - rootDialog.left) |> toPx )
            , style "position" "absolute"
            ]
            [ div [ class "e-titlebar e-header e-dialog e-draggable e-js" ]
                [ span
                    [ class "e-title"
                    , style "max-width" (dialogOptions.width - 65 |> toPx)
                    ]
                    [ text headerText ]
                , div [ class "e-dialog-icon e-icon e-close", tabindex 0, title "Close", onClick onCancel ] []
                ]
            , div
                [ class "e-dialog-scroller e-scroller e-js e-widget"
                , style "height" "auto"
                , style "width" (dialogOptions.width - 2 |> toPx)
                ]
                [ div
                    [ style "height" (dialogOptions.height - 30 |> toPx)
                    , style "display" "block"
                    , style "min-height" "71px"
                    , style "width" (dialogOptions.width - 2 |> toPx)
                    ]
                    [ div
                        [ class "col-xs-12 padding-top-10 confirm-message"
                        , style "height" (dialogOptions.height - 40 - 71 |> toPx)
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
            , style "z-index" " 2147483646"
            , style "top" " 0px"
            , style "left" " 0px"
            , style "position" " fixed"
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

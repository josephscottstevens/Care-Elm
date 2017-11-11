module Utils.CommonHtml exposing (dropInput, textInput, fileInput, fullWidth, labelWidth, controlWidth)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a, label)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for, name, readonly)
import Html.Events exposing (onInput, on)
import Char exposing (isLower, isUpper)
import Json.Decode as Json
import Utils.CommonTypes as CT exposing (RequiredType)


isAlpha : Char -> Bool
isAlpha char =
    isLower char || isUpper char


forId : String -> Html.Attribute msg
forId str =
    for (String.filter isAlpha str)


idAttr : String -> Html.Attribute msg
idAttr str =
    id (String.filter isAlpha str)


nameAttr : String -> Html.Attribute msg
nameAttr str =
    name (String.filter isAlpha str)


fullWidth : String
fullWidth =
    "col-sm-10 col-md-7 col-lg-6"


labelWidth : String
labelWidth =
    "col-sm-2 col-md-2 col-lg-2"


controlWidth : String
controlWidth =
    "col-sm-8 col-md-5 col-lg-4"


commonStructure : Html msg -> Html msg
commonStructure t =
    div [ class controlWidth ]
        [ t
        ]


isRequiredStr : RequiredType -> String
isRequiredStr requiredType =
    case requiredType of
        CT.Required ->
            " required"

        CT.Optional ->
            ""


inputCommonFormat : RequiredType -> String -> Html msg -> Html msg
inputCommonFormat requiredType displayText t =
    div [ class "form-group" ]
        [ label [ class (labelWidth ++ "control-label" ++ isRequiredStr requiredType), forId displayText ] [ text displayText ]
        , t
        ]


type InputControlType
    = Textbox
    | TextArea


inputCommonWithType : String -> (String -> msg) -> RequiredType -> InputControlType -> Html msg
inputCommonWithType displayText event requiredType controlType =
    let
        commonInput t =
            inputCommonFormat requiredType displayText (commonStructure t)
    in
        case controlType of
            TextArea ->
                commonInput (input [ type_ "textarea", class "e-textbox", id "Files", name "Files" ] [])

            Textbox ->
                commonInput (input [ type_ "textbox", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event ] [])


fileInput : String -> (String -> msg) -> RequiredType -> Html msg
fileInput displayText event requiredType =
    div [ class "form-group" ]
        [ label [ class (labelWidth ++ "control-label " ++ isRequiredStr requiredType), for "fileName" ]
            [ text displayText ]
        , div [ class "col-sm-6 col-md-4 col-lg-3" ]
            [ input [ type_ "text", class "e-textbox", id "fileName", onChange event, readonly True ] []
            ]
        , div [ class "col-sm-2 col-md-1 col-lg-1" ]
            [ div [ id "fileBtn" ] []
            ]
        ]


textInput : String -> (String -> msg) -> RequiredType -> Html msg
textInput displayText event requiredType =
    inputCommonWithType displayText event requiredType Textbox


dropInput : String -> RequiredType -> Html msg
dropInput displayText requiredType =
    inputCommonFormat requiredType displayText (commonStructure (input [ type_ "text", idAttr displayText ] []))


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.map handler <| Json.at [ "target", "value" ] Json.string

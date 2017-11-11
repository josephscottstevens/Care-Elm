module Utils.CommonHtml exposing (textInput, dropInput, areaInput, fileInput, fullWidth, labelWidth, controlWidth, InputControlType(..), makeControls)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a, label, textarea)
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


type InputControlType msg
    = TextInput RequiredType String (String -> msg)
    | AreaInput RequiredType String String (String -> msg)
    | DropInput RequiredType String String
    | FileInput RequiredType String String


makeControls : List (InputControlType msg) -> List (Html msg)
makeControls controls =
    List.map common controls


common : InputControlType msg -> Html msg
common controlType =
    case controlType of
        TextInput requiredType displayText event ->
            inputCommonFormat requiredType displayText (commonStructure (textInput displayText event))

        AreaInput requiredType displayText displayValue event ->
            inputCommonFormat requiredType displayText (commonStructure (areaInput displayText displayValue event))

        DropInput requiredType displayText syncfusionId ->
            inputCommonFormat requiredType displayText (commonStructure (dropInput displayText syncfusionId))

        FileInput requiredType displayText displayValue ->
            fileInput requiredType displayText displayValue


textInput : String -> (String -> msg) -> Html msg
textInput displayText event =
    input [ type_ "textbox", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event ] []


areaInput : String -> String -> (String -> msg) -> Html msg
areaInput displayText displayValue event =
    textarea [ idAttr displayText, class "e-textarea", onChange event ] [ text displayValue ]


dropInput : String -> String -> Html msg
dropInput displayText syncfusionId =
    input [ type_ "text", id syncfusionId ] []


fileInput : RequiredType -> String -> String -> Html msg
fileInput requiredType displayText displayValue =
    div [ class "form-group" ]
        [ label [ class (labelWidth ++ "control-label " ++ isRequiredStr requiredType), for "fileName" ]
            [ text displayText ]
        , div [ class "col-sm-6 col-md-4 col-lg-3" ]
            [ input [ type_ "text", class "e-textbox", id "fileName", readonly True, value displayValue ] []
            ]
        , div [ class "col-sm-2 col-md-1 col-lg-1" ]
            [ div [ id "fileBtn" ] []
            ]
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.map handler <| Json.at [ "target", "value" ] Json.string

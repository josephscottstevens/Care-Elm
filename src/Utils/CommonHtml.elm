module Utils.CommonHtml exposing (textInput, dropInput, areaInput, fileInput, fullWidth, labelWidth, controlWidth, InputControlType(..), makeControls, getValidationErrors)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a, label, textarea)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for, name, readonly)
import Html.Events exposing (onInput, on)
import Char exposing (isLower, isUpper)
import Json.Decode as Json
import Utils.CommonTypes as CommonTypes exposing (RequiredType)


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
        CommonTypes.Required ->
            " required"

        CommonTypes.Optional ->
            ""


inputCommonFormat : RequiredType -> String -> Html msg -> Html msg
inputCommonFormat requiredType displayText t =
    div [ class "form-group" ]
        [ label [ class (labelWidth ++ "control-label" ++ isRequiredStr requiredType), forId displayText ] [ text displayText ]
        , t
        ]


type InputControlType msg
    = TextInput RequiredType String String (String -> msg)
    | AreaInput RequiredType String String (String -> msg)
    | DropInput RequiredType String String String
    | FileInput RequiredType String String


makeControls : List (InputControlType msg) -> List (Html msg)
makeControls controls =
    List.map common controls


getValidationErrors : List (InputControlType msg) -> List String
getValidationErrors controls =
    controls
        |> List.map commonValidation
        |> List.filterMap identity


commonValidation : InputControlType msg -> Maybe String
commonValidation controlType =
    case controlType of
        TextInput requiredType labelText displayValue event ->
            requiredStr displayValue labelText

        AreaInput requiredType labelText displayValue event ->
            requiredStr displayValue labelText

        DropInput requiredType labelText displayValue syncfusionId ->
            requiredStr displayValue labelText

        FileInput requiredType labelText displayValue ->
            requiredStr displayValue labelText


requiredStr : String -> String -> Maybe String
requiredStr str propName =
    if str == "" then
        Just (propName ++ " is required")
    else
        Nothing


common : InputControlType msg -> Html msg
common controlType =
    case controlType of
        TextInput requiredType labelText displayValue event ->
            inputCommonFormat requiredType labelText (commonStructure (textInput labelText event))

        AreaInput requiredType labelText displayValue event ->
            inputCommonFormat requiredType labelText (commonStructure (areaInput labelText event))

        DropInput requiredType labelText displayValue syncfusionId ->
            inputCommonFormat requiredType labelText (commonStructure (dropInput labelText syncfusionId))

        FileInput requiredType labelText displayValue ->
            fileInput requiredType labelText displayValue


textInput : String -> (String -> msg) -> Html msg
textInput displayText event =
    input [ type_ "textbox", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event ] []


areaInput : String -> (String -> msg) -> Html msg
areaInput displayText event =
    textarea [ idAttr displayText, class "e-textarea", onInput event ] []


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

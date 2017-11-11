module Utils.CommonHtml exposing (textInput, dropInput, areaInput, fileInput, fullWidth, labelWidth, controlWidth, CommonControl, commonInput, InputControlType(..), makeControls)

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
    = FileInput
    | AreaInput
    | DropInput
    | TextInput


type alias CommonControl =
    { controlType : InputControlType
    , requiredType : RequiredType
    , displayText : String
    }


commonInput : InputControlType -> RequiredType -> String -> Html msg -> Html msg
commonInput controlType requiredType displayText t =
    case controlType of
        AreaInput ->
            inputCommonFormat requiredType displayText (commonStructure t)

        TextInput ->
            inputCommonFormat requiredType displayText (commonStructure t)

        DropInput ->
            inputCommonFormat requiredType displayText (commonStructure t)

        FileInput ->
            t


makeControls : List ( InputControlType, RequiredType, String, Maybe (String -> msg) ) -> List (Html msg)
makeControls controls =
    List.map common controls


common : ( InputControlType, RequiredType, String, Maybe (String -> msg) ) -> Html msg
common ( controlType, requiredType, displayText, t ) =
    case t of
        Just event ->
            case controlType of
                TextInput ->
                    commonInput controlType requiredType displayText (textInput displayText event)

                AreaInput ->
                    commonInput controlType requiredType displayText (areaInput displayText event)

                DropInput ->
                    Debug.crash "Drop input's cannot have an input, because syncfusion will override"

                FileInput ->
                    commonInput controlType requiredType displayText (fileInput requiredType displayText)

        Nothing ->
            commonInput controlType requiredType displayText (dropInput displayText)


textInput : String -> (String -> msg) -> Html msg
textInput displayText event =
    input [ type_ "textbox", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event ] []


areaInput : String -> (String -> msg) -> Html msg
areaInput displayText event =
    input [ type_ "text", idAttr displayText, onChange event ] []


dropInput : String -> Html msg
dropInput displayText =
    input [ type_ "textarea", class "e-textbox", id "Files", name "Files" ] []


fileInput : RequiredType -> String -> Html msg
fileInput requiredType displayText =
    div [ class "form-group" ]
        [ label [ class (labelWidth ++ "control-label " ++ isRequiredStr requiredType), for "fileName" ]
            [ text displayText ]
        , div [ class "col-sm-6 col-md-4 col-lg-3" ]
            [ input [ type_ "text", class "e-textbox", id "fileName", readonly True ] []
            ]
        , div [ class "col-sm-2 col-md-1 col-lg-1" ]
            [ div [ id "fileBtn" ] []
            ]
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.map handler <| Json.at [ "target", "value" ] Json.string

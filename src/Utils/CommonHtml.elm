module Utils.CommonHtml exposing (dropInput, textInput, fileInput, hideInput, fullWidth, labelWidth, controlWidth)

import Html exposing (Html, text, div, program, button, input, span, th, li, ul, a, label)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for, name, required)
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


inputCommonFormat : RequiredType -> String -> List (Html msg) -> Html msg
inputCommonFormat isRequired displayText t =
    let
        isRequiredStr =
            case isRequired of
                CT.Required ->
                    " required"

                CT.Optional ->
                    ""
    in
        div [ class "form-group" ]
            [ label [ class (labelWidth ++ "control-label" ++ isRequiredStr), forId displayText ] [ text displayText ]
            , div [ class controlWidth ]
                t
            ]


type InputControlType
    = File
    | Textbox
    | TextArea
    | DropDown


inputCommonWithType : String -> String -> (String -> msg) -> RequiredType -> InputControlType -> Html msg
inputCommonWithType displayText inputValue event isRequired controlType =
    case controlType of
        File ->
            inputCommonFormat isRequired displayText [ input [ type_ "file", class "e-textbox", id "Files", name "Files", onChange event ] [] ]

        TextArea ->
            inputCommonFormat isRequired displayText [ input [ type_ "textarea", class "e-textbox", id "Files", name "Files" ] [] ]

        Textbox ->
            inputCommonFormat isRequired displayText [ input [ type_ "textbox", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event ] [] ]

        DropDown ->
            inputCommonFormat isRequired displayText [ input [ type_ "?", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event ] [] ]


fileInput : String -> String -> (String -> msg) -> RequiredType -> Html msg
fileInput displayText inputValue event isRequired =
    inputCommonWithType displayText inputValue event isRequired Textbox


textInput : String -> String -> (String -> msg) -> RequiredType -> Html msg
textInput displayText inputValue event isRequired =
    inputCommonWithType displayText inputValue event isRequired Textbox


hideInput : String -> String -> Html msg
hideInput displayText inputValue =
    input [ type_ "text", class "hide", idAttr displayText, nameAttr displayText, value inputValue ] [ text inputValue ]


dropInput : String -> RequiredType -> Html msg
dropInput displayText isRequired =
    inputCommonFormat isRequired displayText [ input [ type_ "text", idAttr displayText ] [] ]


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.map handler <| Json.at [ "target", "value" ] Json.string



-- x =
--     div [ class "form-group" ]
--         [ label [ class (labelWidth ++ "control-label required"), for "fileName" ]
--             [ text "Wanna file" ]
--         , div [ class "col-sm-6 col-md-4 col-lg-3" ]
--             [ input [ type_ "text", class "e-textbox", id "fileName", value newRecord.recordFile, onChange (UpdateRecordFile newRecord), readonly True ] []
--             ]
--         , div [ class "col-sm-2 col-md-1 col-lg-1" ]
--             [ div [ id "fileBtn" ] []
--             ]
--         ]

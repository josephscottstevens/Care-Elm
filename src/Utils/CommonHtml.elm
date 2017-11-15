module Utils.CommonHtml exposing (textInput, dropInput, areaInput, fileInput, fullWidth, labelWidth, controlWidth, InputControlType(..), makeControls, getValidationErrors)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a, label, textarea)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for, name, readonly)
import Html.Events exposing (onInput, on)
import Json.Decode as Json
import Utils.CommonFunctions exposing (..)


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


type InputControlType msg
    = RequiredTextInput String (String -> msg)
    | RequiredNumrInput Int (String -> msg)
    | RequiredAreaInput String (String -> msg)
    | RequiredDropInput String String
    | RequiredFileInput String
    | OptionalTextInput (Maybe String) (String -> msg)
    | OptionalNumrInput (Maybe Int) (String -> msg)
    | OptionalAreaInput (Maybe String) (String -> msg)
    | OptionalDropInput (Maybe String) String
    | OptionalFileInput String


makeControls : List (String -> InputControlType msg) -> List (Html msg)
makeControls controls =
    [ div [] [] ]



-- List.map common controls


getValidationErrors : List ( String, InputControlType msg ) -> List String
getValidationErrors controls =
    controls
        |> List.map commonValidation
        |> List.filterMap identity


commonValidation : ( String, InputControlType msg ) -> Maybe String
commonValidation ( labelText, controlType ) =
    case controlType of
        RequiredTextInput displayValue _ ->
            requiredStr labelText displayValue

        RequiredAreaInput displayValue _ ->
            requiredStr labelText displayValue

        RequiredDropInput displayValue _ ->
            requiredStr labelText displayValue

        RequiredFileInput displayValue ->
            requiredStr labelText displayValue

        _ ->
            Nothing


requiredStr : String -> String -> Maybe String
requiredStr displayValue str =
    if str == "" then
        Just (displayValue ++ " is required")
    else
        Nothing



-- isRequiredStr : RequiredType -> String
-- isRequiredStr requiredType =
--     case requiredType of
--         CommonTypes.Required ->
--             " required"
--         CommonTypes.Optional ->
--             ""


inputCommonFormat : String -> List (Html msg) -> Html msg
inputCommonFormat displayText t =
    let
        firstItem =
            label [ class (labelWidth ++ "control-label"), forId displayText ] [ text displayText ]
    in
        div [ class "form-group" ]
            (firstItem :: t)


common : ( String, InputControlType msg ) -> Html msg
common ( labelText, controlType ) =
    let
        commonStructure =
            div [ class controlWidth ]

        commonFormat =
            inputCommonFormat labelText
    in
        case controlType of
            RequiredTextInput displayValue event ->
                commonFormat [ commonStructure [ textInput labelText displayValue event ] ]

            -- RequiredNumrInput displayValue event ->
            --     commonFormat commonStructure [ numrInput labelText displayValue event ]
            -- RequiredAreaInput displayValue event ->
            --     commonFormat commonStructure [ areaInput labelText displayValue event ]
            -- RequiredDropInput displayValue syncfusionId ->
            --     commonFormat commonStructure [ dropInput labelText displayValue syncfusionId ]
            RequiredFileInput displayValue ->
                commonFormat [ fileInput labelText displayValue ]

            _ ->
                div [] []


textInput : String -> String -> (String -> msg) -> Html msg
textInput displayText displayValue event =
    input [ type_ "textbox", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event, value displayValue ] []


numrInput : String -> Int -> (String -> msg) -> Html msg
numrInput displayText displayValue event =
    input [ type_ "number", class "e-textbox", idAttr displayText, nameAttr displayText, onInput event, value (toString displayValue) ] []


areaInput : String -> String -> (String -> msg) -> Html msg
areaInput displayText displayValue event =
    textarea [ idAttr displayText, class "e-textarea", onInput event, value displayValue ] []


dropInput : String -> String -> String -> Html msg
dropInput displayText displayValue syncfusionId =
    input [ type_ "text", id syncfusionId, value displayValue ] []


fileInput : String -> String -> Html msg
fileInput displayText displayValue =
    div [ class "form-group" ]
        [ label [ class (labelWidth ++ "control-label "), for "fileName" ]
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

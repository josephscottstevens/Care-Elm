module Utils.CommonHtml exposing (fullWidth, labelWidth, controlWidth, InputControlType(..), makeControls, getValidationErrors)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a, label, textarea)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for, name, readonly)
import Html.Events exposing (onInput, on)
import Json.Decode as Json
import Utils.CommonFunctions exposing (..)
import Utils.CommonTypes exposing (..)


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
    = TextInput String (String -> msg)
    | NumrInput Int (String -> msg)
    | AreaInput String (String -> msg)
    | DropInput (Maybe Int) String
    | DateInput String String
    | FileInput String


makeControls : List ( String, RequiredType, InputControlType msg ) -> List (Html msg)
makeControls controls =
    List.map common controls


getValidationErrors : List ( String, RequiredType, InputControlType msg ) -> List String
getValidationErrors controls =
    controls
        |> List.map commonValidation
        |> List.filterMap identity


commonValidation : ( String, RequiredType, InputControlType msg ) -> Maybe String
commonValidation ( labelText, requiredType, controlType ) =
    case controlType of
        TextInput displayValue _ ->
            requiredStr labelText displayValue

        AreaInput displayValue _ ->
            requiredStr labelText displayValue

        DropInput displayValue _ ->
            case displayValue of
                Just t ->
                    Nothing

                Nothing ->
                    Just (labelText ++ "is required")

        FileInput displayValue ->
            requiredStr labelText displayValue

        _ ->
            Nothing


requiredStr : String -> String -> Maybe String
requiredStr labelText str =
    if str == "" then
        Just (labelText ++ " is required")
    else
        Nothing


isRequiredStr : RequiredType -> String
isRequiredStr requiredType =
    case requiredType of
        Required ->
            " required"

        Optional ->
            ""


inputCommonFormat : String -> RequiredType -> List (Html msg) -> Html msg
inputCommonFormat displayText requiredType t =
    let
        firstItem =
            label [ class (labelWidth ++ "control-label" ++ isRequiredStr requiredType), forId displayText ] [ text displayText ]
    in
        div [ class "form-group" ]
            (firstItem :: t)


common : ( String, RequiredType, InputControlType msg ) -> Html msg
common ( labelText, requiredType, controlType ) =
    let
        commonStructure t =
            inputCommonFormat labelText requiredType [ div [ class controlWidth ] t ]
    in
        case controlType of
            TextInput displayValue event ->
                commonStructure
                    [ input [ type_ "textbox", class "e-textbox", idAttr labelText, nameAttr labelText, onInput event, value displayValue ] []
                    ]

            NumrInput displayValue event ->
                commonStructure
                    [ input [ type_ "number", class "e-textbox", idAttr labelText, nameAttr labelText, onInput event, value (toString displayValue) ] []
                    ]

            AreaInput displayValue event ->
                commonStructure
                    [ textarea [ idAttr labelText, class "e-textarea", onInput event, value displayValue ] []
                    ]

            DropInput displayValue syncfusionId ->
                commonStructure
                    [ input [ type_ "text", id syncfusionId, value (toString displayValue) ] []
                    ]

            DateInput displayValue syncfusionId ->
                commonStructure
                    [ input [ type_ "text", id syncfusionId, value displayValue ] []
                    ]

            FileInput displayValue ->
                div [ class "form-group" ]
                    [ label [ class (labelWidth ++ "control-label " ++ isRequiredStr requiredType), for "fileName" ]
                        [ text labelText ]
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

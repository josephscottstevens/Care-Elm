module Common.Html exposing (fullWidth, labelWidth, controlWidth, InputControlType(..), makeControls, getValidationErrors, defaultConfig)

import Html exposing (Html, text, div, button, input, label, textarea)
import Html.Attributes exposing (class, type_, id, value, for, name, readonly, style, checked)
import Html.Events exposing (onInput, onCheck)
import Common.Functions exposing (..)
import Common.Types exposing (..)


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


checkStyle : Html.Attribute msg
checkStyle =
    style [ ( "height", "20px" ), ( "width", "20px" ), ( "margin-top", "2px" ) ]


type alias Config msg =
    { controlAttributes : List (Html.Attribute msg)
    }


defaultConfig : Config msg
defaultConfig =
    { controlAttributes = [ class controlWidth ]
    }


type InputControlType msg
    = TextInput String RequiredType String (String -> msg)
    | NumrInput String RequiredType Int (String -> msg)
    | CheckInput String RequiredType Bool (Bool -> msg)
    | AreaInput String RequiredType String (String -> msg)
    | KnockInput String RequiredType String
    | DropInput String RequiredType (Maybe Int) String
    | DropInputWithButton String RequiredType (Maybe Int) String String
    | DateInput String RequiredType String String
    | FileInput String RequiredType String
    | HtmlElement (Html msg)


makeControls : Config msg -> List (InputControlType msg) -> Html msg
makeControls config controls =
    let
        common controlType =
            case controlType of
                TextInput labelText requiredType displayValue event ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div config.controlAttributes
                            [ input [ type_ "textbox", class "e-textbox", nameAttr labelText, onInput event, value displayValue ] []
                            ]
                        ]

                NumrInput labelText requiredType displayValue event ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div config.controlAttributes
                            [ input [ type_ "number", class "e-textbox", nameAttr labelText, onInput event, value <| toString displayValue ] []
                            ]
                        ]

                CheckInput labelText requiredType displayValue event ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div config.controlAttributes
                            [ input [ type_ "checkbox", checkStyle, nameAttr labelText, onCheck event, checked displayValue ] []
                            ]
                        ]

                AreaInput labelText requiredType displayValue event ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div config.controlAttributes
                            [ textarea [ idAttr labelText, class "e-textbox", onInput event, value displayValue ] [] ]
                        ]

                DropInput labelText requiredType _ syncfusionId ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div config.controlAttributes
                            [ input [ type_ "text", id syncfusionId ] [] ]
                        ]

                KnockInput labelText _ syncfusionId ->
                    div [ id syncfusionId ] []

                DropInputWithButton labelText requiredType _ syncfusionId buttonText ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div [ class controlWidth ] [ input [ type_ "text", id syncfusionId ] [] ]
                        , div [ class labelWidth ] [ button [ class "btn btn-sm btn-default" ] [ text buttonText ] ]
                        ]

                DateInput labelText requiredType displayValue syncfusionId ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div config.controlAttributes
                            [ input [ type_ "text", id syncfusionId, value displayValue, value displayValue ] [] ]
                        ]

                FileInput labelText requiredType displayValue ->
                    div [ class "form-group" ]
                        [ commonLabel labelText requiredType
                        , div config.controlAttributes
                            [ input [ type_ "text", class "e-textbox", id "fileName", readonly True, value displayValue ] [] ]
                        , div [ class labelWidth ] [ div [ id "fileBtn" ] [] ]
                        ]

                HtmlElement htmlElement ->
                    div [ class "form-group" ]
                        [ commonLabel "" Optional
                        , div config.controlAttributes
                            [ htmlElement ]
                        ]
    in
        div [] (controls |> List.map common)


commonLabel : String -> RequiredType -> Html msg
commonLabel labelText requiredType =
    let
        lastChar =
            String.right 1 labelText

        formattedLabelText =
            if lastChar == ":" then
                labelText
            else if labelText == "" then
                ""
            else
                labelText ++ ":"
    in
        label [ class (labelWidth ++ " " ++ isRequiredStr requiredType), forId labelText ] [ text formattedLabelText ]


getValidationErrors : List (InputControlType msg) -> List String
getValidationErrors controls =
    controls
        |> List.map commonValidation
        |> List.filterMap identity


is : RequiredType -> Maybe a -> Maybe a
is requiredType t =
    case requiredType of
        Required ->
            t

        Optional ->
            Nothing


commonValidation : InputControlType msg -> Maybe String
commonValidation controlType =
    case controlType of
        TextInput labelText requiredType displayValue _ ->
            is requiredType <| requiredStr labelText displayValue

        AreaInput labelText requiredType displayValue _ ->
            is requiredType <| requiredStr labelText displayValue

        DropInput labelText requiredType displayValue _ ->
            is requiredType <|
                case displayValue of
                    Just _ ->
                        Nothing

                    Nothing ->
                        Just (labelText ++ " is required")

        DateInput labelText requiredType displayValue _ ->
            is requiredType <| requiredStr labelText displayValue

        FileInput labelText requiredType displayValue ->
            is requiredType <| requiredStr labelText displayValue

        NumrInput labelText requiredType displayValue _ ->
            is requiredType (requiredStr labelText (toString displayValue))

        CheckInput _ _ _ _ ->
            Nothing

        KnockInput labelText requiredType displayValue ->
            is requiredType <| requiredStr labelText displayValue

        DropInputWithButton labelText requiredType displayValue _ _ ->
            is requiredType <|
                case displayValue of
                    Just _ ->
                        Nothing

                    Nothing ->
                        Just (labelText ++ " is required")

        HtmlElement _ ->
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
            label [ class (labelWidth ++ " " ++ isRequiredStr requiredType), forId displayText ] [ text displayText ]
    in
        div [ class "form-group" ]
            (firstItem :: t)

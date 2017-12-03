module Common.Html exposing (fullWidth, labelWidth, controlWidth, InputControlType(..), makeControls, getValidationErrors)

import Html exposing (Html, text, div, button, input, label, textarea)
import Html.Attributes exposing (class, type_, id, value, for, name, readonly, style, checked)
import Html.Events exposing (onInput, onClick, onCheck)
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


type InputControlType msg
    = TextInput String (String -> msg)
    | NumrInput Int (String -> msg)
    | CheckInput Bool (Bool -> msg)
    | AreaInput String (String -> msg)
    | KnockInput String
    | DropInput (Maybe Int) String
    | DropInputWithButton (Maybe Int) String String
    | DateInput String String (Maybe String -> msg)
    | FileInput String


makeControls : List ( String, RequiredType, InputControlType msg ) -> Html msg
makeControls controls =
    div [] (List.map common controls)


isRequired : ( String, RequiredType, InputControlType msg ) -> Bool
isRequired ( _, requiredType, _ ) =
    case requiredType of
        Required ->
            True

        Optional ->
            False


getValidationErrors : List ( String, RequiredType, InputControlType msg ) -> List String
getValidationErrors controls =
    controls
        |> List.filter isRequired
        |> List.map commonValidation
        |> List.filterMap identity


commonValidation : ( String, RequiredType, InputControlType msg ) -> Maybe String
commonValidation ( labelText, _, controlType ) =
    case controlType of
        TextInput displayValue _ ->
            requiredStr labelText displayValue

        AreaInput displayValue _ ->
            requiredStr labelText displayValue

        DropInput displayValue _ ->
            case displayValue of
                Just _ ->
                    Nothing

                Nothing ->
                    Just (labelText ++ " is required")

        DateInput displayValue _ _ ->
            requiredStr labelText displayValue

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
                    [ input [ type_ "textbox", class "e-textbox", nameAttr labelText, onInput event, value displayValue ] []
                    ]

            NumrInput displayValue event ->
                commonStructure
                    [ input [ type_ "number", class "e-textbox", nameAttr labelText, onInput event, value (toString displayValue) ] []
                    ]

            CheckInput displayValue event ->
                commonStructure
                    [ input [ type_ "checkbox", onCheck event, checkStyle, checked displayValue ] []
                    ]

            AreaInput displayValue event ->
                commonStructure
                    [ textarea [ idAttr labelText, class "e-textarea", onInput event, value displayValue ] []
                    ]

            DropInput _ syncfusionId ->
                commonStructure
                    [ input [ type_ "text", id syncfusionId ] []
                    ]

            KnockInput syncfusionId ->
                div [ id syncfusionId ] []

            DropInputWithButton _ syncfusionId buttonText ->
                div [ class "form-group" ]
                    [ label [ class (labelWidth ++ "control-label" ++ isRequiredStr requiredType), forId labelText ] [ text labelText ]
                    , div [ class controlWidth ] [ input [ type_ "text", id syncfusionId ] [] ]
                    , div [ class labelWidth ] [ button [ class "btn btn-sm btn-default" ] [ text buttonText ] ]
                    ]

            DateInput displayValue syncfusionId event ->
                commonStructure
                    [ input [ type_ "text", id syncfusionId, value displayValue, onInput (defaultMaybeMsg event), value displayValue ] []
                    ]

            FileInput displayValue ->
                div [ class "form-group" ]
                    [ label [ class (labelWidth ++ "control-label " ++ isRequiredStr requiredType), for "fileName" ] [ text labelText ]
                    , div [ class controlWidth ] [ input [ type_ "text", class "e-textbox", id "fileName", readonly True, value displayValue ] [] ]
                    , div [ class labelWidth ] [ div [ id "fileBtn" ] [] ]
                    ]

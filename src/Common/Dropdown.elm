module Common.Dropdown exposing (Dropdown, init, Msg(..), update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Common.Types exposing (DropdownItem)


type alias Dropdown =
    { isOpen : Bool
    , dropdownItem : DropdownItem
    , dropdownSource : List DropdownItem
    }


init : List DropdownItem -> Maybe Int -> String -> Dropdown
init list dropId dropVal =
    Dropdown False (DropdownItem dropId dropVal) list


type Msg
    = ItemPicked DropdownItem
    | SetOpenState Bool


update : Msg -> Dropdown -> Dropdown
update msg dropdown =
    case msg of
        ItemPicked item ->
            Dropdown False item dropdown.dropdownSource

        SetOpenState newState ->
            Dropdown newState dropdown.dropdownItem dropdown.dropdownSource


view : Dropdown -> Html Msg
view dropdown =
    let
        displayStyle =
            if dropdown.isOpen then
                ( "display", "block" )
            else
                ( "display", "none" )

        activeClass =
            if dropdown.isOpen then
                "e-focus e-popactive"
            else
                ""

        mainAttr =
            case dropdown.dropdownSource of
                [] ->
                    [ style <| dropdownDisabled ++ dropdownInput
                    ]

                _ ->
                    [ style dropdownInput
                    , onClick <| SetOpenState <| not dropdown.isOpen
                    ]

        numItems =
            dropdown.dropdownSource
                |> List.map (\t -> String.length t.name)
                |> List.sortBy identity
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 150

        dropInputWidth =
            style [ ( "width", "100%" ) ]
    in
        div []
            [ span [ onClick <| SetOpenState <| not dropdown.isOpen, class ("e-ddl e-widget " ++ activeClass), dropInputWidth ]
                [ span
                    [ class "e-in-wrap e-box" ]
                    [ input [ class "e-input", readonly True, value dropdown.dropdownItem.name ] []
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , ul [ style <| displayStyle :: dropdownList, class "dropdown-ul" ] (List.map (viewItem numItems) dropdown.dropdownSource)
            ]


viewItem : Int -> DropdownItem -> Html Msg
viewItem numItems item =
    let
        width =
            numItems * 6

        dropLiStyle =
            style [ ( "width", toString width ++ "px" ) ]
    in
        li [ onClick (ItemPicked item), class "dropdown-li", dropLiStyle ]
            [ text item.name ]


onClick : msg -> Attribute msg
onClick message =
    onWithOptions "click"
        { stopPropagation = True, preventDefault = False }
        (Json.succeed message)



-- styles for dropdown container


dropdownContainer : List ( String, String )
dropdownContainer =
    [ ( "position", "relative" )
    , ( "margin", "16px" )

    -- , ( "width", "152px" )
    , ( "display", "inline-block" )
    , ( "fontFamily", "sans-serif" )
    , ( "fontSize", "16px" )
    ]



-- styles for main input field


dropdownInput : List ( String, String )
dropdownInput =
    [ ( "padding", "6px 12px 8px 15px" )
    , ( "margin", "0" )
    , ( "border", "1px solid rgba(0,0,0,.17)" )
    , ( "border-radius", "4px" )
    , ( "background-color", "white" )
    , ( "display", "flex" )
    , ( "alignItems", "center" )
    ]



-- disabled style


dropdownDisabled : List ( String, String )
dropdownDisabled =
    [ ( "color", "rgba(0,0,0,.54" ) ]



-- styles for the text of selected item


dropdownText : List ( String, String )
dropdownText =
    [ ( "flex", "1 0 auto" ) ]



-- styles for list container


dropdownList : List ( String, String )
dropdownList =
    [ ( "position", "absolute" )
    , ( "top", "32px" )
    , ( "border-radius", "4px" )
    , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
    , ( "padding", "0" )
    , ( "margin", "0" )

    -- , ( "width", "150px" )
    , ( "background-color", "white" )
    , ( "max-height", "152px" )
    , ( "overflow-x", "hidden" )
    , ( "overflow-y", "scroll" )
    , ( "z-index", "100" )
    ]

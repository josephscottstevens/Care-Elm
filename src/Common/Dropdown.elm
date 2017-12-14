module Common.Dropdown exposing (Model, init, Msg(..), update, view, mainContainer)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Common.Types exposing (DropdownItem)


type Model
    = Model
        { selectedItem : DropdownItem
        , isOpen : Bool
        }


init : ( Bool, DropdownItem )
init =
    ( False, DropdownItem Nothing "" )


type Msg
    = ItemPicked DropdownItem
    | SetOpenState Bool


update : Msg -> ( Bool, DropdownItem ) -> ( Bool, DropdownItem )
update msg ( isOpen, dropdownItem ) =
    case msg of
        ItemPicked item ->
            ( False, item )

        SetOpenState newState ->
            ( newState, DropdownItem Nothing "" )


view : ( Bool, DropdownItem ) -> List DropdownItem -> Html Msg
view ( isOpen, dropdownItem ) data =
    let
        displayStyle =
            if isOpen then
                ( "display", "block" )
            else
                ( "display", "none" )

        activeClass =
            if isOpen then
                "e-focus e-popactive"
            else
                ""

        mainAttr =
            case data of
                [] ->
                    [ style <| dropdownDisabled ++ dropdownInput
                    ]

                _ ->
                    [ style dropdownInput
                    , onClick <| SetOpenState <| not isOpen
                    ]
    in
        div [ style dropdownContainer ]
            [ span
                [ onClick <| SetOpenState <| not isOpen, class ("e-ddl e-widget " ++ activeClass), style [ ( "width", "152px" ) ] ]
                [ span [ class "e-in-wrap e-box" ]
                    [ input [ class "e-input", readonly True, value dropdownItem.name ] []
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , div []
                [ ul [ style <| displayStyle :: dropdownList, class "dropdown-ul" ] (List.map viewItem data)
                ]
            ]


viewItem : DropdownItem -> Html Msg
viewItem item =
    li [ onClick (ItemPicked item), class "dropdown-li" ]
        [ text item.name ]


onClick : msg -> Attribute msg
onClick message =
    onWithOptions "click"
        { stopPropagation = True, preventDefault = False }
        (Json.succeed message)



-- styles


mainContainer : List ( String, String )
mainContainer =
    [ ( "height", "100%" )
    , ( "background-color", "#fafafa" )
    , ( "padding", "16px" )
    ]



-- styles for dropdown container


dropdownContainer : List ( String, String )
dropdownContainer =
    [ ( "position", "relative" )
    , ( "margin", "16px" )
    , ( "width", "152px" )
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
    , ( "width", "150px" )
    , ( "background-color", "white" )
    , ( "max-height", "152px" )
    , ( "overflow-x", "hidden" )
    , ( "overflow-y", "scroll" )
    ]

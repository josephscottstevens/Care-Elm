module Common.Dropdown exposing (Dropdown, Msg, init, update, view)

import Html exposing (Html, Attribute, div, span, text, li, ul, input)
import Html.Attributes exposing (style, value, class, readonly)
import Html.Events exposing (onWithOptions, onBlur, onMouseEnter)
import Json.Decode as Json
import Common.Types exposing (DropdownItem)


type alias Dropdown =
    { isOpen : Bool
    , dropdownItem : DropdownItem
    , dropdownSource : List DropdownItem
    }


init : List DropdownItem -> Maybe Int -> String -> Dropdown
init list dropId dropVal =
    { isOpen = False
    , dropdownItem = DropdownItem dropId dropVal
    , dropdownSource = list
    }


type Msg
    = ItemPicked DropdownItem
    | ItemEntered DropdownItem
    | SetOpenState Bool
    | OnBlur


update : Msg -> Dropdown -> Dropdown
update msg dropdown =
    case msg of
        ItemPicked item ->
            { dropdown | dropdownItem = item, isOpen = False }

        ItemEntered item ->
            { dropdown | dropdownItem = item }

        SetOpenState newState ->
            { dropdown | isOpen = newState }

        OnBlur ->
            { dropdown | isOpen = False }


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
            [ span
                [ onClick (SetOpenState (not dropdown.isOpen))
                , class ("e-ddl e-widget " ++ activeClass)
                , dropInputWidth
                ]
                [ span
                    [ class "e-in-wrap e-box" ]
                    [ input [ class "e-input", readonly True, value dropdown.dropdownItem.name, onBlur OnBlur ] []
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
        li
            [ onClick (ItemPicked item)
            , onMouseEnter (ItemEntered item)
            , class "dropdown-li"
            , dropLiStyle
            ]
            [ text item.name ]


onClick : msg -> Attribute msg
onClick message =
    onWithOptions "click"
        { stopPropagation = True, preventDefault = False }
        (Json.succeed message)



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

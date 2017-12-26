port module Common.Dropdown exposing (Dropdown, Msg, init, update, view)

import Html exposing (Html, Attribute, div, span, text, li, ul, input)
import Html.Attributes exposing (style, value, class, readonly)
import Html.Events exposing (onWithOptions, onBlur, onMouseEnter)
import Json.Decode
import Common.Types exposing (DropdownItem)
import Common.Functions as Functions
import Char


port dropdownMenuScroll : String -> Cmd msg


scrollToDomId : String -> Cmd msg
scrollToDomId =
    dropdownMenuScroll


type alias Dropdown =
    { isOpen : Bool
    , selectedItem : DropdownItem
    , highlightedItemMouse : Maybe DropdownItem
    , highlightedItemKeyboard : Maybe DropdownItem
    , selectedIndex : Int
    , dropdownSource : List DropdownItem
    , searchString : String
    , id : String
    }


init : String -> List DropdownItem -> Maybe DropdownItem -> Dropdown
init id list selectedItem =
    case selectedItem of
        Just t ->
            { isOpen = False
            , selectedItem = t
            , highlightedItemMouse = Nothing
            , highlightedItemKeyboard = Nothing
            , dropdownSource = list
            , searchString = ""
            , id = id
            , selectedIndex = 0
            }

        Nothing ->
            { isOpen = False
            , selectedItem = DropdownItem Nothing ""
            , highlightedItemMouse = Nothing
            , highlightedItemKeyboard = Nothing
            , dropdownSource = list
            , searchString = ""
            , id = id
            , selectedIndex = 0
            }


type Key
    = Esc
    | Enter
    | ArrowUp
    | ArrowDown
    | PageUp
    | PageDown
    | Home
    | End
    | Searchable Char


type Msg
    = ItemPicked DropdownItem
    | ItemEntered DropdownItem
    | SetOpenState Bool
    | OnBlur
    | OnKey Key


update : Msg -> Dropdown -> ( Dropdown, Cmd msg )
update msg dropdown =
    let
        pickHighlight selectedItem =
            case selectedItem of
                Just t ->
                    t

                Nothing ->
                    DropdownItem Nothing ""
    in
        case msg of
            ItemPicked item ->
                { dropdown | selectedItem = item, isOpen = False } ! []

            ItemEntered item ->
                { dropdown | highlightedItemMouse = Just item } ! []

            SetOpenState newState ->
                { dropdown | isOpen = newState } ! []

            OnBlur ->
                { dropdown
                    | isOpen = False
                    , selectedItem = pickHighlight dropdown.highlightedItemMouse
                }
                    ! []

            OnKey Esc ->
                { dropdown | isOpen = False } ! []

            OnKey Enter ->
                -- case maybeOpenState config model of
                --     Nothing ->
                --         maybeSelectionId config model |> updateHighlight
                --     Just openState ->
                --         updateSelect openState.maybeHighlightId
                { dropdown
                    | selectedItem =
                        pickHighlight (dropdown.dropdownSource |> List.reverse |> List.head)
                }
                    ! []

            OnKey ArrowUp ->
                pickerSkip 1 (dropdown.dropdownSource |> List.reverse) dropdown

            OnKey ArrowDown ->
                pickerSkip 1 dropdown.dropdownSource dropdown

            OnKey PageUp ->
                pickerSkip 9 (dropdown.dropdownSource |> List.reverse) dropdown

            OnKey PageDown ->
                pickerSkip 9 dropdown.dropdownSource dropdown

            OnKey Home ->
                { dropdown
                    | selectedItem =
                        pickHighlight (dropdown.dropdownSource |> List.head)
                }
                    ! []

            OnKey End ->
                { dropdown
                    | selectedItem =
                        pickHighlight (dropdown.dropdownSource |> List.reverse |> List.head)
                }
                    ! []

            OnKey (Searchable char) ->
                updateSearchString char dropdown


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

        keyMsgDecoder =
            Html.Events.keyCode
                |> Json.Decode.andThen (keyDecoder dropdown)
                |> Json.Decode.map OnKey
    in
        div [ Html.Events.onWithOptions "keydown" { stopPropagation = True, preventDefault = True } keyMsgDecoder ]
            [ span
                [ onClick (SetOpenState (not dropdown.isOpen))
                , class ("e-ddl e-widget " ++ activeClass)
                , dropInputWidth
                ]
                [ span
                    [ class "e-in-wrap e-box" ]
                    [ input [ class "e-input", readonly True, value dropdown.selectedItem.name, onBlur OnBlur ] []
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , ul [ style <| displayStyle :: dropdownList, class "dropdown-ul" ] (List.map (viewItem dropdown numItems) dropdown.dropdownSource)
            ]


getId : String -> DropdownItem -> String
getId id item =
    id ++ "-" ++ Functions.defaultIntToString item.id


viewItem : Dropdown -> Int -> DropdownItem -> Html Msg
viewItem dropdown numItems item =
    let
        width =
            numItems * 6

        commonWidth =
            ( "width", toString width ++ "px" )

        dropLiStyle =
            if dropdown.highlightedItemKeyboard == Just item then
                style [ commonWidth, ( "color", "green" ) ]
            else if dropdown.highlightedItemMouse == Just item then
                style [ commonWidth, ( "background-color", "red" ) ]
            else
                style [ commonWidth ]
    in
        li
            [ onClick (ItemPicked item)
            , onMouseEnter (ItemEntered item)
            , class "dropdown-li"
            , dropLiStyle
            , Html.Attributes.id (getId dropdown.id item)
            ]
            [ text item.name ]


onClick : msg -> Attribute msg
onClick message =
    onWithOptions "click"
        { stopPropagation = True, preventDefault = False }
        (Json.Decode.succeed message)



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


pickerSkip : Int -> List DropdownItem -> Dropdown -> ( Dropdown, Cmd msg )
pickerSkip skipCount dropdownItems dropdown =
    case dropdown.highlightedItemKeyboard of
        Nothing ->
            case List.head dropdownItems of
                Just t ->
                    { dropdown | highlightedItemKeyboard = List.head dropdownItems } ! [ scrollToDomId (getId dropdown.id t) ]

                Nothing ->
                    Debug.crash "1"

        Just selected ->
            let
                selectedItem =
                    dropdownItems
                        |> List.filter (\t -> t == selected)
                        |> List.take skipCount
                        |> List.head
            in
                case selectedItem of
                    Just t ->
                        { dropdown | highlightedItemKeyboard = selectedItem } ! [ scrollToDomId (getId dropdown.id t) ]

                    Nothing ->
                        Debug.crash "2"


maybeFallback : Maybe a -> Maybe a -> Maybe a
maybeFallback replacement original =
    case original of
        Just _ ->
            original

        Nothing ->
            replacement


updateSearchString : Char -> Dropdown -> ( Dropdown, Cmd msg )
updateSearchString searchChar dropdown =
    let
        searchString =
            -- Manage backspace character
            if searchChar == '\x08' then
                String.dropRight 1 dropdown.searchString
            else
                dropdown.searchString ++ String.toLower (String.fromChar searchChar)

        maybeSelectedItem =
            dropdown.dropdownSource
                |> List.filter (\t -> String.startsWith searchString (String.toLower t.name))
                |> List.head
    in
        case maybeSelectedItem of
            Just t ->
                { dropdown
                    | selectedItem = t
                    , searchString = searchString
                }
                    ! [ scrollToDomId (getId dropdown.id t) ]

            Nothing ->
                dropdown ! [ Cmd.none ]


keyDecoder : Dropdown -> Int -> Json.Decode.Decoder Key
keyDecoder dropdown keyCode =
    let
        -- This is necessary to ensure that the key is not consumed and can propagate to the parent
        pass =
            Json.Decode.fail ""

        key =
            Json.Decode.succeed
    in
        case keyCode of
            13 ->
                key Enter

            27 ->
                -- Consume Esc only if the Menu is open
                if dropdown.isOpen then
                    pass
                else
                    key Esc

            -- 32 ->
            --     key Space
            33 ->
                key PageUp

            34 ->
                key PageDown

            35 ->
                key End

            36 ->
                key Home

            38 ->
                key ArrowUp

            40 ->
                key ArrowDown

            _ ->
                let
                    char =
                        Char.fromCode keyCode

                    -- TODO should the user be able to search non-alphanum chars?
                    -- TODO add support for non-ascii alphas
                    isAlpha char =
                        (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
                in
                    -- Backspace is "searchable" because it can be used to modify the search string
                    if isAlpha char || Char.isDigit char || char == '\x08' then
                        key (Searchable char)
                    else
                        pass

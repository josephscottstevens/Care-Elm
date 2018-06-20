port module Common.Dropdown
    exposing
        ( DropConfig
        , DropState
        , Size(..)
        , defaultDropConfig
        , getDropdownText
        , init
        , view
        , viewWithEnabled
        )

import Char
import Common.Dialog as Dialog
import Common.DomHelper as DOM
import Common.Functions as Functions
import Common.Types exposing (DropdownItem)
import Html exposing (Html, div, input, li, span, text, ul)
import Html.Attributes exposing (class, classList, defaultValue, disabled, hidden, placeholder, readonly, style, tabindex)
import Html.Events as Events
import Json.Decode as Decode


port dropdownMenuScroll : String -> Cmd msg



-- port getBoundingRect : String -> Cmd msg
-- port updateBoundingRect : (BoundingRect -> msg) -> Sub msg


port focusOn : String -> Cmd msg


scrollToDomId : String -> Maybe Int -> Cmd msg
scrollToDomId str id =
    dropdownMenuScroll (str ++ "-" ++ Functions.defaultIntToString id)


type alias DropState =
    { isOpen : Bool
    , keyboardSelectedIndex : Int
    , searchString : String
    , domId : String
    , showSearchText : Bool
    , width : Size
    , height : Size
    , y : Float
    }


type Size
    = Exactly Int
    | Auto


type alias DropConfig =
    { domId : String
    , showSearchText : Bool
    , width : Size
    , height : Size
    }


defaultDropConfig : DropConfig
defaultDropConfig =
    { domId = ""
    , showSearchText = False
    , width = Auto
    , height = Auto
    }



--( DropState, (BoundingRect -> msg) -> Sub msg )


init : DropConfig -> DropState
init { domId, showSearchText, width, height } =
    { isOpen = False
    , keyboardSelectedIndex = 0
    , searchString = ""
    , domId = domId
    , showSearchText = showSearchText
    , width = width
    , height = height
    , y = 0.0
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


type SkipAmount
    = First
    | Last
    | Exact Int


getIndex : Maybe Int -> List DropdownItem -> Int
getIndex selectedId dropdownItems =
    dropdownItems
        |> List.indexedMap
            (\idx t ->
                if t.id == selectedId then
                    Just idx
                else
                    Nothing
            )
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault 0


getId : List DropdownItem -> Int -> Maybe Int
getId dropdownItems index =
    dropdownItems
        |> Functions.getAt index
        |> Maybe.map .id
        |> Maybe.withDefault Nothing


onBlur : DropState -> Maybe Int -> String -> ( DropState, Maybe Int, Cmd msg )
onBlur dropState selectedId customTargetId =
    if customTargetId == (dropState.domId ++ "filterInput") || customTargetId == (dropState.domId ++ "root") then
        ( dropState
        , selectedId
        , Cmd.none
        )
    else
        ( { dropState | isOpen = False, searchString = "" }
        , selectedId
        , Cmd.none
        )


onClick : DropState -> Maybe Int -> List DropdownItem -> CustomTarget -> ( DropState, Maybe Int, Cmd msg )
onClick dropdown selectedId dropdownItems customTarget =
    if dropdown.isOpen then
        if customTarget.className == "noselect dropdown-li" then
            ( { dropdown | isOpen = False, searchString = "", y = customTarget.rect.top }
            , Functions.stringToInt (String.slice (String.length dropdown.domId + 1) 9999 customTarget.id)
            , Cmd.none
            )
        else
            ( dropdown
            , selectedId
            , Cmd.none
            )
    else
        ( { dropdown
            | isOpen = True
            , keyboardSelectedIndex = getIndex selectedId dropdownItems
            , y = customTarget.rect.top
          }
        , selectedId
        , Cmd.batch
            [ scrollToDomId dropdown.domId selectedId
            , focusOn (dropdown.domId ++ "filterInput")
            ]
        )


onKey : DropState -> Maybe Int -> List DropdownItem -> Key -> ( DropState, Maybe Int, Cmd msg )
onKey dropdown selectedId dropdownItems key =
    case key of
        Esc ->
            ( { dropdown | isOpen = False, searchString = "" }, selectedId, Cmd.none )

        Enter ->
            let
                newDropdown =
                    { dropdown | isOpen = False, searchString = "" }
            in
            if dropdown.isOpen then
                ( newDropdown, getId dropdownItems dropdown.keyboardSelectedIndex, Cmd.none )
            else
                ( newDropdown, selectedId, Cmd.none )

        ArrowUp ->
            pickerSkip dropdown (Exact -1) dropdownItems selectedId

        ArrowDown ->
            pickerSkip dropdown (Exact 1) dropdownItems selectedId

        PageUp ->
            pickerSkip dropdown (Exact -9) dropdownItems selectedId

        PageDown ->
            pickerSkip dropdown (Exact 9) dropdownItems selectedId

        Home ->
            pickerSkip dropdown First dropdownItems selectedId

        End ->
            pickerSkip dropdown Last dropdownItems selectedId

        Searchable char ->
            updateSearchString char dropdown dropdownItems selectedId


pickerSkip : DropState -> SkipAmount -> List DropdownItem -> Maybe Int -> ( DropState, Maybe Int, Cmd msg )
pickerSkip dropdown skipAmount dropdownItems selectedId =
    let
        newIndexCalc =
            case skipAmount of
                Exact skipCount ->
                    dropdown.keyboardSelectedIndex + skipCount

                First ->
                    0

                Last ->
                    List.length dropdownItems - 1

        newIndex =
            if newIndexCalc < 0 then
                0
            else if newIndexCalc >= List.length dropdownItems then
                List.length dropdownItems - 1
            else
                newIndexCalc

        newSelectedId =
            getId dropdownItems newIndex

        newDropdown =
            { dropdown
                | keyboardSelectedIndex = newIndex
                , searchString = ""
            }
    in
    if dropdown.isOpen then
        ( newDropdown, selectedId, scrollToDomId dropdown.domId newSelectedId )
    else
        ( newDropdown, Just newIndex, Cmd.none )


getDropdownText : List DropdownItem -> Maybe Int -> String
getDropdownText dropdownItems selectedId =
    dropdownItems
        |> List.filter (\t -> t.id == selectedId)
        |> List.map .name
        |> List.head
        |> Maybe.withDefault ""


commonWidth : ( String, String )
commonWidth =
    ( "min-width", "99.7%" )


type alias CustomTarget =
    { className : String
    , id : String
    , rect : DOM.Rectangle
    }


customAt : Decode.Decoder CustomTarget
customAt =
    Decode.map3 CustomTarget
        (Decode.at [ "target", "className" ] Decode.string)
        (Decode.at [ "target", "id" ] Decode.string)
        (DOM.target DOM.boundingClientRect)


customClick : (CustomTarget -> msg) -> Html.Attribute msg
customClick tagger =
    Events.on "click" (Decode.map tagger customAt)


customRelatedAt : Decode.Decoder String
customRelatedAt =
    Decode.at [ "relatedTarget", "id" ] Decode.string


focusOut : (String -> msg) -> Html.Attribute msg
focusOut tagger =
    Events.on "focusout" (Decode.map tagger (Decode.oneOf [ customRelatedAt, Decode.succeed "" ]))


noProp : Html.Attribute msg
noProp =
    disabled False


view : Dialog.RootDialog -> DropState -> (( DropState, Maybe Int, Cmd msg ) -> msg) -> List DropdownItem -> Maybe Int -> Html msg
view rootDialog dropdown toMsg dropdownItems selectedId =
    viewWithEnabled rootDialog True dropdown toMsg dropdownItems selectedId


viewWithEnabled : Dialog.RootDialog -> Bool -> DropState -> (( DropState, Maybe Int, Cmd msg ) -> msg) -> List DropdownItem -> Maybe Int -> Html msg
viewWithEnabled rootDialog isEnabled dropdown toMsg dropdownItems selectedId =
    let
        dropdownWidthMultiplier =
            if dropdown.showSearchText then
                9
            else
                7

        getWidth dropdownItem =
            case dropdown.width of
                Exactly width ->
                    width

                Auto ->
                    String.length dropdownItem.name * dropdownWidthMultiplier

        getDropdownText =
            dropdownItems
                |> List.filter (\t -> t.id == selectedId)
                |> List.map .name
                |> List.head
                |> Maybe.withDefault ""

        keyMsgDecoder =
            Events.keyCode
                |> Decode.andThen (keyDecoder dropdown)
                |> Decode.map (\t -> toMsg (onKey dropdown selectedId dropdownItems t))

        biggestStrLength =
            dropdownItems
                |> List.map getWidth
                |> List.sort
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 150

        dropdownItemsCount =
            List.length dropdownItems

        dropItemHeight =
            30.0

        dropItemPadding =
            4.5125

        dropListHeight =
            if dropdownItemsCount >= 5 then
                152.0
            else
                (toFloat dropdownItemsCount * dropItemHeight) - 2.0

        estimatedHeight =
            if dropdown.showSearchText then
                dropListHeight + 42.0 + dropItemHeight + dropItemPadding
            else
                dropListHeight + dropItemHeight + dropItemPadding

        bottomOfPagePadding =
            if dropdown.y + estimatedHeight > rootDialog.windowScrollY + toFloat rootDialog.windowSize.height then
                estimatedHeight
            else
                0
    in
    div
        [ Html.Attributes.id (dropdown.domId ++ "root")
        , if isEnabled then
            Events.onWithOptions "keydown" { stopPropagation = True, preventDefault = True } keyMsgDecoder
          else
            noProp
        , customClick (\t -> toMsg <| onClick dropdown selectedId dropdownItems t)
        , focusOut (\t -> toMsg <| onBlur dropdown selectedId t)
        , tabindex 1

        -- Make div focusable, since we need the on blur to trigger for both child elements
        , style [ ( "position", "relative" ), ( "width", "100%" ) ]
        , class "dropdown-outline"
        ]
        [ span
            [ classList
                [ ( "e-ddl", True )
                , ( "e-widget", True )
                , ( "e-focus ", dropdown.isOpen )
                , ( "e-popactive", dropdown.isOpen )
                ]
            , style
                [ case dropdown.width of
                    Exactly width ->
                        ( "width", toString width ++ "px" )

                    Auto ->
                        ( "width", "100%" )
                , case dropdown.height of
                    Exactly height ->
                        ( "height", toString height ++ "px" )

                    Auto ->
                        ( "height", "auto" )
                ]
            ]
            [ span
                [ classList
                    [ ( "e-in-wrap", True )
                    , ( "e-box", True )
                    , ( "e-disable", not isEnabled )
                    ]
                ]
                [ input
                    [ classList
                        [ ( "noselect", True )
                        , ( "e-input", True )
                        , ( "e-disable", not isEnabled )
                        ]
                    , Html.Attributes.id (dropdown.domId ++ "-input")
                    , readonly True
                    , defaultValue getDropdownText
                    , tabindex -1 -- Make it so you cannot set focus via tabbing, we need root div to have the focus
                    , disabled True -- Make it so you cannot click to set focus, we need root div to have the focus
                    , placeholder "Choose..."
                    ]
                    []
                , span
                    [ classList
                        [ ( "e-select", True )
                        , ( "e-disable", not isEnabled )
                        ]
                    ]
                    [ span [ class "e-icon e-arrow-sans-down" ] []
                    ]
                ]
            ]
        , div
            [ style
                [ if dropdown.isOpen then
                    ( "display", "block" )
                  else
                    ( "display", "none" )
                , ( "width", toString biggestStrLength ++ "px" )
                , ( "position", "absolute" )
                , ( "top", toString (32 - bottomOfPagePadding) ++ "px" )
                , ( "height", "42px" )
                , ( "border-top-left-radius", "4px" )
                , ( "border-top-right-radius", "4px" )
                , ( "border-color", "#cecece" )
                , ( "border-style", "solid" )
                , ( "border-width", "1px 1px 0 1px" )
                , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
                , ( "padding", "0" )
                , ( "margin", "0" )
                , ( "background-color", "white" )
                , ( "max-height", "152px" )
                , ( "overflow-x", "hidden" )
                , ( "overflow-y", "hidden" )
                , ( "z-index", "100" )
                , ( "min-width", "99.7%" )
                ]
            ]
            [ span
                [ class "e-atc e-search"
                , hidden (not dropdown.showSearchText)
                , style
                    [ ( "min-width", "96%" )
                    , ( "margin-left", "8px" )
                    , ( "margin-right", "6px" )
                    , ( "margin-top", "6px" )
                    ]
                ]
                [ span [ class "e-in-wrap" ]
                    [ input
                        [ class "e-input"
                        , defaultValue dropdown.searchString
                        , tabindex 0
                        , Html.Attributes.id (dropdown.domId ++ "filterInput")
                        ]
                        []
                    , span
                        [ class "e-icon e-search"
                        , style
                            [ ( "width", "14px" )
                            , ( "right", "10px" )
                            , ( "color", "#cecece" )
                            , ( "position", "absolute" )
                            ]
                        ]
                        []
                    ]
                ]
            ]
        , div
            [ style
                [ if dropdown.isOpen then
                    ( "display", "block" )
                  else
                    ( "display", "none" )
                , ( "width", toString biggestStrLength ++ "px" )
                , ( "position", "absolute" )
                , if dropdown.showSearchText then
                    ( "top", toString (74 - bottomOfPagePadding) ++ "px" )
                  else
                    ( "top", toString (32 - bottomOfPagePadding) ++ "px" )
                , ( "border-radius", "4px" )
                , ( "border-top-left-radius", "0" )
                , ( "border-top-right-radius", "0" )
                , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
                , ( "padding", "0" )
                , ( "margin", "0" )
                , ( "background-color", "white" )
                , ( "max-height", "152px" )
                , ( "overflow-x", "hidden" )
                , ( "overflow-y", "scroll" )
                , ( "z-index", "100" )
                , ( "min-width", "99.7%" )
                ]
            , class "dropdown-ul"
            ]
            [ ul
                [ style
                    [ ( "padding-left", "0" )
                    , ( "padding-right", "1px" )
                    , ( "-webkit-margin-after", "0" )
                    ]
                ]
                (viewItem dropdown dropdownItems)
            ]
        ]


viewItem : DropState -> List DropdownItem -> List (Html msg)
viewItem dropdown dropdownItems =
    dropdownItems
        |> List.indexedMap
            (\i item ->
                li
                    [ class "noselect dropdown-li"
                    , if dropdown.keyboardSelectedIndex == i && dropdown.isOpen then
                        style [ ( "background-color", "#f4f4f4" ), ( "color", "#333" ), commonWidth ]
                      else
                        style [ commonWidth ]
                    , Html.Attributes.id (dropdown.domId ++ "-" ++ Functions.defaultIntToString item.id)
                    ]
                    [ text item.name ]
            )


updateSearchString : Char -> DropState -> List DropdownItem -> Maybe Int -> ( DropState, Maybe Int, Cmd msg )
updateSearchString searchChar dropdown dropdownItems selectedId =
    let
        searchString =
            -- Manage backspace character
            if searchChar == '\x08' then
                String.dropRight 1 dropdown.searchString
            else
                dropdown.searchString ++ String.toLower (String.fromChar searchChar)

        maybeSelectedItem =
            dropdownItems
                |> List.filter (\t -> String.startsWith searchString (String.toLower t.name))
                |> List.head
    in
    case maybeSelectedItem of
        Just t ->
            ( { dropdown
                | searchString = searchString
                , keyboardSelectedIndex = getIndex t.id dropdownItems
              }
            , selectedId
            , scrollToDomId dropdown.domId t.id
            )

        Nothing ->
            ( { dropdown
                | searchString = searchString
              }
            , selectedId
            , Cmd.none
            )


keyDecoder : DropState -> Int -> Decode.Decoder Key
keyDecoder dropdown keyCode =
    let
        -- This is necessary to ensure that the key is not consumed and can propagate to the parent
        pass =
            Decode.fail ""

        key =
            Decode.succeed
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

                isAlpha char =
                    (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
            in
            -- Backspace is "searchable" because it can be used to modify the search string
            if isAlpha char || Char.isDigit char || char == '\x08' then
                key (Searchable char)
            else
                pass

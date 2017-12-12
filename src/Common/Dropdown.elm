module Common.Dropdown exposing (Context, Model, init, selectedFrom, openState, Msg(..), update, view, mainContainer)

{- a Dropdown component that manages its own state
 -}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


-- MODEL
{- main model, opaque to ensure it can only be updated thru Msg and Update
 -}


type Model
    = Model
        { selectedItem : Maybe String
        , isOpen : Bool
        }


init : Model
init =
    Model
        { selectedItem = Nothing
        , isOpen = False
        }



{- Context type alias
   (this is stuff not managed by the dropdown, but passed in from parent)
   kind of like props (including callbacks) in react
   in our dropdown context is the default text, displayed if no item is selected
-}


type alias Context =
    String



-- helpers to enable reading from Model


selectedFrom : Model -> Maybe String
selectedFrom (Model { selectedItem }) =
    selectedItem


openState : Model -> Bool
openState (Model { isOpen }) =
    isOpen



-- UPDATE


type Msg
    = ItemPicked (Maybe String)
    | SetOpenState Bool


update : Msg -> Model -> ( Model, Maybe String )
update msg (Model model) =
    case msg of
        ItemPicked item ->
            ( Model
                { model
                    | selectedItem = item
                    , isOpen = False
                }
            , item
            )

        SetOpenState newState ->
            ( Model
                { model
                    | isOpen = newState
                }
            , Nothing
            )



-- VIEW


view : Context -> Model -> List String -> Html Msg
view context (Model model) data =
    let
        mainText =
            model.selectedItem
                |> Maybe.withDefault context

        displayStyle =
            if model.isOpen then
                ( "display", "block" )
            else
                ( "display", "none" )

        mainAttr =
            case data of
                [] ->
                    [ style <| dropdownDisabled ++ dropdownInput
                    ]

                _ ->
                    [ style dropdownInput
                    , onClick <| SetOpenState <| not model.isOpen
                    ]
    in
        div
            [ style dropdownContainer ]
            [ p
                mainAttr
                [ span [ style dropdownText ] [ text mainText ]
                , span [] [ text "▾" ]
                ]
            , ul
                [ style <| displayStyle :: dropdownList ]
                (List.map viewItem data)
            ]


viewItem : String -> Html Msg
viewItem item =
    li
        [ style dropdownListItem
        , onClick <| ItemPicked <| Just item
        ]
        [ text item ]



-- helper to cancel click anywhere


onClick : msg -> Attribute msg
onClick message =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = False
        }
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
    , ( "width", "216px" )
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
    , ( "top", "36px" )
    , ( "border-radius", "4px" )
    , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
    , ( "padding", "4px 8px" )
    , ( "margin", "0" )
    , ( "width", "200px" )
    , ( "background-color", "white" )
    ]



-- styles for list items


dropdownListItem : List ( String, String )
dropdownListItem =
    [ ( "display", "block" )
    , ( "padding", "8px 8px" )
    ]

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

        activeClass =
            if model.isOpen then
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
                    , onClick <| SetOpenState <| not model.isOpen
                    ]
    in
        div [ style dropdownContainer ]
            [ span
                [ onClick <| SetOpenState <| not model.isOpen, class ("e-ddl e-widget " ++ activeClass), style [ ( "width", "152px" ) ] ]
                [ span [ class "e-in-wrap e-box" ]
                    [ input [ class "e-input", readonly True ] [ text mainText ]
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , div []
                [ ul [ style <| displayStyle :: dropdownList, class "dropdown-ul" ] (List.map viewItem data)
                ]
            ]


viewItem : String -> Html Msg
viewItem item =
    li
        [ onClick <| ItemPicked <| Just item
        , class "dropdown-li"
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

module Main exposing(..)
import Hello exposing (getEmployment)
import Model exposing (..)
import Html exposing (Html, text, div, input, program)
import Http
import Html.Attributes exposing (style, class, placeholder, id, type_)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

type Msg = 
    GetEmploy (Result Http.Error Employment)

type alias Model =
  { status : String
  , employ : Employment
  }

emptyEmploy = (Employment 0 "" "" "")

init : (Model, Cmd Msg)
init = (Model "Loading" emptyEmploy
    , Http.send GetEmploy getEmployment)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetEmploy (Ok emp) ->
            (Model "Success" emp , Cmd.none)
        GetEmploy (Err _) ->
            (Model "Fail" emptyEmploy, Cmd.none)

view : Model -> Html Msg
view model =
    div [] [
        input [ type_ "text", class "e-textbox", id "testBob"] []
        , div [] [ text model.employ.startDate ]
        , div [] [ text model.status ]
        --,div [gridStyle] (taskHeader :: taskRow)
    ]
    --<input type="text" id="DateOfDeath" data-bind="ejDatePicker: { value: DateOfDeath, enableStrictMode: true, width: '100%', htmlAttributes : { id: 'DateOfDeath', name: 'Date Of Death' } }" pattern="\d{1,2}/\d{1,2}/\d{4}" title="Please enter date in mm/dd/yyyy format" name="Date Of Death" />
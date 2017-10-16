port module Main exposing(..)
import Hello exposing (getEmployment)
import Model exposing (..)
import Html exposing (Html, text, div, input, program)
import Http
import Html.Attributes exposing (style, class, placeholder, id, type_, value)

port check : String -> Cmd msg

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

type Msg = 
    GetEmploy (Result Http.Error Employment)
    | Change String

type alias Model =
  { status : String
  , employ : Employment
  }

emptyEmploy : Employment
emptyEmploy = (Employment 0 "" "" "")

init : (Model, Cmd Msg)
init = (Model "Loading" emptyEmploy
    , Http.send GetEmploy getEmployment)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetEmploy (Ok emp) ->
            (Model "Success" emp, check emp.startDate)
        GetEmploy (Err _) ->
            (Model "Fail" emptyEmploy, Cmd.none)
        Change str -> ({model | status = str}, Cmd.none)

view : Model -> Html Msg
view model =
    div [] [
        input [ type_ "text", class "e-textbox", id "testBob" ] []
        , div [] [ text model.employ.startDate ]
        , div [] [ text model.status ]
    ]
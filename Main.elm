module Main exposing(..)
import Hello exposing (getEmployment, Employment)
import Html exposing (Html, text, div, input, program)
import Http
import Html.Attributes exposing (style, class, placeholder, id, type_)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg = 
    GetEmploy (Result Http.Error Employment)

type alias Model = { 
    content : String,
    employer : Maybe Employment 
    }

init : (Model, Cmd Msg)
init = (Model "" Nothing
    , Http.send GetEmploy getEmployment)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetEmploy (Ok emp) ->
            (Model "" (Just emp), Cmd.none)
        GetEmploy (Err _) ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [] [
        input [ type_ "text", class "e-textbox", id "testBob"] []
        , div [] [ text (String.reverse model.content) ]
        --,div [gridStyle] (taskHeader :: taskRow)
    ]
    --<input type="text" id="DateOfDeath" data-bind="ejDatePicker: { value: DateOfDeath, enableStrictMode: true, width: '100%', htmlAttributes : { id: 'DateOfDeath', name: 'Date Of Death' } }" pattern="\d{1,2}/\d{1,2}/\d{4}" title="Please enter date in mm/dd/yyyy format" name="Date Of Death" />
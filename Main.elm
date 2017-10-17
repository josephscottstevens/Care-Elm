port module Main exposing (..)

import Hello exposing (getEmployment)
import Model exposing (..)
import Html exposing (Html, text, div, input, program)
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


init : ( Model, Cmd Msg )
init =
    ( Model Initial
    , getEmployment
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok emp) ->
            ( Model (Grid emp), check emp.startDate )

        Load (Err t) ->
            ( Model Error, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid t ->
            input [ type_ "text", class "e-textbox", id "testBob" ] []

        Error ->
            div [] [ text "error!" ]

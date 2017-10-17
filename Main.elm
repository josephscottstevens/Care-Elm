port module Main exposing (..)

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


init : ( Model, Cmd Msg )
init =
    ( Model Initial
    , Http.send Load getEmployment
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initial ->
            ( Model Initial, Cmd.none )

        Failed ->
            ( Model Failed, Cmd.none )

        Grid t ->
            ( Model (Grid t), Cmd.none )

        Load (Ok emp) ->
            ( Model (Grid emp), check emp.startDate )

        Load (Err t) ->
            ( Model Failed, Cmd.none )


view : Model -> Html Msg
view model =
    case model.status of
        Initial ->
            div [] [ text "loading" ]

        Load _ ->
            div [] [ text "requesting data" ]

        Failed ->
            div [] [ text "error!" ]

        Grid t ->
            input [ type_ "text", class "e-textbox", id "testBob" ] []

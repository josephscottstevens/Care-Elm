port module Main exposing (..)

import Load exposing (getEmployment)
import Model exposing (..)
import Html exposing (Html, text, div, input, program, button)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick)


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
        EditStart emp ->
            ( Model (Edit emp), Cmd.none )

        EditEnd emp ->
            ( Model (Grid emp), Cmd.none )

        Load (Ok emp) ->
            ( Model (Grid emp), check emp.startDate )

        Load (Err t) ->
            ( Model Error, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid emp ->
            div []
                [ button [ onClick (EditStart emp) ] [ text "edit" ]
                , input [ type_ "text", class "e-textbox", id "testBob" ] []
                , div [] [ text "a" ]
                , div [] [ text "b" ]
                ]

        Edit emp ->
            div []
                [ button [ onClick (EditEnd emp) ] [ text "edit" ]
                , div [] [ text "edit mode" ]
                ]

        Error ->
            div [] [ text "error!" ]

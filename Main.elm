port module Main exposing (..)

import Load exposing (getEmployment, newEmployers, updateEmployers)
import Model exposing (..)
import HtmlHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)


port sendTestDate : String -> Cmd msg


port getTestDate : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    getTestDate UpdateStartDate


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getEmployment )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStart employer ->
            ( { model | state = Edit employer }, sendTestDate employer.dob )

        EditSave employer ->
            ( { model | state = Grid, employers = (updateEmployers model.employers employer) }, Cmd.none )

        EditCancel ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok model) ->
            ( { model | state = Grid, employers = (newEmployers model.employers) }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        UpdateState emp newState ->
            ( { model | state = Edit { emp | state = newState } }, Cmd.none )

        UpdateCity emp newCity ->
            ( { model | state = Edit { emp | city = newCity } }, Cmd.none )

        UpdateStartDate newDob ->
            case model.state of
                Edit emp ->
                    ( { model | state = Edit { emp | dob = newDob } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SortByZip ->
            case model.sortMode of
                SortNone ->
                    ( { model | sortMode = SortDesc }, Cmd.none )

                SortDesc ->
                    ( { model | sortMode = SortAsc }, Cmd.none )

                SortAsc ->
                    ( { model | sortMode = SortNone }, Cmd.none )

        Reset ->
            ( emptyModel, getEmployment )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ button [ class "btn btn-default", controlStyle, onClick Reset ] [ text "reset" ]
                , div [ gridStyle ] (employmentHeaders :: (employmentRows model.employers model.sortMode))
                ]

        Edit emp ->
            div []
                [ input [ placeholder "Date of birth", type_ "text", class "e-textbox", controlStyle, id "testDate", value emp.dob ] []
                , input [ placeholder "City", class "e-textbox", controlStyle, onInput (UpdateCity emp), value emp.city ] []
                , input [ placeholder "State", class "e-textbox", controlStyle, onInput (UpdateState emp), value emp.state ] []
                , button [ class "btn btn-default", controlStyle, onClick (EditSave emp) ] [ text "save" ]
                , button [ class "btn btn-default", controlStyle, onClick EditCancel ] [ text "cancel" ]
                ]

        Error err ->
            div [] [ text (toString err) ]



-- { rowId : Int
--     , dob : String
--     , email : String
--     , addressLine1 : String
--     , addressLine2 : String
--     , city : String
--     , state : String
--     , zipCode : String
--     , phone : String
--     }

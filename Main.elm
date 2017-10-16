import Hello exposing (tasks)
import Html exposing (Html, text, div, input)
import Html.Attributes exposing (style, class, placeholder)
import Html.Events exposing (onInput)

gridStyle : Html.Attribute msg
gridStyle =
  style
    [ ("display", "grid")
    , ("grid-template-columns", "1fr")
    , ("grid-template-rows", "repeat(-1, auto)")
    , ("grid-gap", "1px")
    , ("padding", "40px")
    , ("text-align", "center")
    ]    

rowStyle : Html.Attribute msg
rowStyle =
  style
    [ ("display", "grid")
    , ("grid-template-columns", "80px 1fr 1fr 1fr 1fr 1fr 1fr")
    , ("grid-template-rows", "auto auto")
    , ("grid-gap", "5px")
    , ("grid-column-start", "1")
    , ("box-shadow", "0 0 0 1px gray")
    ]

cellStyle : Html.Attribute msg
cellStyle = style [("padding", "10px")]

main = Html.beginnerProgram { model = model, view = view, update = update }

type alias Model = { content: String }
model = { content = "" }

type Msg 
    = Change String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }
taskHeader : Html msg
taskHeader = 
                    div [rowStyle] [
                        div [cellStyle] [text "Id"]
                    ,   div [cellStyle] [text "Priority"]
                    ,   div [cellStyle] [text "Title"]
                    ,   div [cellStyle] [text "Name"]
                    ,   div [cellStyle] [text "InitiatedOn"]
                    ,   div [cellStyle] [text "Due At"]
                    ,   div [cellStyle] [text "closed"]
                    ]

taskRow : List (Html msg)
taskRow = tasks |> List.map (\t -> 
                    div [rowStyle] [
                        div [cellStyle] [text (toString t.id)]
                    ,   div [cellStyle] [text t.priority]
                    ,   div [cellStyle] [text t.title]
                    ,   div [cellStyle] [text t.name]
                    ,   div [cellStyle] [text t.initiatedOn]
                    ,   div [cellStyle] [text t.dueAt]
                    ,   div [cellStyle] [text (toString t.closed)]
                    ])

view : Model -> Html Msg
view model =
    div [] [
        input [ placeholder "Text to reverse", class "e-textbox", onInput Change] []
        ,div [gridStyle] (taskHeader :: taskRow)
    ]
    --<input type="text" id="DateOfDeath" data-bind="ejDatePicker: { value: DateOfDeath, enableStrictMode: true, width: '100%', htmlAttributes : { id: 'DateOfDeath', name: 'Date Of Death' } }" pattern="\d{1,2}/\d{1,2}/\d{4}" title="Please enter date in mm/dd/yyyy format" name="Date Of Death" />
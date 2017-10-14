import Hello exposing (tasks)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)

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

main : Html msg
main = div [gridStyle] (taskHeader :: taskRow)
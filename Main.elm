import Hello exposing (tasks)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)

gridStyle : Html.Attribute msg
gridStyle =
  style
    [ ("display", "grid")
    , ("grid-template-columns", "1fr")
    , ("grid-template-rows", "repeat(-1, auto)")
    , ("grid-gap", "5px")
    ]

rowStyle : Html.Attribute msg
rowStyle =
  style
    [ ("display", "grid")
    , ("grid-template-columns", "1fr 1fr 1fr 1fr 1fr 1fr 1fr")
    , ("grid-template-rows", "auto auto")
    , ("grid-gap", "5px")
    , ("grid-column-start", "1")
    ]

taskRow : List (Html msg)
taskRow = tasks |> List.map (\t -> 
                    div [rowStyle] [
                        div [] [text (toString t.id)]
                    ,   div [] [text t.priority]
                    ,   div [] [text t.title]
                    ,   div [] [text t.name]
                    ,   div [] [text t.initiatedOn]
                    ,   div [] [text t.dueAt]
                    ,   div [] [text (toString t.closed)]
                    ])

main : Html msg
main = div [gridStyle] taskRow
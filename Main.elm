import Hello exposing (comments)
import Html exposing (Html, text, div)

main = comments |> List.map (\t -> div [] [text t.msg])
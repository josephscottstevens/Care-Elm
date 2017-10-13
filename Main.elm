import Hello exposing (comments)
import Html exposing (Html, text, div)

x : List (Html msg)
x = comments |> List.map (\t -> div [] [text t.msg])

main : Html msg
main = div [] x
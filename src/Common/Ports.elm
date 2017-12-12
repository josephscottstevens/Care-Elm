port module Common.Ports exposing (sendMenuMessage, dropDownToggle, setUnsavedChanges)

import Common.Types exposing (MenuMessage)


port sendMenuMessage : MenuMessage -> Cmd msg


port dropDownToggle : (Int -> msg) -> Sub msg


port setUnsavedChanges : Bool -> Cmd msg

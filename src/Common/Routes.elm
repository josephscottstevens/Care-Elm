module Common.Routes exposing (..)

import Navigation
import Common.Types as Pages


getPage : String -> Pages.Page
getPage pageStr =
    case pageStr of
        "#/people/_hospitalizations/" ->
            Pages.Hospitilizations

        "#/people/_hospitalizations/addedit" ->
            Pages.HospitilizationsAddEdit

        _ ->
            Pages.None


navHospitilizations : Cmd msg
navHospitilizations =
    Navigation.load "#/people/_hospitalizations/"


navHospitilizationsAddEdit : Cmd msg
navHospitilizationsAddEdit =
    Navigation.load "#/people/_hospitalizations/addedit"


navRecords : Cmd msg
navRecords =
    Navigation.load "#/people/_records"

port module Ports exposing (..)

import Common.Types exposing (..)


port resetUpdate : Maybe Int -> Cmd msg




port toggleConsent : Bool -> Cmd msg


port editTask : Int -> Cmd msg


port dropDownToggle : (Int -> msg) -> Sub msg


port deleteConfirmed : (Int -> msg) -> Sub msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port loadDataSourceComplete : (AddEditDataSource -> msg) -> Sub msg


port resetUpdateComplete : (Maybe Int -> msg) -> Sub msg


port updateFacility : (DropDownItem -> msg) -> Sub msg


port updateCategory : (DropDownItem -> msg) -> Sub msg


port updateTimeVisit : (Maybe String -> msg) -> Sub msg


port updateTimeAcc : (Maybe String -> msg) -> Sub msg


port updateFileName : (String -> msg) -> Sub msg


port updateReportDate : (Maybe String -> msg) -> Sub msg


port updateRecordingDate : (Maybe String -> msg) -> Sub msg


port updateUser : (DropDownItem -> msg) -> Sub msg


port updateTask : (DropDownItem -> msg) -> Sub msg



-- Hospitilizations


port updateHospitilization : (DropDownItem -> msg) -> Sub msg


port updateFacility2 : (DropDownItem -> msg) -> Sub msg


port updateDateOfAdmission : (Maybe String -> msg) -> Sub msg


port updateDateOfDischarge : (Maybe String -> msg) -> Sub msg


port updateHospitalServiceType : (DropDownItem -> msg) -> Sub msg


port updateDischargePhysician : (DropDownItem -> msg) -> Sub msg

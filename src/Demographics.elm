port module Demographics exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Dropdown as Dropdown exposing (defaultDropConfig)
import Common.Functions as Functions exposing (decodeDropdownItem, maybeVal)
import Common.Types as Types exposing (DropdownItem)
import Html exposing (Html, b, div, h4, input, label, option, select, span, table, tbody, td, text, textarea, tr)
import Html.Attributes exposing (attribute, checked, class, colspan, defaultValue, disabled, hidden, id, maxlength, name, style, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra as List
import MaskedInput.Number as MaskedNumber


port initDemographics : SfData -> Cmd msg


port initDemographicsAddress : List SfAddress -> Cmd msg


port addNewAddress : SfAddress -> Cmd msg


port updateDemographicsAddressMoveInDate : (SfAddress -> msg) -> Sub msg


port updateDemographics : (SfData -> msg) -> Sub msg


port saveSuccess : Int -> Cmd msg


scrollToError : Cmd msg
scrollToError =
    Functions.scrollTo "#ErrorDiv"


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ updateDemographics UpdateDemographics
        , updateDemographicsAddressMoveInDate UpdateDemographicsAddressMoveInDate
        ]


init : Int -> Cmd Msg
init patientId =
    load patientId


type Phone
    = Home
    | Work
    | Cell
    | Unknown


type alias Model =
    { facilityId : Maybe Int
    , facilityDropState : Dropdown.DropState
    , patientAddresses : List PatientAddress
    , stateDropdown : List DropdownItem
    , primaryAddressIndex : Int
    , mainProviderId : Maybe Int
    , mainProviderDropState : Dropdown.DropState
    , careCoordinatorId : Maybe Int
    , careCoordinatorDropState : Dropdown.DropState
    , sexTypeId : Maybe Int
    , sexTypeDropState : Dropdown.DropState
    , sexualOrientationId : Maybe Int
    , sexualOrientationDropState : Dropdown.DropState
    , genderIdentityId : Maybe Int
    , genderIdentityDropState : Dropdown.DropState
    , uSVeteranId : Maybe Int
    , uSVeteranDropState : Dropdown.DropState
    , religionId : Maybe Int
    , religionDropState : Dropdown.DropState
    , patientId : Int
    , demographicsId : Maybe Int
    , nickName : Maybe String
    , ssn : Maybe String
    , lastName : Maybe String
    , firstName : Maybe String
    , middle : Maybe String
    , birthPlace : Maybe String
    , mrn : Maybe String
    , mrnRequired : Bool
    , patientAccountNumber : Maybe String
    , facilityPtID : Maybe String
    , facilityPtIDRequired : Bool
    , sexualOrientationNote : Maybe String
    , genderIdentityNote : Maybe String
    , email : Maybe String
    , sfData : SfData
    , patientLanguagesMap : List PatientLanguagesMap
    , householdMembers : List HouseholdMember

    -- Start Contact Hours
    , tZ : List SelectList
    , weekData : List DayData
    , selectedTimeZoneId : Maybe Int
    , selectedTimeZoneDropState : Dropdown.DropState
    , showContactValidationErrors : Bool

    -- End Contact Hours
    , showValidationErrors : Bool
    , suffixId : Maybe Int
    , suffixDropState : Dropdown.DropState
    , prefixId : Maybe Int
    , prefixDropState : Dropdown.DropState
    , raceId : Maybe Int
    , raceDropState : Dropdown.DropState
    , ethnicityId : Maybe Int
    , ethnicityDropState : Dropdown.DropState
    , acuityLevelId : Maybe Int
    , acuityLevelDropState : Dropdown.DropState
    , homePhoneNumber : Maybe Int
    , cellPhoneNumber : Maybe Int
    , workPhoneNumber : Maybe Int
    , homePhoneNumberMaskState : MaskedNumber.State
    , cellPhoneNumberMaskState : MaskedNumber.State
    , workPhoneNumberMaskState : MaskedNumber.State
    , preferredPhoneNumber : Phone
    , nodeCounter : Int
    , drops : DropdownSource
    }


type alias SfData =
    { dateOfBirth : Maybe String
    , dateOfDeath : Maybe String
    , vip : Maybe Bool
    }


type alias SfAddress =
    { nodeId : Int
    , dt : Maybe String
    , removed : Bool
    }


type alias PatientLanguagesMap =
    { id : Maybe Int
    , languageId : Maybe Int
    , isPreferred : Bool
    , dropState : Dropdown.DropState
    , nodeId : Int
    }


type alias HouseholdMember =
    { id : Maybe Int
    , name : Maybe String
    , relationshipId : Maybe Int
    , comments : Maybe String
    , dropState : Dropdown.DropState
    , nodeId : Int
    }


type alias PatientAddress =
    { id : Maybe Int
    , addressLine1 : Maybe String
    , addressLine2 : Maybe String
    , addressLine3 : Maybe String
    , city : Maybe String
    , stateId : Maybe Int
    , zipCode : Maybe String
    , isPreferred : Bool
    , moveInDate : Maybe String
    , addressType : Maybe Int
    , facilityAddress : Maybe FacilityAddress
    , facilityAddressId : Maybe Int
    , facilityAddressDropState : Dropdown.DropState
    , addressTypeDropState : Dropdown.DropState
    , addressStateDropState : Dropdown.DropState
    , nodeId : Int
    }


type alias FacilityAddress =
    { id : Maybe Int
    , address : Maybe String
    , city : Maybe String
    , stateId : Maybe Int
    , zipCode : Maybe String
    }


view : Model -> Html Msg
view model =
    div [ id "demographicInformationForm", class "col-xs-12 padding-h-0" ]
        [ h4 [ class "col-xs-12 padding-h-0" ] [ text "Assigned To" ]
        , div [ class "col-xs-12 padding-h-0 padding-bottom-10", id "ErrorDiv" ]
            [ viewValidationErrorsDiv model (validatationErrors model)
            ]
        , div rowStyle
            [ dropbox "Facility" True <|
                Dropdown.view model.facilityDropState UpdateFacility model.drops.facilityDropdown model.facilityId
            , textbox "Patient's Facility ID No" model.facilityPtIDRequired model.facilityPtID UpdateFacilityPtID
            , textbox "Medical Record No" model.mrnRequired model.mrn UpdateMedicalRecordNo
            , textbox "Patient Account No" False model.patientAccountNumber UpdatePatientAccountNo
            ]
        , div rowStyle
            [ dropbox "Main Provider" True <|
                Dropdown.view model.mainProviderDropState UpdateMainProvider model.drops.mainProviderDropdown model.mainProviderId
            , dropbox "Care Coordinator" True <|
                Dropdown.view model.careCoordinatorDropState UpdateCareCoordinator model.drops.careCoordinatorDropdown model.careCoordinatorId
            ]
        , h4 [ class "col-xs-12 padding-h-0 padding-top-10" ] [ text "Demographic Information" ]
        , div rowStyle
            [ dropbox "Prefix" False <|
                Dropdown.view model.prefixDropState UpdatePrefix model.drops.prefixDropdown model.prefixId
            , nonumberbox "First Name" True model.firstName UpdateFirstName
            , nonumberbox "Middle Name" False model.middle UpdateMiddle
            , nonumberbox "Last Name" True model.lastName UpdateLastName
            , dropbox "Suffix" False <|
                Dropdown.view model.suffixDropState UpdateSuffix model.drops.suffixDropdown model.suffixId
            , textbox "Nickname" False model.nickName UpdateNickname
            , sfbox "Date of Birth" True
            , textbox "Birth Place" False model.birthPlace UpdateBirthPlace
            , sfbox "Date of Death" False
            , textbox "SSN" False model.ssn UpdateSSN
            , dropbox "Acuity Level" False <|
                Dropdown.view model.acuityLevelDropState UpdateAcuityLevel Types.acuityLevelDropdown model.acuityLevelId
            ]
        , div rowStyle
            [ sfbox "VIP" False
            , dropbox "Sex at Birth" True <|
                Dropdown.view model.sexTypeDropState UpdateSexType model.drops.sexTypeDropdown model.sexTypeId
            , dropbox "Sexual Orientation" False <|
                Dropdown.view model.sexualOrientationDropState UpdateSexualOrientation model.drops.sexualOrientationDropdown model.sexualOrientationId
            , textbox "Sexual Orientation Note" False model.sexualOrientationNote UpdateSexualOrientationNote
            , dropbox "Gender Identity" False <|
                Dropdown.view model.genderIdentityDropState UpdateGenderIdentity model.drops.genderIdentityDropdown model.genderIdentityId
            , textbox "Gender Identity Note" False model.genderIdentityNote UpdateGenderIdentityNote
            , dropbox "Race" False <|
                Dropdown.view model.raceDropState UpdateRace model.drops.raceDropdown model.raceId
            , dropbox "Ethnicity" False <|
                Dropdown.view model.ethnicityDropState UpdateEthnicity model.drops.ethnicityDropdown model.ethnicityId
            , dropbox "US Veteran" False <|
                Dropdown.view model.uSVeteranDropState UpdateUSVeteran model.drops.uSVeteranDropdown model.uSVeteranId
            , dropbox "Religion" False <|
                Dropdown.view model.religionDropState UpdateReligion model.drops.religionDropdown model.religionId
            , textbox "Email" False model.email UpdateEmail
            ]
        , div [ class "col-xs-12 padding-h-0 padding-top-10" ]
            [ div [ class "col-xs-12 col-sm-12 col-md-10 col-lg-8 padding-h-0" ]
                [ h4 [ class "inline-block" ] [ text "Languages" ]
                , div [ class "inline-block e-tooltxt pointer", title "Add new language", onClick AddNewLanguage ]
                    [ span [ class "e-addnewitem e-toolbaricons e-icon e-addnew" ] []
                    ]
                , div [] (List.map (viewLanguages model.drops.languageDropdown) model.patientLanguagesMap)
                ]
            ]
        , div [ class "col-xs-12 padding-h-0" ]
            [ div [ class "col-xs-12 col-sm-12 col-md-10 col-lg-8 padding-h-0" ]
                [ h4 [ class "inline-block required" ] [ text "Phones" ]
                , div [ class "margin-bottom-5", style [ ( "width", "350px" ) ] ]
                    [ div [ class "inline-block ", style [ ( "width", "22px" ), ( "vertical-align", "middle" ) ], title "Mark as preferred" ]
                        [ input [ type_ "radio", style [ ( "margin-top", "0" ) ], checked (model.preferredPhoneNumber == Home), name "phoneGroup", onClick (UpdatePreferredPhoneNumber Home) ] [] ]
                    , div [ class "inline-block", style [ ( "width", "45px" ), ( "vertical-align", "middle" ) ] ]
                        [ label [ style [ ( "width", "auto" ), ( "margin-left", "3px" ), ( "padding-top", "0px" ) ] ] [ text ("Home" ++ ":") ]
                        ]
                    , div [ class "inline-block" ]
                        [ MaskedNumber.input
                            { pattern = "(###) ###-####"
                            , inputCharacter = '#'
                            , onInput = UpdateHomePhone
                            , toMsg = UpdateHomePhoneMaskState
                            , hasFocus = Nothing
                            }
                            [ class "e-textbox", maskStyle ]
                            model.homePhoneNumberMaskState
                            model.homePhoneNumber
                        ]
                    ]
                , div [ class "margin-bottom-5", style [ ( "width", "350px" ) ] ]
                    [ div [ class "inline-block ", style [ ( "width", "22px" ), ( "vertical-align", "middle" ) ], title "Mark as preferred" ]
                        [ input [ type_ "radio", style [ ( "margin-top", "0" ) ], checked (model.preferredPhoneNumber == Cell), name "phoneGroup", onClick (UpdatePreferredPhoneNumber Cell) ] [] ]
                    , div [ class "inline-block", style [ ( "width", "45px" ), ( "vertical-align", "middle" ) ] ]
                        [ label [ style [ ( "width", "auto" ), ( "margin-left", "3px" ), ( "padding-top", "0px" ) ] ] [ text ("Cell" ++ ":") ]
                        ]
                    , div [ class "inline-block" ]
                        [ MaskedNumber.input
                            { pattern = "(###) ###-####"
                            , inputCharacter = '#'
                            , onInput = UpdateCellPhone
                            , toMsg = UpdateCellPhoneMaskState
                            , hasFocus = Nothing
                            }
                            [ class "e-textbox", maskStyle ]
                            model.cellPhoneNumberMaskState
                            model.cellPhoneNumber
                        ]
                    ]
                , div [ class "margin-bottom-5", style [ ( "width", "350px" ) ] ]
                    [ div [ class "inline-block ", style [ ( "width", "22px" ), ( "vertical-align", "middle" ) ], title "Mark as preferred" ]
                        [ input [ type_ "radio", style [ ( "margin-top", "0" ) ], checked (model.preferredPhoneNumber == Work), name "phoneGroup", onClick (UpdatePreferredPhoneNumber Work) ] [] ]
                    , div [ class "inline-block", style [ ( "width", "45px" ), ( "vertical-align", "middle" ) ] ]
                        [ label [ style [ ( "width", "auto" ), ( "margin-left", "3px" ), ( "padding-top", "0px" ) ] ] [ text ("Work" ++ ":") ]
                        ]
                    , div [ class "inline-block" ]
                        [ MaskedNumber.input
                            { pattern = "(###) ###-#### ext.#####"
                            , inputCharacter = '#'
                            , onInput = UpdateWorkPhone
                            , toMsg = UpdateWorkPhoneMaskState
                            , hasFocus = Nothing
                            }
                            [ class "e-textbox", maskStyle ]
                            model.workPhoneNumberMaskState
                            model.workPhoneNumber
                        ]
                    ]
                ]
            ]
        , div [ class "col-xs-12 padding-h-0 margin-bottom-5" ]
            [ div [ class "col-xs-12 col-sm-12 col-md-10 col-lg-8 padding-h-0" ]
                [ h4 [ class "inline-block required" ] [ text "Addresses" ]
                , div [ class "inline-block e-tooltxt pointer", title "Add new address", onClick AddNewAddress ]
                    [ span [ class "e-addnewitem e-toolbaricons e-icon e-addnew" ] []
                    ]
                , div [] (List.map (viewAddress model.stateDropdown model.drops.facilityDropdown) model.patientAddresses)
                ]
            ]
        , div [ class "col-xs-12 padding-h-0 margin-bottom-5" ]
            [ div [ class "col-xs-12 col-sm-12 col-md-10 col-lg-8 padding-h-0" ]
                [ h4 [ class "inline-block" ] [ text "Household Members" ]
                , div [ class "inline-block e-tooltxt pointer", title "Add new household member", onClick AddNewHouseholdMember ]
                    [ span [ class "e-addnewitem e-toolbaricons e-icon e-addnew" ] []
                    ]
                , div [] (List.map viewHouseholdMembers model.householdMembers)
                ]
            ]
        , viewContactHours model
        , div [ class "col-xs-12 padding-h-0 padding-top-10 padding-bottom-10" ]
            [ div [ class "col-xs-12 padding-h-0 padding-top-10" ]
                [ input [ type_ "button", class "btn btn-sm btn-success", value "Save", onClick Save ] []
                , input [ type_ "button", class "btn btn-sm btn-default margin-left-5", value "Reset", onClick Cancel ] []
                ]
            ]
        ]


vertCent : ( String, String )
vertCent =
    ( "vertical-align", "middle" )


viewLanguages : List DropdownItem -> PatientLanguagesMap -> Html Msg
viewLanguages dropdownItems lang =
    div [ class "margin-bottom-5", style [ ( "width", "350px" ) ] ]
        [ div [ class "inline-block ", style [ ( "width", "22px" ), ( "padding-top", "5px" ), ( "vertical-align", "middle" ) ], title "Mark as preferred" ]
            [ input [ type_ "radio", checked lang.isPreferred, name "languageGroup", onCheck (UpdatePreferredLanguage lang) ] [] ]
        , div [ class "inline-block", style [ ( "width", "calc(100% - 50px)" ), ( "vertical-align", "middle" ) ], title "Choose language" ]
            [ Dropdown.view lang.dropState (UpdateLanguage lang) dropdownItems lang.languageId ]
        , div [ class "inline-block", style [ ( "width", "20px" ), ( "vertical-align", "middle" ) ], title "Remove", onClick (RemoveLanguage lang) ]
            [ span [ class "e-cancel e-toolbaricons e-icon e-cancel margin-bottom-5 pointer" ] []
            ]
        ]


viewAddress : List DropdownItem -> List DropdownItem -> PatientAddress -> Html Msg
viewAddress stateDropdownItems facilityDropdownItems address =
    let
        helper t =
            address.facilityAddress
                |> Maybe.map t
                |> Maybe.withDefault Nothing

        thisOrThat t y =
            if address.addressType == Just 1 then
                helper t
            else
                y

        addressLine1 =
            thisOrThat .address address.addressLine1

        addressLine2 =
            thisOrThat (\_ -> Nothing) address.addressLine2

        addressLine3 =
            thisOrThat (\_ -> Nothing) address.addressLine3

        city =
            thisOrThat .city address.city

        stateId =
            thisOrThat .stateId address.stateId

        zipCode =
            thisOrThat .zipCode address.zipCode

        isDisabled =
            case address.addressType of
                Nothing ->
                    False

                Just addressType ->
                    if addressType == 1 then
                        True
                    else
                        False

        addressDiv =
            case address.addressType of
                Nothing ->
                    text ""

                Just addressType ->
                    if addressType == 0 then
                        text ""
                    else
                        div []
                            [ label [ labelPad ] [ text "Facility:" ]
                            , div [ class "DemographicsInputDiv2" ]
                                [ Dropdown.view address.facilityAddressDropState (UpdateFacilityAddress address) facilityDropdownItems address.facilityAddressId
                                ]
                            ]

        sm6 =
            class "col-xs-8 col-sm-6 padding-h-0"

        sm12 =
            class "col-xs-12 col-sm-12 padding-h-0"

        labelPad =
            style [ ( "padding-left", "10px" ) ]
    in
    div [ class "multi-address-template" ]
        [ div [ class "col-xs-12 padding-h-0 margin-bottom-5" ]
            [ div [ title "Mark as primary", class "col-xs-6 padding-h-0 inline-block" ]
                [ input
                    [ type_ "radio"
                    , checked address.isPreferred
                    , style [ ( "margin-top", "0px" ), vertCent ]
                    , onCheck (UpdatePreferredAddress address)
                    , name "addressGroup"
                    ]
                    []
                , label [ style [ ( "margin-bottom", "0px" ), ( "margin-left", "4px" ) ] ] [ text "Primary" ]
                ]
            , div [ class "col-xs-6 padding-h-0 inline-block", style [ vertCent ], title "Remove", onClick (RemoveAddress address) ]
                [ span [ style [ ( "padding-right", "20px" ), ( "padding-top", "5px" ) ], class "e-cancel e-toolbaricons e-icon e-cancel margin-bottom-5 pointer pull-right" ] []
                ]
            ]
        , div [ class "col-xs-12 padding-h-0", style [ ( "padding-bottom", "20px" ) ] ]
            [ div [ class "row" ]
                [ div [ sm6 ]
                    [ label [ labelPad ] [ text "Address Type:" ]
                    , div [ class "DemographicsInputDiv" ]
                        [ Dropdown.view address.addressTypeDropState (UpdateAddressType address) Types.addressTypeDropdown address.addressType
                        ]
                    ]
                , div [ sm6 ]
                    [ label [ labelPad ] [ text "Move in Date:" ]
                    , div [ class "DemographicsInputDiv2" ]
                        [ input
                            [ type_ "text"
                            , id ("MoveInDate" ++ toString address.nodeId)
                            , maybeValue <| Just <| Functions.defaultDate address.moveInDate
                            ]
                            []
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ sm12 ]
                    [ addressDiv
                    ]
                ]
            , div [ class "row" ]
                [ div [ sm6 ]
                    [ label [ labelPad, class "required" ] [ text "Address Line 1:" ]
                    , div [ class "DemographicsInputDiv" ]
                        [ input [ class "e-textbox", type_ "text", maybeValue addressLine1, disabled isDisabled, onInput (UpdateAddressLine1 address) ] []
                        ]
                    ]
                , div [ sm6 ]
                    [ label [ labelPad, class "required" ] [ text "City:" ]
                    , div [ class "DemographicsInputDiv2" ]
                        [ input [ class "e-textbox", type_ "text", maybeValue city, disabled isDisabled, onInput (UpdateCity address) ] []
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ sm6 ]
                    [ label [ labelPad ] [ text "Address Line 2:" ]
                    , div [ class "DemographicsInputDiv" ]
                        [ input [ class "e-textbox", type_ "text", maybeValue addressLine2, disabled isDisabled, onInput (UpdateAddressLine2 address) ] []
                        ]
                    ]
                , div [ sm6 ]
                    [ label [ labelPad, class "required" ] [ text "State:" ]
                    , if isDisabled then
                        div [ class "DemographicsInputDiv2" ]
                            [ input [ class "e-textbox", type_ "text", maybeValue <| Just (Dropdown.getDropdownText stateDropdownItems stateId), disabled True ] []
                            ]
                      else
                        div [ class "DemographicsInputDiv2 margin-bottom-5" ]
                            [ Dropdown.view address.addressStateDropState (UpdateState address) stateDropdownItems stateId
                            ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ sm6 ]
                    [ label [ labelPad ] [ text "Apt./Room No.:" ]
                    , div [ class "DemographicsInputDiv" ]
                        [ input [ class "e-textbox", type_ "text", maybeValue addressLine3, disabled isDisabled, onInput (UpdateAddressLine3 address) ] []
                        ]
                    ]
                , div [ sm6 ]
                    [ label [ labelPad, class "required" ] [ text "Zip Code:" ]
                    , div [ class "DemographicsInputDiv2" ]
                        [ input [ class "e-textbox", type_ "text", maybeValue zipCode, disabled isDisabled, onInput (UpdateZipcode address), maxlength 5 ] []
                        ]
                    ]
                ]
            ]
        ]


viewHouseholdMembers : HouseholdMember -> Html Msg
viewHouseholdMembers householdMember =
    div [ class "multi-address-template", style [ ( "padding-bottom", "20px" ) ] ]
        [ div [ class "col-xs-12 col-sm-6 padding-h-0" ]
            [ div []
                [ label [] [ text "Name:" ]
                , div [ class "form-column" ]
                    [ input
                        [ class "e-textbox"
                        , type_ "text"
                        , maybeValue householdMember.name
                        , onInput (UpdateHouseholdMemberName householdMember)
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "col-xs-12 col-sm-5 padding-h-0" ]
            [ div []
                [ label [] [ text "Relationships:" ]
                , div [ class "form-column" ]
                    [ Dropdown.view householdMember.dropState (UpdateHouseholdMemberRelationship householdMember) Types.relationshipsDropdown householdMember.relationshipId
                    ]
                ]
            ]
        , div [ class "col-xs-12 col-sm-1 padding-h-0", style [ vertCent ], title "Remove", onClick (RemoveHouseholdMember householdMember) ]
            [ span [ style [ ( "padding-right", "20px" ), ( "padding-top", "5px" ) ], class "e-cancel e-toolbaricons e-icon e-cancel margin-bottom-5 pointer pull-right" ] []
            ]
        , div []
            [ div []
                [ div [ class "form-column" ]
                    [ label [] [ text "Comments:" ]
                    ]
                ]
            ]
        , div [ class "col-xs-12 col-sm-12 padding-h-0 margin-bottom-10" ]
            [ textarea
                [ maybeValue householdMember.comments
                , onInput (UpdateHouseholdMemberComments householdMember)
                , style [ ( "min-width", "99.5%" ) ]
                ]
                []
            ]
        ]


type Msg
    = Load (Result Http.Error ServerResponse)
    | UpdateDemographics SfData
    | UpdateDemographicsAddressMoveInDate SfAddress
    | Save
    | SaveCompleted (Result Http.Error DemographicsResponse)
    | Cancel
    | AddNewLanguage
    | RemoveLanguage PatientLanguagesMap
    | AddNewAddress
    | RemoveAddress PatientAddress
    | AddNewHouseholdMember
    | RemoveHouseholdMember HouseholdMember
      -- Nested Controls
    | UpdateAddressLine1 PatientAddress String
    | UpdateAddressLine2 PatientAddress String
    | UpdateAddressLine3 PatientAddress String
    | UpdateCity PatientAddress String
    | UpdateZipcode PatientAddress String
    | UpdatePreferredAddress PatientAddress Bool
    | UpdatePreferredPhoneNumber Phone
    | UpdatePreferredLanguage PatientLanguagesMap Bool
    | UpdateState PatientAddress ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateAddressType PatientAddress ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateFacilityAddress PatientAddress ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | GetFacilityAddress PatientAddress (Result Http.Error (Maybe FacilityAddress))
    | UpdateLanguage PatientLanguagesMap ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateHouseholdMemberName HouseholdMember String
    | UpdateHouseholdMemberComments HouseholdMember String
    | UpdateHouseholdMemberRelationship HouseholdMember ( Dropdown.DropState, Maybe Int, Cmd Msg )
      -- Nested Contact Hours
    | UpdateContactTimeZone ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdatePreferred DayData Bool
    | UpdateTimingOptions DayData ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateBeginTime DayData ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateEndTime DayData ( Dropdown.DropState, Maybe Int, Cmd Msg )
      -- Edit
    | UpdateFacility ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateFacilityPtID String
    | UpdateMainProvider ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateCareCoordinator ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateSexType ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateSexualOrientation ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateGenderIdentity ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateUSVeteran ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateReligion ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateMedicalRecordNo String
    | UpdatePatientAccountNo String
    | UpdateFirstName String
    | UpdateMiddle String
    | UpdateLastName String
    | UpdateNickname String
    | UpdateBirthPlace String
    | UpdateSSN String
    | UpdateAcuityLevel ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateSexualOrientationNote String
    | UpdateGenderIdentityNote String
    | UpdateEmail String
    | UpdatePrefix ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateSuffix ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateRace ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateEthnicity ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateCellPhone (Maybe Int)
    | UpdateHomePhone (Maybe Int)
    | UpdateWorkPhone (Maybe Int)
    | UpdateCellPhoneMaskState MaskedNumber.State
    | UpdateHomePhoneMaskState MaskedNumber.State
    | UpdateWorkPhoneMaskState MaskedNumber.State


updateAddress : Model -> PatientAddress -> Model
updateAddress model newPatientAddress =
    let
        newAddresses =
            List.map
                (\t ->
                    if t.nodeId == newPatientAddress.nodeId then
                        newPatientAddress
                    else
                        t
                )
                model.patientAddresses
    in
    { model | patientAddresses = newAddresses }


updateLanguage : Model -> PatientLanguagesMap -> Model
updateLanguage model patientLanguagesMap =
    let
        newPatientLanguagesMap =
            List.map
                (\t ->
                    if t.nodeId == patientLanguagesMap.nodeId then
                        patientLanguagesMap
                    else
                        t
                )
                model.patientLanguagesMap
    in
    { model | patientLanguagesMap = newPatientLanguagesMap }


updateHouseholdMembers : Model -> HouseholdMember -> Model
updateHouseholdMembers model householdMember =
    let
        newHouseholdMembers =
            List.map
                (\t ->
                    if t.nodeId == householdMember.nodeId then
                        householdMember
                    else
                        t
                )
                model.householdMembers
    in
    { model | householdMembers = newHouseholdMembers }


updateContactHours : Model -> DayData -> Model
updateContactHours model dayData =
    let
        weekData =
            List.map
                (\t ->
                    if t.idx == dayData.idx then
                        dayData
                    else
                        t
                )
                model.weekData
    in
    { model | weekData = weekData }


togglePreferred : Int -> { c | nodeId : Int, isPreferred : a } -> { c | isPreferred : Bool, nodeId : Int }
togglePreferred nodeId t =
    if t.nodeId == nodeId then
        { t | isPreferred = True }
    else
        { t | isPreferred = False }


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        Load (Ok serverResponse) ->
            let
                newModel =
                    updateModelFromServerMessage serverResponse model

                newPatientLanguagesMap =
                    if List.length newModel.patientLanguagesMap == 0 then
                        [ emptyPatientLanguagesMap 0 True ]
                    else
                        newModel.patientLanguagesMap
                            |> List.indexedMap (\idx t -> { t | nodeId = idx })

                newPatientAddress =
                    if List.length newModel.patientAddresses == 0 then
                        [ emptyPatientAddress 0 True ]
                    else
                        newModel.patientAddresses
                            |> List.indexedMap (\idx t -> { t | nodeId = idx })

                newHouseholdMembers =
                    if List.length newModel.householdMembers == 0 then
                        [ emptyHouseholdMembers 0 ]
                    else
                        newModel.householdMembers
                            |> List.indexedMap (\idx t -> { t | nodeId = idx })

                addresses =
                    newPatientAddress
                        |> List.map (\t -> SfAddress t.nodeId t.moveInDate False)
            in
            { newModel
                | patientLanguagesMap = newPatientLanguagesMap
                , patientAddresses = newPatientAddress
                , householdMembers = newHouseholdMembers
                , nodeCounter = 4
            }
                ! [ initDemographics newModel.sfData
                  , Functions.setLoadingStatus False
                  , initDemographicsAddress addresses
                  ]

        Load (Err t) ->
            model
                ! [ Functions.displayErrorMessage (toString t)
                  , Functions.setLoadingStatus False
                  ]

        UpdateDemographicsAddressMoveInDate address ->
            let
                patientAddresses =
                    model.patientAddresses
                        |> List.map
                            (\t ->
                                if t.nodeId == address.nodeId then
                                    { t
                                        | moveInDate = address.dt
                                    }
                                else
                                    t
                            )
            in
            { model | patientAddresses = patientAddresses } ! []

        UpdateDemographics sfData ->
            { model | sfData = sfData } ! []

        Save ->
            let
                newLangs =
                    model.patientLanguagesMap |> List.filter (\t -> t.languageId /= Nothing)

                newAddresses =
                    model.patientAddresses
                        |> List.filter
                            (\t ->
                                if t.addressType == Just 1 then
                                    case t.facilityAddressId of
                                        Just _ ->
                                            True

                                        Nothing ->
                                            False
                                else
                                    t.addressLine1 /= Nothing && t.city /= Nothing && t.stateId /= Nothing && t.zipCode /= Nothing
                            )

                newModel =
                    { model
                        | patientLanguagesMap = newLangs
                        , patientAddresses = newAddresses
                        , showValidationErrors = False
                        , showContactValidationErrors = False
                    }
            in
            if isAllContactDaysValid model.weekData then
                if List.length (validatationErrors newModel) > 0 then
                    { model | showValidationErrors = True, showContactValidationErrors = False } ! [ scrollToError ]
                else
                    newModel ! [ Functions.setLoadingStatus True, save newModel ]
            else
                { model | showContactValidationErrors = True } ! []

        SaveCompleted (Ok response) ->
            case ( response.success, response.patientId ) of
                ( _, Just newPatientId ) ->
                    model
                        ! [ Functions.displaySuccessMessage "Save completed successfully!"
                          , Functions.setLoadingStatus False
                          , saveSuccess newPatientId
                          , Functions.setUnsavedChanges False
                          ]

                _ ->
                    model ! [ Functions.displayErrorMessage response.message ]

        SaveCompleted (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        Cancel ->
            model ! [ load patientId, Functions.scrollTo "div.body-content" ]

        AddNewLanguage ->
            { model
                | patientLanguagesMap = model.patientLanguagesMap ++ [ emptyPatientLanguagesMap model.nodeCounter False ]
                , nodeCounter = model.nodeCounter + 1
            }
                ! [ Functions.setUnsavedChanges True ]

        RemoveLanguage lang ->
            let
                newPatientLanguagesMap =
                    model.patientLanguagesMap
                        |> List.filter (\t -> t.nodeId /= lang.nodeId)

                updatedPatientLanguagesMap =
                    case List.any (\t -> t.isPreferred == True) newPatientLanguagesMap of
                        True ->
                            newPatientLanguagesMap

                        False ->
                            List.indexedMap
                                (\t y ->
                                    if t == 0 then
                                        { y | isPreferred = True }
                                    else
                                        y
                                )
                                newPatientLanguagesMap
            in
            { model | patientLanguagesMap = updatedPatientLanguagesMap } ! [ Functions.setUnsavedChanges True ]

        AddNewAddress ->
            let
                newPatientAddress =
                    emptyPatientAddress model.nodeCounter False
            in
            { model
                | patientAddresses = model.patientAddresses ++ [ newPatientAddress ]
                , nodeCounter = model.nodeCounter + 1
            }
                ! [ Functions.setUnsavedChanges True
                  , addNewAddress (SfAddress model.nodeCounter newPatientAddress.moveInDate False)
                  ]

        RemoveAddress address ->
            let
                newAddress =
                    model.patientAddresses
                        |> List.filter (\t -> t.nodeId /= address.nodeId)

                jsAddresses =
                    model.patientAddresses
                        |> List.map (\t -> SfAddress t.nodeId t.moveInDate (t.nodeId == address.nodeId))

                updatedAddress =
                    case List.any (\t -> t.isPreferred == True) newAddress of
                        True ->
                            newAddress

                        False ->
                            List.indexedMap
                                (\t y ->
                                    if t == 0 then
                                        { y | isPreferred = True }
                                    else
                                        y
                                )
                                newAddress
            in
            { model | patientAddresses = updatedAddress }
                ! [ Functions.setUnsavedChanges True, initDemographicsAddress jsAddresses ]

        AddNewHouseholdMember ->
            { model
                | householdMembers = model.householdMembers ++ [ emptyHouseholdMembers model.nodeCounter ]
                , nodeCounter = model.nodeCounter + 1
            }
                ! [ Functions.setUnsavedChanges True ]

        RemoveHouseholdMember householdMember ->
            let
                newHouseholdMembers =
                    model.householdMembers
                        |> List.filter (\t -> t.nodeId /= householdMember.nodeId)
            in
            { model | householdMembers = newHouseholdMembers } ! [ Functions.setUnsavedChanges True ]

        -- Nested Controls
        UpdateAddressLine1 patientAddress str ->
            updateAddress model { patientAddress | addressLine1 = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateAddressLine2 patientAddress str ->
            updateAddress model { patientAddress | addressLine2 = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateAddressLine3 patientAddress str ->
            updateAddress model { patientAddress | addressLine3 = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateCity patientAddress str ->
            updateAddress model { patientAddress | city = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateZipcode patientAddress str ->
            updateAddress model { patientAddress | zipCode = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdatePreferredAddress address _ ->
            { model | patientAddresses = List.map (togglePreferred address.nodeId) model.patientAddresses } ! [ Functions.setUnsavedChanges True ]

        UpdatePreferredPhoneNumber phone ->
            { model | preferredPhoneNumber = phone } ! [ Functions.setUnsavedChanges True ]

        UpdatePreferredLanguage language _ ->
            { model | patientLanguagesMap = List.map (togglePreferred language.nodeId) model.patientLanguagesMap } ! [ Functions.setUnsavedChanges True ]

        UpdateHouseholdMemberName householdMember str ->
            updateHouseholdMembers model { householdMember | name = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateHouseholdMemberComments householdMember str ->
            updateHouseholdMembers model { householdMember | comments = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateState t ( newDropState, newId, newMsg ) ->
            updateAddress model { t | addressStateDropState = newDropState, stateId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateAddressType t ( newDropState, newId, newMsg ) ->
            updateAddress model { t | addressTypeDropState = newDropState, addressType = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateFacilityAddress t ( newDropState, newId, newMsg ) ->
            let
                newAddress =
                    { t | facilityAddressDropState = newDropState, facilityAddressId = newId }
            in
            updateAddress model newAddress
                ! [ newMsg
                  , Functions.setUnsavedChanges True
                  , case newId of
                        Just facilityId ->
                            Decode.maybe decodeFacilityAddress
                                |> Http.get ("/People/GetFacilityAddress?facilityId=" ++ toString facilityId)
                                |> Http.send (GetFacilityAddress newAddress)

                        Nothing ->
                            Cmd.none
                  ]

        GetFacilityAddress patientAddress (Ok t) ->
            updateAddress model { patientAddress | facilityAddress = t } ! []

        GetFacilityAddress _ (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        UpdateLanguage t ( newDropState, newId, newMsg ) ->
            updateLanguage model { t | dropState = newDropState, languageId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateHouseholdMemberRelationship t ( newDropState, newId, newMsg ) ->
            updateHouseholdMembers model { t | dropState = newDropState, relationshipId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        -- Begin Contact Hours
        UpdateContactTimeZone ( newDropState, newId, newMsg ) ->
            { model | selectedTimeZoneDropState = newDropState, selectedTimeZoneId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdatePreferred dayData t ->
            updateContactHours model { dayData | preferredDay = t } ! [ Functions.setUnsavedChanges True ]

        UpdateTimingOptions dayData ( newDropState, newId, newMsg ) ->
            updateContactHours model
                { dayData
                    | timingInstructionsDropState = newDropState
                    , timingInstructionsSelectedId = newId
                    , timingInstructions = List.indexedMap (\idx t -> { t | selected = Just idx == newId }) dayData.timingInstructions
                }
                ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateBeginTime dayData ( newDropState, newId, newMsg ) ->
            updateContactHours model
                { dayData
                    | beginTimeDropState = newDropState
                    , beginTimeSelectedId = newId
                    , beginTime = List.indexedMap (\idx t -> { t | selected = Just idx == newId }) dayData.beginTime
                }
                ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateEndTime dayData ( newDropState, newId, newMsg ) ->
            updateContactHours model
                { dayData
                    | endTimeDropState = newDropState
                    , endTimeSelectedId = newId
                    , endTime = List.indexedMap (\idx t -> { t | selected = Just idx == newId }) dayData.endTime
                }
                ! [ newMsg, Functions.setUnsavedChanges True ]

        -- End Contact Hours
        UpdateCellPhone t ->
            let
                preferredPhoneNumber =
                    if model.homePhoneNumber == Nothing && model.workPhoneNumber == Nothing then
                        Cell
                    else
                        model.preferredPhoneNumber
            in
            { model | cellPhoneNumber = t, preferredPhoneNumber = preferredPhoneNumber }
                ! [ Functions.setUnsavedChanges True ]

        UpdateHomePhone t ->
            let
                preferredPhoneNumber =
                    if model.cellPhoneNumber == Nothing && model.workPhoneNumber == Nothing then
                        Home
                    else
                        model.preferredPhoneNumber
            in
            { model | homePhoneNumber = t, preferredPhoneNumber = preferredPhoneNumber }
                ! [ Functions.setUnsavedChanges True ]

        UpdateWorkPhone t ->
            let
                preferredPhoneNumber =
                    if model.homePhoneNumber == Nothing && model.cellPhoneNumber == Nothing then
                        Work
                    else
                        model.preferredPhoneNumber
            in
            { model | workPhoneNumber = t, preferredPhoneNumber = preferredPhoneNumber }
                ! [ Functions.setUnsavedChanges True ]

        UpdateCellPhoneMaskState maskState ->
            { model | cellPhoneNumberMaskState = maskState } ! []

        UpdateHomePhoneMaskState maskState ->
            { model | homePhoneNumberMaskState = maskState } ! []

        UpdateWorkPhoneMaskState maskState ->
            { model | workPhoneNumberMaskState = maskState } ! []

        -- Edit
        UpdateFacility ( newDropState, newId, newMsg ) ->
            { model | facilityDropState = newDropState, facilityId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateFacilityPtID str ->
            { model | facilityPtID = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateMainProvider ( newDropState, newId, newMsg ) ->
            { model | mainProviderDropState = newDropState, mainProviderId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateCareCoordinator ( newDropState, newId, newMsg ) ->
            { model | careCoordinatorDropState = newDropState, careCoordinatorId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateSexType ( newDropState, newId, newMsg ) ->
            { model | sexTypeDropState = newDropState, sexTypeId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateSexualOrientation ( newDropState, newId, newMsg ) ->
            { model | sexualOrientationDropState = newDropState, sexualOrientationId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateGenderIdentity ( newDropState, newId, newMsg ) ->
            { model | genderIdentityDropState = newDropState, genderIdentityId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateUSVeteran ( newDropState, newId, newMsg ) ->
            { model | uSVeteranDropState = newDropState, uSVeteranId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateReligion ( newDropState, newId, newMsg ) ->
            { model | religionDropState = newDropState, religionId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateMedicalRecordNo str ->
            { model | mrn = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdatePatientAccountNo str ->
            { model | patientAccountNumber = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateFirstName str ->
            { model | firstName = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateMiddle str ->
            { model | middle = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateLastName str ->
            { model | lastName = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateNickname str ->
            { model | nickName = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateBirthPlace str ->
            { model | birthPlace = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateSSN str ->
            { model | ssn = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateAcuityLevel ( newDropState, newId, newMsg ) ->
            { model | acuityLevelDropState = newDropState, acuityLevelId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateSexualOrientationNote str ->
            { model | sexualOrientationNote = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateGenderIdentityNote str ->
            { model | genderIdentityNote = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdateEmail str ->
            { model | email = Just str } ! [ Functions.setUnsavedChanges True ]

        UpdatePrefix ( newDropState, newId, newMsg ) ->
            { model | prefixDropState = newDropState, prefixId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateSuffix ( newDropState, newId, newMsg ) ->
            { model | suffixDropState = newDropState, suffixId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateRace ( newDropState, newId, newMsg ) ->
            { model | raceDropState = newDropState, raceId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]

        UpdateEthnicity ( newDropState, newId, newMsg ) ->
            { model | ethnicityDropState = newDropState, ethnicityId = newId } ! [ newMsg, Functions.setUnsavedChanges True ]



-- HELPER Functions


maybeValue : Maybe String -> Html.Attribute msg
maybeValue str =
    defaultValue (Maybe.withDefault "" str)


rowStyle : List (Html.Attribute msg)
rowStyle =
    [ class "col-xs-12 col-sm-12 col-md-5 col-lg-4 padding-left-0" ]


isRequiredClass : Bool -> Html.Attribute msg
isRequiredClass isRequired =
    case isRequired of
        True ->
            class "required "

        False ->
            class ""


commonStructureWithCustomAttr : String -> Bool -> Html.Attribute msg -> Html msg -> Html msg
commonStructureWithCustomAttr displayText isRequired attr t =
    div [ class "col-xs-12 padding-h-0", attr ]
        [ label [ isRequiredClass isRequired ] [ text (displayText ++ ":") ]
        , div [ class "DemographicsInputDiv padding-h-0" ]
            [ t ]
        ]


commonStructure : String -> Bool -> Html msg -> Html msg
commonStructure displayText isRequired t =
    commonStructureWithCustomAttr displayText isRequired (class "") t


noNumbers : Html.Attribute msg
noNumbers =
    attribute "onkeypress" "return event.charCode < 48 || event.charCode > 57"


maskStyle : Html.Attribute msg
maskStyle =
    style [ ( "margin-left", "5px" ), ( "margin-top", "5px" ) ]


maxLength : Maybe Int -> Html.Attribute msg
maxLength maybeMax =
    case maybeMax of
        Just t ->
            maxlength t

        Nothing ->
            maxlength -1


textboxInner : Maybe Int -> String -> Bool -> Maybe String -> (String -> msg) -> Html msg
textboxInner maybeMax displayText isRequired maybeStr event =
    commonStructure displayText isRequired <|
        input [ type_ "text", id (Functions.idAttr displayText), maybeValue maybeStr, class "e-textbox", onInput event, maxLength maybeMax ] []


textbox : String -> Bool -> Maybe String -> (String -> msg) -> Html msg
textbox displayText isRequired maybeStr event =
    textboxInner Nothing displayText isRequired maybeStr event


nonumberbox : String -> Bool -> Maybe String -> (String -> msg) -> Html msg
nonumberbox displayText isRequired maybeStr event =
    commonStructure displayText isRequired <|
        input [ type_ "text", id (Functions.idAttr displayText), maybeValue maybeStr, noNumbers, class "e-textbox", onInput event ] []


dropbox : String -> Bool -> Html msg -> Html msg
dropbox displayText isRequired t =
    commonStructure displayText isRequired <|
        t


sfbox : String -> Bool -> Html msg
sfbox displayText isRequired =
    commonStructure displayText isRequired <|
        input [ type_ "text", id (Functions.idAttr displayText), class "e-textbox" ] []


requireInt : String -> Maybe Int -> Maybe String
requireInt fieldName maybeInt =
    case maybeInt of
        Nothing ->
            Just (fieldName ++ " is required")

        --Just defaultErrorMsg
        Just _ ->
            Nothing


requireString : String -> Maybe String -> Maybe String
requireString fieldName maybeStr =
    if Maybe.withDefault "" maybeStr == "" then
        Just (fieldName ++ " is required")
        --Just defaultErrorMsg
    else
        Nothing


addressValidation : PatientAddress -> Maybe String
addressValidation address =
    (if address.addressType == Just 1 then
        [ requireInt "Facility" address.facilityAddressId
        ]
     else
        [ requireString "Address Line 1" address.addressLine1
        , requireString "City" address.city
        , requireInt "State" address.stateId
        , requireString "Zip Code" address.zipCode
        ]
    )
        |> List.filterMap identity
        |> List.head


phoneDuplicateValidation : Model -> Maybe String
phoneDuplicateValidation model =
    let
        phones =
            [ model.cellPhoneNumber, model.homePhoneNumber, model.workPhoneNumber ]
                |> List.filterMap identity
    in
    if List.allDifferent phones then
        Nothing
    else
        case List.head phones of
            Just phoneNumber ->
                Just ("Duplicate phone number \"" ++ toString phoneNumber ++ "\"")

            Nothing ->
                Nothing


atleast1 : List a -> String -> Maybe String
atleast1 items msg =
    case List.head items of
        Just _ ->
            Nothing

        Nothing ->
            Just msg


validatationErrors : Model -> List String
validatationErrors model =
    let
        maybeRequired1 =
            if model.mrnRequired then
                [ requireString "Medical Record No" model.mrn ]
            else
                []

        maybeRequired2 =
            if model.facilityPtIDRequired then
                [ requireString "Patient's Facility ID No" model.facilityPtID ]
            else
                []
    in
    maybeRequired1
        ++ maybeRequired2
        ++ [ requireInt "Facility" model.facilityId
           , requireInt "Main Provider" model.mainProviderId
           , requireInt "Care Coordinator" model.careCoordinatorId
           , requireString "First Name" model.firstName
           , requireString "Last Name" model.lastName
           , requireString "Date of Birth" model.sfData.dateOfBirth
           , requireInt "Sex at Birth" model.sexTypeId
           , model.patientAddresses
                |> List.filterMap addressValidation
                |> List.head
           , phoneDuplicateValidation model
           , atleast1 model.patientAddresses "At least one address is required."
           , atleast1 [ model.cellPhoneNumber, model.homePhoneNumber, model.workPhoneNumber ] "At least one phone is required."
           ]
        |> List.filterMap identity


viewValidationErrorsDiv : Model -> List String -> Html msg
viewValidationErrorsDiv model errors =
    div [ class "error", hidden (List.length errors == 0 || model.showValidationErrors == False) ]
        (List.map (\t -> div [] [ text t ]) errors)


emptyModel : Int -> Model
emptyModel patientId =
    { facilityId = Nothing
    , facilityDropState = Dropdown.init { defaultDropConfig | domId = "facilityDropdown", showSearchText = True }
    , mainProviderId = Nothing
    , mainProviderDropState = Dropdown.init { defaultDropConfig | domId = "mainProviderDropdown", showSearchText = True }
    , careCoordinatorId = Nothing
    , careCoordinatorDropState = Dropdown.init { defaultDropConfig | domId = "careCoordinatorDropdown", showSearchText = True }
    , sexTypeId = Nothing
    , sexTypeDropState = Dropdown.init { defaultDropConfig | domId = "sexTypeDropdown" }
    , sexualOrientationId = Nothing
    , sexualOrientationDropState = Dropdown.init { defaultDropConfig | domId = "sexualOrientationDropdown" }
    , genderIdentityId = Nothing
    , genderIdentityDropState = Dropdown.init { defaultDropConfig | domId = "genderIdentityDropdown" }
    , uSVeteranId = Nothing
    , uSVeteranDropState = Dropdown.init { defaultDropConfig | domId = "uSVeteranDropdown" }
    , religionId = Nothing
    , religionDropState = Dropdown.init { defaultDropConfig | domId = "religionDropdown" }
    , patientId = patientId
    , demographicsId = Nothing
    , nickName = Nothing
    , ssn = Nothing
    , lastName = Nothing
    , firstName = Nothing
    , middle = Nothing
    , birthPlace = Nothing
    , mrn = Nothing
    , mrnRequired = False
    , patientAccountNumber = Nothing
    , facilityPtID = Nothing
    , facilityPtIDRequired = False
    , sexualOrientationNote = Nothing
    , genderIdentityNote = Nothing
    , email = Nothing
    , sfData = emptySfData
    , patientLanguagesMap = []
    , householdMembers = []
    , patientAddresses = []
    , stateDropdown = []
    , primaryAddressIndex = 0

    -- Start Contact Hours
    , tZ = []
    , weekData = []
    , selectedTimeZoneId = Nothing
    , selectedTimeZoneDropState = Dropdown.init { defaultDropConfig | domId = "selectedTimeZone" }
    , showContactValidationErrors = False

    -- End Contact Hours
    , showValidationErrors = False
    , suffixId = Nothing
    , suffixDropState = Dropdown.init { defaultDropConfig | domId = "suffixDropdown" }
    , prefixId = Nothing
    , prefixDropState = Dropdown.init { defaultDropConfig | domId = "prefixDropdown" }
    , raceId = Nothing
    , raceDropState = Dropdown.init { defaultDropConfig | domId = "raceDropdown" }
    , ethnicityId = Nothing
    , ethnicityDropState = Dropdown.init { defaultDropConfig | domId = "ethnicityDropdown" }
    , acuityLevelId = Nothing
    , acuityLevelDropState = Dropdown.init { defaultDropConfig | domId = "acuityLevelDropdown" }
    , homePhoneNumber = Nothing
    , cellPhoneNumber = Nothing
    , workPhoneNumber = Nothing
    , homePhoneNumberMaskState = MaskedNumber.initialState
    , cellPhoneNumberMaskState = MaskedNumber.initialState
    , workPhoneNumberMaskState = MaskedNumber.initialState
    , preferredPhoneNumber = Home
    , nodeCounter = 0
    , drops = emptyDrops
    }


emptySfData : SfData
emptySfData =
    { dateOfBirth = Nothing
    , dateOfDeath = Nothing
    , vip = Nothing
    }


emptyPatientLanguagesMap : Int -> Bool -> PatientLanguagesMap
emptyPatientLanguagesMap nodeCounter isPreferred =
    { id = Nothing
    , languageId = Nothing
    , isPreferred = isPreferred
    , dropState = Dropdown.init { defaultDropConfig | domId = "languageDropdown" }
    , nodeId = nodeCounter
    }


emptyHouseholdMembers : Int -> HouseholdMember
emptyHouseholdMembers nodeCounter =
    { id = Nothing
    , name = Nothing
    , relationshipId = Nothing
    , comments = Nothing
    , dropState = Dropdown.init { defaultDropConfig | domId = "householdMembersDropdown" }
    , nodeId = nodeCounter
    }


emptyPatientAddress : Int -> Bool -> PatientAddress
emptyPatientAddress nodeCounter isPreferred =
    { id = Nothing
    , addressLine1 = Nothing
    , addressLine2 = Nothing
    , addressLine3 = Nothing
    , city = Nothing
    , stateId = Nothing
    , zipCode = Nothing
    , isPreferred = isPreferred
    , moveInDate = Nothing
    , facilityAddress = Nothing
    , facilityAddressId = Nothing
    , addressType = Nothing
    , facilityAddressDropState = Dropdown.init { defaultDropConfig | domId = "facilityAddressDropdown" }
    , addressTypeDropState = Dropdown.init { defaultDropConfig | domId = "addressDropdown" }
    , addressStateDropState = Dropdown.init { defaultDropConfig | domId = "stateDropdown" }
    , nodeId = nodeCounter
    }


emptyDrops : DropdownSource
emptyDrops =
    { languageDropdown = []
    , ethnicityDropdown = []
    , sexTypeDropdown = []
    , sexualOrientationDropdown = []
    , genderIdentityDropdown = []
    , raceDropdown = []
    , suffixDropdown = []
    , prefixDropdown = []
    , uSVeteranDropdown = []
    , religionDropdown = []
    , careCoordinatorDropdown = []
    , facilityDropdown = []
    , mainProviderDropdown = []
    }


updateModelFromServerMessage : ServerResponse -> Model -> Model
updateModelFromServerMessage { d, c, contactHoursModel, ds } model =
    { model
        | demographicsId = d.demographicsId
        , facilityId = d.facilityId
        , mainProviderId = d.mainProviderId
        , careCoordinatorId = d.careCoordinatorId
        , sexTypeId = d.sexTypeId
        , sexualOrientationId = d.genderIdentityId
        , genderIdentityId = d.genderIdentityId
        , uSVeteranId = d.uSVeteranId
        , religionId = d.religionId
        , nickName = d.nickName
        , ssn = d.ssn
        , lastName = d.lastName
        , firstName = d.firstName
        , middle = d.middle
        , birthPlace = d.birthPlace
        , mrn = d.mrn
        , mrnRequired = d.mrnRequired
        , patientAccountNumber = d.patientAccountNumber
        , facilityPtID = d.facilityPtID
        , facilityPtIDRequired = d.facilityPtIDRequired
        , sexualOrientationNote = d.sexualOrientationNote
        , genderIdentityNote = d.genderIdentityNote
        , email = d.email
        , sfData = d.sfData
        , patientLanguagesMap = d.patientLanguagesMap
        , householdMembers = d.householdMembers
        , patientAddresses = c.patientAddresses
        , primaryAddressIndex = c.primaryAddressIndex

        -- Start Contact Hours
        , tZ = contactHoursModel.tZ
        , weekData = List.indexedMap dayDataFromServer contactHoursModel.weekData
        , selectedTimeZoneId = List.findIndex (\t -> t.selected) contactHoursModel.tZ
        , selectedTimeZoneDropState = contactHoursModel.selectedTimeZoneDropState

        -- End Contact Hours
        , suffixId = d.suffixId
        , prefixId = d.prefixId
        , raceId = d.raceId
        , ethnicityId = d.ethnicityId
        , acuityLevelId = d.acuityLevelId
        , stateDropdown = c.stateDropdown
        , preferredPhoneNumber = d.preferredPhoneNumber
        , homePhoneNumber = d.homePhoneNumber
        , cellPhoneNumber = d.cellPhoneNumber
        , workPhoneNumber = d.workPhoneNumber
        , drops = ds
    }



-- JSON Decoding


type alias DemographicsInformationModel =
    { facilityId : Maybe Int
    , mainProviderId : Maybe Int
    , careCoordinatorId : Maybe Int
    , sexTypeId : Maybe Int
    , sexualOrientationId : Maybe Int
    , genderIdentityId : Maybe Int
    , uSVeteranId : Maybe Int
    , religionId : Maybe Int
    , patientId : Int
    , demographicsId : Maybe Int
    , nickName : Maybe String
    , ssn : Maybe String
    , lastName : Maybe String
    , firstName : Maybe String
    , middle : Maybe String
    , birthPlace : Maybe String
    , mrn : Maybe String
    , mrnRequired : Bool
    , patientAccountNumber : Maybe String
    , facilityPtID : Maybe String
    , facilityPtIDRequired : Bool
    , sexualOrientationNote : Maybe String
    , genderIdentityNote : Maybe String
    , email : Maybe String
    , sfData : SfData
    , patientLanguagesMap : List PatientLanguagesMap
    , householdMembers : List HouseholdMember
    , suffixId : Maybe Int
    , prefixId : Maybe Int
    , raceId : Maybe Int
    , ethnicityId : Maybe Int
    , acuityLevelId : Maybe Int
    , preferredPhoneNumber : Phone
    , homePhoneNumber : Maybe Int
    , cellPhoneNumber : Maybe Int
    , workPhoneNumber : Maybe Int
    }


type alias ContactInformationModel =
    { patientAddresses : List PatientAddress
    , stateDropdown : List DropdownItem
    , primaryAddressIndex : Int
    }



-- Contact Hours section, sectioned off because of the ICK factor.


timeZoneDropdown : List DropdownItem
timeZoneDropdown =
    [ DropdownItem (Just 0) "Central Standard Time"
    , DropdownItem (Just 1) "Eastern Standard Time"
    , DropdownItem (Just 2) "Mountain Standard Time"
    , DropdownItem (Just 3) "Pacific Standard Time"
    , DropdownItem (Just 4) "Coordinated Universal Time"
    ]


type TimingOption
    = Anytime
    | Between
    | Before
    | After


timingOption : Maybe Int -> TimingOption
timingOption dropdownId =
    case dropdownId of
        Just 0 ->
            Anytime

        Just 1 ->
            Between

        Just 2 ->
            Before

        Just 3 ->
            After

        _ ->
            Debug.crash "Invalid Timing Option"


timingOptionsDropdown : List DropdownItem
timingOptionsDropdown =
    [ DropdownItem (Just 0) "Anytime"
    , DropdownItem (Just 1) "Between"
    , DropdownItem (Just 2) "Before"
    , DropdownItem (Just 3) "After"
    ]


encodeContactHoursModel : ContactHoursModel -> Encode.Value
encodeContactHoursModel contactHoursModel =
    Encode.object
        [ ( "TZ", Encode.list (List.map encodeSelectList contactHoursModel.tZ) )
        ]


encodeSelectList : SelectList -> Encode.Value
encodeSelectList selectList =
    Encode.object
        [ ( "Disabled", Encode.bool selectList.disabled )
        , ( "Group", maybeVal Encode.string selectList.group )
        , ( "Selected", Encode.bool selectList.selected )
        , ( "Text", maybeVal Encode.string selectList.text )
        , ( "Value", maybeVal Encode.string selectList.value )
        ]


encodeDayData : DayData -> Encode.Value
encodeDayData dayData =
    let
        maybeBeginTime =
            dayData.beginTime
                |> List.filter (\t -> t.selected)
                |> List.head

        maybeEndTime =
            dayData.endTime
                |> List.filter (\t -> t.selected)
                |> List.head

        maybeTimingInstructions =
            dayData.timingInstructions
                |> List.filter (\t -> t.selected)
                |> List.head
    in
    case ( maybeBeginTime, maybeEndTime, maybeTimingInstructions ) of
        ( Just beginTime, Just endTime, Just timingInstructions ) ->
            Encode.object
                [ ( "BeginTime", encodeSelectList beginTime )
                , ( "EndTime", encodeSelectList endTime )
                , ( "PreferredDay", Encode.bool dayData.preferredDay )
                , ( "TimingInstructions", encodeSelectList timingInstructions )
                , ( "WeekDay", maybeVal Encode.string dayData.weekDay )
                ]

        _ ->
            Debug.crash "invalid contact hours"


decodeContactHoursModel : Decode.Decoder ContactHoursModel
decodeContactHoursModel =
    Pipeline.decode ContactHoursModel
        |> Pipeline.required "TZ" (Decode.list decodeSelectList)
        |> Pipeline.required "WeekData" (Decode.list decodeDayData)
        |> Pipeline.required "SelectedTimeZoneId" (Decode.maybe Decode.int)
        |> Pipeline.hardcoded (Dropdown.init { defaultDropConfig | domId = "selectedTimeZone" })


decodeSelectList : Decode.Decoder SelectList
decodeSelectList =
    Pipeline.decode SelectList
        |> Pipeline.required "Disabled" Decode.bool
        |> Pipeline.required "Group" (Decode.maybe Decode.string)
        |> Pipeline.optional "Selected" Decode.bool False
        |> Pipeline.required "Text" (Decode.maybe Decode.string)
        |> Pipeline.required "Value" (Decode.maybe Decode.string)


decodeDayData : Decode.Decoder ServerDayData
decodeDayData =
    Pipeline.decode ServerDayData
        |> Pipeline.required "WeekDay" (Decode.maybe Decode.string)
        |> Pipeline.required "PreferredDay" Decode.bool
        |> Pipeline.required "TimingInstructions" (Decode.list decodeSelectList)
        |> Pipeline.required "BeginTime" (Decode.list decodeSelectList)
        |> Pipeline.required "EndTime" (Decode.list decodeSelectList)


dayDataFromServer : Int -> ServerDayData -> DayData
dayDataFromServer idx serverDayData =
    { idx = idx
    , weekDay = serverDayData.weekDay
    , preferredDay = serverDayData.preferredDay
    , timingInstructions = serverDayData.timingInstructions
    , timingInstructionsSelectedId = List.findIndex (\t -> t.selected) serverDayData.timingInstructions
    , timingInstructionsDropState =
        Dropdown.init
            { defaultDropConfig
                | domId = "timingInstructions" ++ toString idx
                , width = Dropdown.Exactly 100
                , height = Dropdown.Exactly 18
            }
    , beginTime = serverDayData.beginTime
    , beginTimeSelectedId = List.findIndex (\t -> t.selected) serverDayData.beginTime
    , beginTimeDropState =
        Dropdown.init
            { defaultDropConfig
                | domId = "beginTime" ++ toString idx
                , width = Dropdown.Exactly 100
                , height = Dropdown.Exactly 18
            }
    , endTime = serverDayData.endTime
    , endTimeSelectedId = List.findIndex (\t -> t.selected) serverDayData.endTime
    , endTimeDropState =
        Dropdown.init
            { defaultDropConfig
                | domId = "endTime" ++ toString idx
                , width = Dropdown.Exactly 100
                , height = Dropdown.Exactly 18
            }
    }


type alias ServerDayData =
    { weekDay : Maybe String
    , preferredDay : Bool
    , timingInstructions : List SelectList
    , beginTime : List SelectList
    , endTime : List SelectList
    }


type alias DayData =
    { idx : Int
    , weekDay : Maybe String
    , preferredDay : Bool
    , timingInstructions : List SelectList
    , timingInstructionsSelectedId : Maybe Int
    , timingInstructionsDropState : Dropdown.DropState
    , beginTime : List SelectList
    , beginTimeSelectedId : Maybe Int
    , beginTimeDropState : Dropdown.DropState
    , endTime : List SelectList
    , endTimeSelectedId : Maybe Int
    , endTimeDropState : Dropdown.DropState
    }


type alias ContactHoursModel =
    { tZ : List SelectList
    , weekData : List ServerDayData
    , selectedTimeZoneId : Maybe Int
    , selectedTimeZoneDropState : Dropdown.DropState
    }


type alias SelectList =
    { disabled : Bool
    , group : Maybe String
    , selected : Bool
    , text : Maybe String
    , value : Maybe String
    }


viewContactHours : Model -> Html Msg
viewContactHours model =
    div [ id "DemographicsForm", class "col-xs-12 col-sm-8 col-md-10 padding-h-0" ]
        [ h4 [ class "col-xs-12 padding-h-0 padding-top-0" ] [ text "Contact Hours" ]
        , div [ class "col-xs-12 padding-h-0 col-sm-12 col-md-12 padding-h-0" ]
            [ if isAllContactDaysValid model.weekData then
                div [] []
              else
                div [ id "MasterPCHError", class "error" ]
                    [ text "Errors with contact hours must be corrected before submission"
                    ]
            , table [ class "PatientContactHoursGrid" ]
                [ tbody []
                    ([ tr [ class "padding-h-0" ]
                        [ td [ colspan 1, style [ ( "display", "block" ), ( "width", "250px" ), ( "padding-top", "10px" ), ( "padding-bottom", "10px" ) ] ]
                            [ Dropdown.view model.selectedTimeZoneDropState UpdateContactTimeZone timeZoneDropdown model.selectedTimeZoneId
                            ]
                        ]
                     , tr [ class "col-xs-12 padding-h-0" ]
                        [ td [ class "Day" ] [ b [] [ text "Day" ] ]
                        , td [ class "Preferred" ] [ b [] [ text "Preferred" ] ]
                        , td [ class "TimingOptions" ] [ b [] [ text "Timing Options" ] ]
                        , td [ class "Times" ] [ b [] [ text "Begin Time" ] ]
                        , td [ class "Times" ] [ b [] [ text "End Time" ] ]
                        ]
                     ]
                        ++ List.indexedMap viewContactHoursRow model.weekData
                    )
                ]
            ]
        ]


isAllContactDaysValid : List DayData -> Bool
isAllContactDaysValid weekData =
    List.all isContactDayValid weekData


isContactDayValid : DayData -> Bool
isContactDayValid dayData =
    let
        beginId =
            Maybe.withDefault -1 dayData.beginTimeSelectedId

        endId =
            Maybe.withDefault -1 dayData.endTimeSelectedId

        isOptionBetween =
            dayData.timingInstructionsSelectedId == Just 1
    in
    if isOptionBetween then
        if beginId <= endId then
            True
        else
            False
    else
        True


viewContactHoursRow : Int -> DayData -> Html Msg
viewContactHoursRow idx dayData =
    let
        beginTimeEnabled =
            case timingOption dayData.timingInstructionsSelectedId of
                Anytime ->
                    False

                Between ->
                    True

                Before ->
                    True

                After ->
                    True

        endTimeEnabled =
            case timingOption dayData.timingInstructionsSelectedId of
                Anytime ->
                    False

                Between ->
                    True

                Before ->
                    False

                After ->
                    False

        beforeError =
            if isContactDayValid dayData then
                []
            else
                [ b [ style [ ( "color", "red" ), ( "font-size", "0.75em" ) ] ]
                    [ text "Must fall before End Time"
                    ]
                ]
    in
    tr [ class "col-xs-12 padding-h-0" ]
        [ td [ class "Day" ]
            [ b [ style [ ( "text-align", "right" ) ] ] [ text (Functions.defaultString dayData.weekDay) ]
            ]
        , td [ class "Preferred" ]
            [ input [ type_ "checkbox", checked dayData.preferredDay, onCheck (UpdatePreferred dayData) ] []
            ]
        , td [ class "TimingOptions" ]
            [ Dropdown.view dayData.timingInstructionsDropState (UpdateTimingOptions dayData) timingOptionsDropdown dayData.timingInstructionsSelectedId
            ]
        , td [ class "Times" ] <|
            [ Dropdown.viewWithEnabled beginTimeEnabled dayData.beginTimeDropState (UpdateBeginTime dayData) (List.map selectListToDropdownItem dayData.beginTime) dayData.beginTimeSelectedId
            ]
                ++ beforeError
        , td [ class "Times" ]
            [ Dropdown.viewWithEnabled endTimeEnabled dayData.endTimeDropState (UpdateEndTime dayData) (List.map selectListToDropdownItem dayData.endTime) dayData.endTimeSelectedId
            ]
        ]


selectListToDropdownItem : SelectList -> DropdownItem
selectListToDropdownItem selectList =
    DropdownItem (Functions.stringToInt (Functions.defaultString selectList.value)) (Functions.defaultString selectList.text)



-- End Contact Hours section


type alias ServerResponse =
    { d : DemographicsInformationModel
    , c : ContactInformationModel
    , contactHoursModel : ContactHoursModel
    , ds : DropdownSource
    }


decodePhone : Decode.Decoder Phone
decodePhone =
    Decode.maybe Decode.int
        |> Decode.andThen
            (\t ->
                case t of
                    Just 0 ->
                        Decode.succeed Home

                    Just 1 ->
                        Decode.succeed Cell

                    Just 2 ->
                        Decode.succeed Work

                    Just 3 ->
                        Decode.succeed Unknown

                    somethingElse ->
                        Decode.fail <| "unknown phone: " ++ toString somethingElse
            )


encodePhone : Phone -> Int
encodePhone phone =
    case phone of
        Home ->
            0

        Cell ->
            1

        Work ->
            2

        Unknown ->
            3


decodeServerResponse : Decode.Decoder ServerResponse
decodeServerResponse =
    Pipeline.decode ServerResponse
        |> Pipeline.required "demographicsInformationModel" decodeDemographicsInformationModel
        |> Pipeline.required "contactInformationModel" decodeContactInformationModel
        |> Pipeline.required "contactHoursModel" decodeContactHoursModel
        |> Pipeline.required "demographicLists" decodeLists


decodeDemographicsInformationModel : Decode.Decoder DemographicsInformationModel
decodeDemographicsInformationModel =
    Pipeline.decode DemographicsInformationModel
        |> Pipeline.required "FacilityId" (Decode.maybe Decode.int)
        |> Pipeline.required "MainProviderId" (Decode.maybe Decode.int)
        |> Pipeline.required "CareCoordinatorId" (Decode.maybe Decode.int)
        |> Pipeline.required "SexTypeId" (Decode.maybe Decode.int)
        |> Pipeline.required "SexualOrientationId" (Decode.maybe Decode.int)
        |> Pipeline.required "GenderIdentityId" (Decode.maybe Decode.int)
        |> Pipeline.required "USVeteranId" (Decode.maybe Decode.int)
        |> Pipeline.required "ReligionId" (Decode.maybe Decode.int)
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "DemographicsId" (Decode.maybe Decode.int)
        |> Pipeline.required "NickName" (Decode.maybe Decode.string)
        |> Pipeline.required "SSN" (Decode.maybe Decode.string)
        |> Pipeline.required "LastName" (Decode.maybe Decode.string)
        |> Pipeline.required "FirstName" (Decode.maybe Decode.string)
        |> Pipeline.required "Middle" (Decode.maybe Decode.string)
        |> Pipeline.required "BirthPlace" (Decode.maybe Decode.string)
        |> Pipeline.required "MRN" (Decode.maybe Decode.string)
        |> Pipeline.required "MRNRequired" Decode.bool
        |> Pipeline.required "PatientAccountNumber" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityPtID" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityPtIDRequired" Decode.bool
        |> Pipeline.required "SexualOrientationNote" (Decode.maybe Decode.string)
        |> Pipeline.required "GenderIdentityNote" (Decode.maybe Decode.string)
        |> Pipeline.required "Email" (Decode.maybe Decode.string)
        |> Pipeline.custom decodeSfData
        |> Pipeline.required "PatientLanguagesMap" (Decode.list decodePatientLanguagesMap)
        |> Pipeline.required "HouseholdMembers" (Decode.list decodeHouseholdMembers)
        |> Pipeline.required "SuffixId" (Decode.maybe Decode.int)
        |> Pipeline.required "PrefixId" (Decode.maybe Decode.int)
        |> Pipeline.required "RaceId" (Decode.maybe Decode.int)
        |> Pipeline.required "EthnicityId" (Decode.maybe Decode.int)
        |> Pipeline.required "AcuityLevelId" (Decode.maybe Decode.int)
        |> Pipeline.required "PreferredPhoneNumber" decodePhone
        |> Pipeline.required "HomePhoneNumber" (Decode.maybe Decode.int)
        |> Pipeline.required "CellPhoneNumber" (Decode.maybe Decode.int)
        |> Pipeline.required "WorkPhoneNumber" (Decode.maybe Decode.int)


decodeContactInformationModel : Decode.Decoder ContactInformationModel
decodeContactInformationModel =
    Pipeline.decode ContactInformationModel
        |> Pipeline.required "PatientAddresses" (Decode.list decodePatientAddress)
        |> Pipeline.required "StateDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "PrimaryAddressIndex" Decode.int


decodePatientLanguagesMap : Decode.Decoder PatientLanguagesMap
decodePatientLanguagesMap =
    Pipeline.decode PatientLanguagesMap
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "LanguageId" (Decode.maybe Decode.int)
        |> Pipeline.required "IsPreferred" Decode.bool
        |> Pipeline.hardcoded (Dropdown.init { defaultDropConfig | domId = "languageDropdown" })
        |> Pipeline.hardcoded 0


decodeHouseholdMembers : Decode.Decoder HouseholdMember
decodeHouseholdMembers =
    Pipeline.decode HouseholdMember
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "Name" (Decode.maybe Decode.string)
        |> Pipeline.required "RelationshipId" (Decode.maybe Decode.int)
        |> Pipeline.required "Comments" (Decode.maybe Decode.string)
        |> Pipeline.hardcoded (Dropdown.init { defaultDropConfig | domId = "householdMembersDropdown" })
        |> Pipeline.hardcoded 0


decodePatientAddress : Decode.Decoder PatientAddress
decodePatientAddress =
    Pipeline.decode PatientAddress
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "AddressLine1" (Decode.maybe Decode.string)
        |> Pipeline.required "AddressLine2" (Decode.maybe Decode.string)
        |> Pipeline.required "AddressLine3" (Decode.maybe Decode.string)
        |> Pipeline.required "City" (Decode.maybe Decode.string)
        |> Pipeline.required "StateId" (Decode.maybe Decode.int)
        |> Pipeline.required "ZipCode" (Decode.maybe Decode.string)
        |> Pipeline.required "IsPrimary" Decode.bool
        |> Pipeline.required "MoveInDate" (Decode.maybe Decode.string)
        |> Pipeline.required "AddressType" (Decode.maybe Decode.int)
        |> Pipeline.required "FacilityAddress" (Decode.maybe decodeFacilityAddress)
        |> Pipeline.required "FacilityId" (Decode.maybe Decode.int)
        |> Pipeline.hardcoded (Dropdown.init { defaultDropConfig | domId = "facilityAddressDropdown" })
        |> Pipeline.hardcoded (Dropdown.init { defaultDropConfig | domId = "stateDropdown" })
        |> Pipeline.hardcoded (Dropdown.init { defaultDropConfig | domId = "addressDropdown" })
        |> Pipeline.hardcoded 0


decodeSfData : Decode.Decoder SfData
decodeSfData =
    Pipeline.decode SfData
        |> Pipeline.required "DateOfBirth" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDeath" (Decode.maybe Decode.string)
        |> Pipeline.required "VIP" (Decode.maybe Decode.bool)


decodeFacilityAddress : Decode.Decoder FacilityAddress
decodeFacilityAddress =
    Pipeline.decode FacilityAddress
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "Address" (Decode.maybe Decode.string)
        |> Pipeline.required "City" (Decode.maybe Decode.string)
        |> Pipeline.required "StateId" (Decode.maybe Decode.int)
        |> Pipeline.required "ZipCode" (Decode.maybe Decode.string)


type alias DropdownSource =
    { languageDropdown : List DropdownItem
    , ethnicityDropdown : List DropdownItem
    , sexTypeDropdown : List DropdownItem
    , sexualOrientationDropdown : List DropdownItem
    , genderIdentityDropdown : List DropdownItem
    , raceDropdown : List DropdownItem
    , suffixDropdown : List DropdownItem
    , prefixDropdown : List DropdownItem
    , uSVeteranDropdown : List DropdownItem
    , religionDropdown : List DropdownItem
    , careCoordinatorDropdown : List DropdownItem
    , facilityDropdown : List DropdownItem
    , mainProviderDropdown : List DropdownItem
    }


decodeLists : Decode.Decoder DropdownSource
decodeLists =
    Pipeline.decode DropdownSource
        |> Pipeline.required "LanguageDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "EthnicityDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "SexTypeDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "SexualOrientationDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "GenderIdentityDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "RaceDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "SuffixDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "PrefixDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "USVeteranDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "ReligionDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "CareCoordinatorDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "FacilityDropdown" (Decode.list decodeDropdownItem)
        |> Pipeline.required "MainProviderDropdown" (Decode.list decodeDropdownItem)



-- ENCODING


encodePatientLanguagesMap : PatientLanguagesMap -> Encode.Value
encodePatientLanguagesMap lang =
    Encode.object
        [ ( "Id ", maybeVal Encode.int lang.id )
        , ( "LanguageId", maybeVal Encode.int lang.languageId )
        , ( "IsPreferred", Encode.bool lang.isPreferred )
        ]


encodeHouseholdMembers : HouseholdMember -> Encode.Value
encodeHouseholdMembers householdMember =
    Encode.object
        [ ( "Id", maybeVal Encode.int householdMember.id )
        , ( "Name", maybeVal Encode.string householdMember.name )
        , ( "RelationshipId", maybeVal Encode.int householdMember.relationshipId )
        , ( "Comments", maybeVal Encode.string householdMember.comments )
        ]


encodeDemographicsInformationModel : Model -> Encode.Value
encodeDemographicsInformationModel model =
    Encode.object
        [ ( "PatientId", Encode.int model.patientId )
        , ( "DemographicsId", maybeVal Encode.int model.demographicsId )
        , ( "NickName", maybeVal Encode.string model.nickName )
        , ( "SSN", maybeVal Encode.string model.ssn )
        , ( "LastName", maybeVal Encode.string model.lastName )
        , ( "FirstName", maybeVal Encode.string model.firstName )
        , ( "Middle", maybeVal Encode.string model.middle )
        , ( "BirthPlace", maybeVal Encode.string model.birthPlace )
        , ( "MRN", maybeVal Encode.string model.mrn )
        , ( "PatientAccountNumber", maybeVal Encode.string model.patientAccountNumber )
        , ( "FacilityPtID", maybeVal Encode.string model.facilityPtID )
        , ( "SexualOrientationNote", maybeVal Encode.string model.sexualOrientationNote )
        , ( "GenderIdentityNote", maybeVal Encode.string model.genderIdentityNote )
        , ( "Email", maybeVal Encode.string model.email )
        , ( "FacilityId", maybeVal Encode.int model.facilityId )
        , ( "MainProviderId", maybeVal Encode.int model.mainProviderId )
        , ( "CareCoordinatorId", maybeVal Encode.int model.careCoordinatorId )
        , ( "PrefixId", maybeVal Encode.int model.prefixId )
        , ( "SexTypeId", maybeVal Encode.int model.sexTypeId )
        , ( "SexualOrientationId", maybeVal Encode.int model.sexualOrientationId )
        , ( "SuffixId", maybeVal Encode.int model.suffixId )
        , ( "GenderIdentityId", maybeVal Encode.int model.genderIdentityId )
        , ( "RaceId", maybeVal Encode.int model.raceId )
        , ( "EthnicityId", maybeVal Encode.int model.ethnicityId )
        , ( "USVeteranId", maybeVal Encode.int model.uSVeteranId )
        , ( "ReligionId", maybeVal Encode.int model.religionId )
        , ( "DateOfBirth", maybeVal Encode.string model.sfData.dateOfBirth )
        , ( "DateOfDeath", maybeVal Encode.string model.sfData.dateOfDeath )
        , ( "VIP", maybeVal Encode.bool model.sfData.vip )
        , ( "PatientLanguagesMap", Encode.list (List.map encodePatientLanguagesMap model.patientLanguagesMap) )
        , ( "HouseholdMembers", Encode.list (List.map encodeHouseholdMembers model.householdMembers) )
        , ( "AcuityLevelId", maybeVal Encode.int model.acuityLevelId )
        , ( "PreferredPhoneNumber", Encode.int (encodePhone model.preferredPhoneNumber) )
        , ( "CellPhoneNumber", maybeVal Encode.int model.cellPhoneNumber )
        , ( "HomePhoneNumber", maybeVal Encode.int model.homePhoneNumber )
        , ( "WorkPhoneNumber", maybeVal Encode.int model.workPhoneNumber )
        ]


encodeContactInformationModel : Model -> Encode.Value
encodeContactInformationModel model =
    Encode.object
        [ ( "PatientAddresses", Encode.list (List.map encodePatientAddress model.patientAddresses) )
        , ( "FacilityId", maybeVal Encode.int model.facilityId )
        ]


encodePatientAddress : PatientAddress -> Encode.Value
encodePatientAddress address =
    Encode.object
        [ ( "Id", maybeVal Encode.int address.id )
        , ( "AddressLine1", maybeVal Encode.string address.addressLine1 )
        , ( "AddressLine2", maybeVal Encode.string address.addressLine2 )
        , ( "AddressLine3", maybeVal Encode.string address.addressLine3 )
        , ( "City", maybeVal Encode.string address.city )
        , ( "StateId", maybeVal Encode.int address.stateId )
        , ( "ZipCode", maybeVal Encode.string address.zipCode )
        , ( "IsPrimary", Encode.bool address.isPreferred )
        , ( "AddressType", maybeVal Encode.int address.addressType )
        , ( "MoveInDate", maybeVal Encode.string address.moveInDate )
        , ( "FacilityId", maybeVal Encode.int address.facilityAddressId )
        ]


type alias DemographicsResponse =
    { success : Bool
    , message : String
    , patientId : Maybe Int
    }


decodeDemographicsResponse : Decode.Decoder DemographicsResponse
decodeDemographicsResponse =
    Pipeline.decode DemographicsResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "message" Decode.string
        |> Pipeline.required "patientId" (Decode.maybe Decode.int)


load : Int -> Cmd Msg
load patientId =
    decodeServerResponse
        |> Http.get ("/People/GetDemographicsInformation?patientId=" ++ toString patientId)
        |> Http.send Load


save : Model -> Cmd Msg
save model =
    let
        timeZoneText =
            Functions.getDropdownItemById model.selectedTimeZoneId timeZoneDropdown

        encodeStr t =
            Encode.string (Encode.encode 0 t)
    in
    Functions.customPostRequest
        [ ( "demographicsInformation", encodeStr <| encodeDemographicsInformationModel model )
        , ( "contactInformation", encodeStr <| encodeContactInformationModel model )
        , ( "addresses", encodeStr <| Encode.list (List.map encodePatientAddress model.patientAddresses) )
        , ( "languages", encodeStr <| Encode.list (List.map encodePatientLanguagesMap model.patientLanguagesMap) )
        , ( "TZ", maybeVal Encode.string timeZoneText )
        , ( "WDStr", encodeStr <| Encode.list (List.map encodeDayData model.weekData) )
        ]
        "/people/SaveDemographics"
        (Http.expectJson decodeDemographicsResponse)
        |> Http.send SaveCompleted

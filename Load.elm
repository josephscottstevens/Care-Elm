module Load exposing (..)

import Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Table


decodeEnrollment : Json.Decode.Decoder Enrollment
decodeEnrollment =
    Json.Decode.Pipeline.decode Enrollment
        |> Json.Decode.Pipeline.required "ID" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "FacilityId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "ProviderId" (Json.Decode.maybe int)
        |> Json.Decode.Pipeline.required "FirstName" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "MiddleName" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "LastName" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "SexTypeId" (Json.Decode.maybe int)
        |> Json.Decode.Pipeline.required "DoB" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "SSN" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "MRN" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "PAN" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "Email" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "PrimaryPhone" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "PrimaryPhoneNumberTypeId" (Json.Decode.maybe int)
        |> Json.Decode.Pipeline.required "SecondaryPhone" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "SecondaryPhoneNumberTypeId" (Json.Decode.maybe int)
        |> Json.Decode.Pipeline.required "Address" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "Address2" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "Address3" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "City" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "StateId" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "Zip" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ProxyFirstName" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ProxyMiddleName" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ProxyLastName" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ProxyRelationshipTypeId" (Json.Decode.maybe int)
        |> Json.Decode.Pipeline.required "ProxyPhone" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ProxyPhoneNumberTypeId" (Json.Decode.maybe int)
        |> Json.Decode.Pipeline.required "Facility" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Provider" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "PrimaryInsurance" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "SecondaryInsurance" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "Status" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "AssignedTo" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ProxyName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "LastContactAttempt" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ContactAttempts" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "Comments" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ExistingComments" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ImportDate" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ConsentObtained" (Json.Decode.maybe string)
        |> Json.Decode.Pipeline.required "ElligibleICD10" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "ElligibleICD9" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "DisableCall" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "BatchId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "CanRegister" (Json.Decode.bool)



-- |> Json.Decode.Pipeline.required "selectedICDCodes" (Json.Decode.maybe string)
-- encodeEnrollment : Enrollment -> Json.Encode.Value
-- encodeEnrollment record =
--     Json.Encode.object
--         [ ( "iD", Json.Encode.int <| record.iD )
--         , ( "facilityId", Json.Encode.int <| record.facilityId )
--         , ( "providerId", Json.Encode.int <| record.providerId )
--         , ( "firstName", Json.Encode.maybe <| encodeString <| record.firstName )
--         , ( "middleName", Json.Encode.maybe <| encodeString <| record.middleName )
--         , ( "lastName", Json.Encode.maybe <| encodeString <| record.lastName )
--         , ( "sexTypeId", Json.Encode.maybe <| encodeString <| record.sexTypeId )
--         , ( "doB", Json.Encode.string <| record.doB )
--         , ( "sSN", Json.Encode.maybe <| encodeString <| record.sSN )
--         , ( "mRN", Json.Encode.maybe <| encodeString <| record.mRN )
--         , ( "pAN", Json.Encode.maybe <| encodeString <| record.pAN )
--         , ( "email", Json.Encode.maybe <| encodeString <| record.email )
--         , ( "primaryPhone", Json.Encode.string <| record.primaryPhone )
--         , ( "primaryPhoneNumberTypeId", Json.Encode.maybe <| encodeString <| record.primaryPhoneNumberTypeId )
--         , ( "secondaryPhone", Json.Encode.maybe <| encodeString <| record.secondaryPhone )
--         , ( "secondaryPhoneNumberTypeId", Json.Encode.maybe <| encodeString <| record.secondaryPhoneNumberTypeId )
--         , ( "address", Json.Encode.maybe <| encodeString <| record.address )
--         , ( "address2", Json.Encode.maybe <| encodeString <| record.address2 )
--         , ( "address3", Json.Encode.maybe <| encodeString <| record.address3 )
--         , ( "city", Json.Encode.maybe <| encodeString <| record.city )
--         , ( "stateId", Json.Encode.maybe <| encodeString <| record.stateId )
--         , ( "zip", Json.Encode.maybe <| encodeString <| record.zip )
--         , ( "proxyFirstName", Json.Encode.maybe <| encodeString <| record.proxyFirstName )
--         , ( "proxyMiddleName", Json.Encode.maybe <| encodeString <| record.proxyMiddleName )
--         , ( "proxyLastName", Json.Encode.maybe <| encodeString <| record.proxyLastName )
--         , ( "proxyRelationshipTypeId", Json.Encode.maybe <| encodeString <| record.proxyRelationshipTypeId )
--         , ( "proxyPhone", Json.Encode.maybe <| encodeString <| record.proxyPhone )
--         , ( "proxyPhoneNumberTypeId", Json.Encode.maybe <| encodeString <| record.proxyPhoneNumberTypeId )
--         , ( "facility", Json.Encode.string <| record.facility )
--         , ( "provider", Json.Encode.string <| record.provider )
--         , ( "name", Json.Encode.string <| record.name )
--         , ( "primaryInsurance", Json.Encode.maybe <| encodeString <| record.primaryInsurance )
--         , ( "secondaryInsurance", Json.Encode.maybe <| encodeString <| record.secondaryInsurance )
--         , ( "status", Json.Encode.string <| record.status )
--         , ( "assignedTo", Json.Encode.string <| record.assignedTo )
--         , ( "proxyName", Json.Encode.string <| record.proxyName )
--         , ( "lastContactAttempt", Json.Encode.maybe <| encodeString <| record.lastContactAttempt )
--         , ( "contactAttempts", Json.Encode.maybe <| encodeString <| record.contactAttempts )
--         , ( "comments", Json.Encode.maybe <| encodeString <| record.comments )
--         , ( "existingComments", Json.Encode.maybe <| encodeString <| record.existingComments )
--         , ( "importDate", Json.Encode.string <| record.importDate )
--         , ( "consentObtained", Json.Encode.maybe <| encodeString <| record.consentObtained )
--         , ( "elligibleICD10", Json.Encode.int <| record.elligibleICD10 )
--         , ( "elligibleICD9", Json.Encode.int <| record.elligibleICD9 )
--         , ( "disableCall", Json.Encode.bool <| record.disableCall )
--         , ( "batchId", Json.Encode.int <| record.batchId )
--         , ( "canRegister", Json.Encode.bool <| record.canRegister )
--         , ( "facilities", Json.Encode.maybe <| encodeString <| record.facilities )
--         , ( "providers", Json.Encode.maybe <| encodeString <| record.providers )
--         , ( "sexTypes", Json.Encode.maybe <| encodeString <| record.sexTypes )
--         , ( "relationshipTypes", Json.Encode.maybe <| encodeString <| record.relationshipTypes )
--         , ( "states", Json.Encode.maybe <| encodeString <| record.states )
--         , ( "phoneNumberTypes", Json.Encode.maybe <| encodeString <| record.phoneNumberTypes )
--         , ( "selectedICDCodes", Json.Encode.maybe <| encodeString <| record.selectedICDCodes )
--         ]


decodeModel : Decoder Model
decodeModel =
    decode Model
        |> hardcoded Initial
        |> required "list" (list decodeEnrollment)
        |> hardcoded (Table.initialSort "dob")
        |> hardcoded ""
        |> hardcoded 0


request : Http.Request Model
request =
    Http.get "/people/GetEmploymentInfo?showPending=true" decodeModel


getEmployment : Cmd Msg
getEmployment =
    Http.send Load request



-- Not good, rowId has to be patched on later, but I don't how to make it apart of the decoder


newEmployers : List Enrollment -> List Enrollment
newEmployers enrollment =
    enrollment |> List.indexedMap (\idx t -> { t | iD = idx })


updateEmployers : List Enrollment -> Enrollment -> List Enrollment
updateEmployers enrollment newEnrollment =
    enrollment
        |> List.map
            (\t ->
                if t.iD == newEnrollment.iD then
                    newEnrollment
                else
                    t
            )

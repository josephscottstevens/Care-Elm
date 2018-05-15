module Common.Dates exposing (..)

import Time exposing (Posix)


dateToString : Posix -> String
dateToString posix =
    let
        monthStr =
            String.fromInt (monthIndex posix + 1)

        dayStr =
            String.fromInt (Time.toDay Time.utc posix)

        yearStr =
            String.fromInt (Time.toYear Time.utc posix)
    in
    monthStr ++ "/" ++ dayStr ++ "/" ++ yearStr


defaultDateToString : Maybe Posix -> String
defaultDateToString posix =
    posix
        |> Maybe.map dateToString
        |> Maybe.withDefault ""


timeToString : Posix -> String
timeToString posix =
    let
        hour =
            Time.toHour Time.utc posix

        hourStr =
            String.fromInt <|
                if hour < 12 then
                    hour

                else
                    hour - 12

        minuteStr =
            String.fromInt <|
                (1 + Time.toMinute Time.utc posix)

        secondStr =
            String.fromInt <|
                Time.toSecond Time.utc posix

        amPmStr =
            if hour < 12 then
                "AM"

            else
                "PM"
    in
    hourStr ++ ":" ++ minuteStr ++ ":" ++ secondStr ++ " " ++ amPmStr


dateTimeToString : Posix -> String
dateTimeToString posix =
    dateToString posix ++ " " ++ timeToString posix


defaultDateTimeToString : Maybe Posix -> String
defaultDateTimeToString posix =
    posix
        |> Maybe.map dateTimeToString
        |> Maybe.withDefault ""


monthIndex : Posix -> Int
monthIndex posix =
    case Time.toMonth Time.utc posix of
        Time.Jan ->
            0

        Time.Feb ->
            1

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            11

module Time.Delta exposing (diff, calc, asString)

{-| This package includes helpers for generating human-readable time deltas.
-}

import Time as Time exposing (Time)
import String


{-| -}
day : Time
day =
    24 * Time.hour


{-| -}
week : Time
week =
    day * 7


{-| -}
month : Time
month =
    year / 12


{-| -}
year : Time
year =
    day * 365


{-| -}
inDays : Time -> Float
inDays t =
    t / day


{-| -}
inWeeks : Time -> Float
inWeeks t =
    t / week


{-| -}
inMonths : Time -> Float
inMonths t =
    t / month


{-| -}
inYears : Time -> Float
inYears t =
    t / year


{-| -}
type Interval
    = Year
    | Month
    | Week
    | Day
    | Hour
    | Minute
    | Second
    | Millisecond


{-| -}
diff : Time -> Time -> ( Int, Interval )
diff from to =
    calc (from - to)


{-| -}
calc : Time -> ( Int, Interval )
calc ms =
    let
        seconds =
            floor <| Time.inSeconds ms

        minutes =
            floor <| Time.inMinutes ms

        hours =
            floor <| Time.inHours ms

        days =
            floor <| inDays ms

        weeks =
            floor <| inWeeks ms

        months =
            floor <| inMonths ms

        years =
            floor <| inYears ms
    in
        if abs years > 0 then
            ( years, Year )
        else if abs months > 0 then
            ( months, Month )
        else if abs weeks > 0 then
            ( weeks, Week )
        else if abs days > 0 then
            ( days, Day )
        else if abs hours > 0 then
            ( hours, Hour )
        else if abs minutes > 0 then
            ( minutes, Minute )
        else if abs seconds > 0 then
            ( seconds, Second )
        else
            ( floor ms, Millisecond )


{-| -}
asString : Interval -> String
asString interval =
    case interval of
        Year ->
            "year"

        Month ->
            "month"

        Week ->
            "week"

        Day ->
            "day"

        Hour ->
            "hour"

        Minute ->
            "minute"

        Second ->
            "second"

        Millisecond ->
            "millisecond"



{-
   humanize : ( Int, Interval ) -> String
   humanize ( count, interval ) =
     let
       suffix =
         if count < 0 then
           "ago"
         else
           "from now"

       countStr =
         toString count

       intervalStr =
         interval |> asString |> pluralize count
     in
       String.join " " [ countStr, intervalStr, suffix ]
-}

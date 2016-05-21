module Model exposing (..)

import Json.Decode as D exposing (Decoder, (:=))
import Json.Decode.Extra exposing ((|:))
import Time exposing (Time, hour, minute)


type alias Model =
    { habits : List Habit
    , newHabit : Habit
    , now : Time
    }


emptyModel : Model
emptyModel =
    { habits = sampleHabits
    , newHabit = emptyHabit
    , now = 0
    }


type alias HabitId =
    Int


type alias Habit =
    { id : HabitId
    , desc : String
    , unit : Maybe String
    , target : Float
    , interval : Time
    , checkins : List Checkin
    , added : Time
    }


emptyHabit : Habit
emptyHabit =
    { id = 0
    , desc = ""
    , unit = Nothing
    , target = 0
    , interval = 0
    , checkins = []
    , added = 0
    }


sampleHabits : List Habit
sampleHabits =
    [ { id = 1
      , desc = "Floss"
      , unit = Nothing
      , target = 2
      , interval = hour * 24 * 7
      , checkins = []
      , added = 0
      }
    , { id = 2
      , desc = "Drink water"
      , unit = Just "cup"
      , target = 12
      , interval = hour * 24
      , checkins = []
      , added = 0
      }
    , { id = 3
      , desc = "Meditate"
      , unit = Nothing
      , target = 3
      , interval = hour * 24 * 7
      , checkins = []
      , added = 0
      }
    , { id = 4
      , desc = "Testing"
      , unit = Just "click"
      , target = 5
      , interval = minute
      , checkins = []
      , added = 0
      }
    , { id = 5
      , desc = "Smoke"
      , unit = Nothing
      , target = 0
      , interval = hour * 24
      , checkins = []
      , added = 0
      }
    ]


habitDecoder : Decoder Habit
habitDecoder =
    D.succeed Habit
        |: ("id" := D.int)
        |: ("desc" := D.string)
        |: ("unit" := D.maybe D.string)
        |: ("target" := D.float)
        |: ("interval" := D.float)
        |: ("checkins" := D.list checkinDecoder)
        |: ("added" := D.float)


checkin : Time -> Habit -> Habit
checkin time habit =
    let
        checkins =
            { time = time } :: habit.checkins
    in
        { habit | checkins = checkins }


relevantCheckins : Time -> Habit -> List Checkin
relevantCheckins now habit =
    habit.checkins
        |> List.filter (\c -> now - c.time < habit.interval)


pastCheckins : Time -> Habit -> List Checkin
pastCheckins now habit =
    habit.checkins
        |> List.filter (\c -> now - c.time >= habit.interval)


habitRate : Time -> Habit -> Float
habitRate now habit =
    habit
        |> relevantCheckins now
        |> List.length
        |> toFloat


type alias Checkin =
    { time : Time
    }


checkinDecoder : Decoder Checkin
checkinDecoder =
    D.succeed Checkin
        |: ("time" := D.float)

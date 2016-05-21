module Model exposing (..)

import Dict
import Json.Decode as D exposing ((:=))
import Json.Encode as E
import Time
import Style


-- MODEL


type alias Model =
    { habits : List Habit
    , settingsInputs : Dict.Dict String String
    , newHabitInputs : Dict.Dict String String
    , newHabit : Habit
    , now : Time.Time
    , expanded : Maybe Expandable
    , style : Style.Config
    }


empty : Model
empty =
    { habits = sampleHabits
    , settingsInputs = Dict.empty
    , newHabitInputs = Dict.empty
    , newHabit = emptyHabit
    , now = 0
    , expanded = Nothing
    , style = Style.defaultConfig
    }


decoder : D.Decoder Model
decoder =
    D.object7 Model
        ("habits" := D.list habitDecoder)
        ("settingsInputs" := D.dict D.string)
        ("newHabitInputs" := D.dict D.string)
        ("newHabit" := habitDecoder)
        ("now" := D.float)
        (D.maybe ("expanded" := expandableDecoder))
        ("style" := Style.configDecoder)


encode : Model -> E.Value
encode model =
    E.object
        [ ( "habits", E.list <| List.map encodeHabit model.habits )
        , ( "settingsInputs", encodeDict model.settingsInputs )
        , ( "newHabitInputs", encodeDict model.newHabitInputs )
        , ( "newHabit", encodeHabit model.newHabit )
        , ( "now", E.float model.now )
        , ( "expanded", encodeMaybe encodeExpandable model.expanded )
        , ( "style", Style.encodeConfig model.style )
        ]


encodeDict : Dict.Dict String String -> E.Value
encodeDict dict =
    E.object <| List.map (\( k, v ) -> ( k, E.string v )) <| Dict.toList dict



-- EXPANDABLE


type Expandable
    = Settings
    | NewHabit


expandableFromString : String -> Result String Expandable
expandableFromString str =
    case str of
        "Settings" ->
            Ok Settings

        "NewHabit" ->
            Ok NewHabit

        _ ->
            Err <| "'" ++ str ++ "' is not an expandable type"


expandableDecoder : D.Decoder Expandable
expandableDecoder =
    D.customDecoder D.string expandableFromString


encodeExpandable : Expandable -> E.Value
encodeExpandable expandable =
    E.string <| toString expandable



-- HABIT


type alias Habit =
    { id : Int
    , desc : String
    , unit : String
    , target : Float
    , interval : Time.Time
    , checkins : List Checkin
    , added : Time.Time
    }


emptyHabit : Habit
emptyHabit =
    { id = 0
    , desc = ""
    , unit = "session"
    , target = 0
    , interval = 0
    , checkins = []
    , added = 0
    }


habitDecoder : D.Decoder Habit
habitDecoder =
    D.object7 Habit
        ("id" := habitIdDecoder)
        ("desc" := D.string)
        ("unit" := D.string)
        ("target" := D.float)
        ("interval" := D.float)
        ("checkins" := D.list checkinDecoder)
        ("added" := D.float)


encodeHabit : Habit -> E.Value
encodeHabit habit =
    E.object
        [ ( "id", encodeHabitId habit.id )
        , ( "desc", E.string habit.desc )
        , ( "unit", E.string habit.unit )
        , ( "target", E.float habit.target )
        , ( "interval", E.float habit.interval )
        , ( "checkins", E.list <| List.map encodeCheckin habit.checkins )
        , ( "added", E.float habit.added )
        ]



-- HABIT ID


type alias HabitId =
    Int


habitIdDecoder : D.Decoder HabitId
habitIdDecoder =
    D.int


encodeHabitId : HabitId -> E.Value
encodeHabitId habitId =
    E.int habitId



-- HABIT HELPERS


sampleHabits : List Habit
sampleHabits =
    [ { id = 1
      , desc = "Floss"
      , unit = "session"
      , target = 2
      , interval = Time.hour * 24 * 7
      , checkins = []
      , added = 0
      }
    , { id = 2
      , desc = "Drink water"
      , unit = "cup"
      , target = 12
      , interval = Time.hour * 24
      , checkins = []
      , added = 0
      }
    , { id = 3
      , desc = "Meditate"
      , unit = "session"
      , target = 3
      , interval = Time.hour * 24 * 7
      , checkins = []
      , added = 0
      }
    , { id = 4
      , desc = "Testing"
      , unit = "click"
      , target = 5
      , interval = Time.minute
      , checkins = []
      , added = 0
      }
    , { id = 5
      , desc = "Smoke"
      , unit = "session"
      , target = 0
      , interval = Time.hour * 24
      , checkins = []
      , added = 0
      }
    ]



-- CHECKIN


type alias Checkin =
    { time : Time.Time
    }


checkinDecoder : D.Decoder Checkin
checkinDecoder =
    D.object1 Checkin ("time" := D.float)


encodeCheckin : Checkin -> E.Value
encodeCheckin checkin =
    E.object [ ( "time", E.float checkin.time ) ]



-- HELPERS


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe toValue maybe =
    case maybe of
        Just x ->
            toValue x

        Nothing ->
            E.null


checkin : Time.Time -> Habit -> Habit
checkin time habit =
    let
        checkins =
            { time = time } :: habit.checkins
    in
        { habit | checkins = checkins }


relevantCheckins : Time.Time -> Habit -> List Checkin
relevantCheckins now habit =
    habit.checkins
        |> List.filter (\c -> now - c.time < habit.interval)


pastCheckins : Time.Time -> Habit -> List Checkin
pastCheckins now habit =
    habit.checkins
        |> List.filter (\c -> now - c.time >= habit.interval)


habitRate : Time.Time -> Habit -> Float
habitRate now habit =
    habit
        |> relevantCheckins now
        |> List.length
        |> toFloat

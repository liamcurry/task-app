module Model.Habit
    exposing
        ( Habit
        , empty
        , decoder
        , encode
        , Id
        , idDecoder
        , encodeId
        , Checkin
        , checkinDecoder
        , encodeCheckin
        , checkin
        , relevantCheckins
        , pastCheckins
        , rate
        )

import Json.Decode as D exposing ((:=))
import Json.Encode as E
import Time


type alias Habit =
    { id : Int
    , desc : String
    , unit : String
    , target : Float
    , interval : Time.Time
    , checkins : List Checkin
    , added : Time.Time
    }


empty : Habit
empty =
    { id = 0
    , desc = ""
    , unit = "session"
    , target = 0
    , interval = 0
    , checkins = []
    , added = 0
    }


decoder : D.Decoder Habit
decoder =
    D.object7 Habit
        ("id" := idDecoder)
        ("desc" := D.string)
        ("unit" := D.string)
        ("target" := D.float)
        ("interval" := D.float)
        ("checkins" := D.list checkinDecoder)
        ("added" := D.float)


encode : Habit -> E.Value
encode habit =
    E.object
        [ ( "id", encodeId habit.id )
        , ( "desc", E.string habit.desc )
        , ( "unit", E.string habit.unit )
        , ( "target", E.float habit.target )
        , ( "interval", E.float habit.interval )
        , ( "checkins", E.list <| List.map encodeCheckin habit.checkins )
        , ( "added", E.float habit.added )
        ]



-- ID


type alias Id =
    Int


idDecoder : D.Decoder Id
idDecoder =
    D.int


encodeId : Id -> E.Value
encodeId habitId =
    E.int habitId



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


rate : Time.Time -> Habit -> Float
rate now habit =
    habit
        |> relevantCheckins now
        |> List.length
        |> toFloat

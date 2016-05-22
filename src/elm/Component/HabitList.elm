module Component.HabitList exposing (..)

-- LIB

import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D exposing ((:=))
import Json.Encode as E
import Time
import Time.Delta as Delta


-- INTERNAL

import Model.Habit as Habit
import Style as S


type alias Model =
    { habits : List Habit.Habit }


empty : Model
empty =
    { habits = sampleHabits }


decoder : D.Decoder Model
decoder =
    D.object1 Model
        ("habits" := D.list Habit.decoder)


encode : Model -> E.Value
encode model =
    E.object [ ( "habits", E.list <| List.map Habit.encode model.habits ) ]


sampleHabits : List Habit.Habit
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
    ]



-- STATE


type Msg
    = SaveHabit Habit.Habit
    | Checkin Habit.Id
    | ResetCheckins


update : Time.Time -> Msg -> Model -> ( Model, Cmd Msg )
update now msg model =
    case msg of
        SaveHabit habit ->
            { model
                | habits = model.habits ++ [ habit ]
            }
                ! []

        Checkin id ->
            let
                addCheckin habit =
                    if habit.id == id then
                        Habit.checkin now habit
                    else
                        habit
            in
                { model | habits = List.map addCheckin model.habits } ! []

        ResetCheckins ->
            let
                reset habit =
                    { habit | checkins = Habit.pastCheckins now habit }
            in
                { model | habits = List.map reset model.habits } ! []



-- VIEWS


view : Time.Time -> S.Config -> Model -> H.Html Msg
view now style model =
    H.div [ S.class S.HabitList ]
        (model.habits
            |> List.map (\habit -> ( habit, Habit.rate now habit ))
            |> List.map (habitView style model)
        )


habitView : S.Config -> Model -> ( Habit.Habit, Float ) -> H.Html Msg
habitView style model ( habit, rate ) =
    let
        ( count, interval ) =
            Delta.calc habit.interval

        color =
            habit.target
                |> S.rateColor style.colorGood style.colorBad rate
                |> S.colorStyle
    in
        H.div
            [ S.classList
                [ ( S.Habit, True )
                , ( S.HabitHitTarget, habit.target == rate )
                ]
            , E.onClick (Checkin habit.id)
            , A.style [ ( "backgroundColor", color ) ]
            ]
            [ H.strong [ S.class S.HabitDesc ]
                [ H.text habit.desc ]
            , H.span [ S.class S.HabitRates ]
                [ H.span [ S.class S.HabitRateCounts ]
                    [ H.strong [ S.class S.HabitRateCurrent ]
                        [ H.text <| toString rate ]
                    , H.span [ S.class S.HabitRateSep ]
                        [ H.text " / " ]
                    , H.span [ S.class S.HabitRateTarget ]
                        [ H.text <| toString habit.target ]
                    ]
                , H.small [ S.class S.HabitInterval ]
                    [ H.text habit.unit
                    , H.text " per "
                    , H.text <| Delta.asString interval
                    ]
                ]
            ]

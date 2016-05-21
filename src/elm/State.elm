module State
    exposing
        ( init
        , update
        , subscriptions
        , Msg(..)
        , NewHabitMsg(..)
        , SettingsMsg(..)
        )

import AnimationFrame
import Dict
import Json.Decode as D
import Model exposing (Model)
import Time
import String
import Style


-- TEA BASE


type Msg
    = NoOp
    | Tick Time.Time
    | AddHabit
    | HandleNewHabitMsg NewHabitMsg
    | HandleSettingsMsg SettingsMsg
    | CheckIn Model.HabitId
    | ToggleExpanded Model.Expandable
    | ResetCheckins


init : D.Value -> ( Model, Cmd Msg )
init modelValue =
    let
        model =
            (D.decodeValue Model.decoder modelValue)
                |> Result.toMaybe
                |> Maybe.withDefault Model.empty
    in
        model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            model ! []

        Tick time ->
            { model | now = time } ! []

        AddHabit ->
            { model
                | habits = model.habits ++ [ model.newHabit ]
                , newHabit = Model.emptyHabit
            }
                ! []

        HandleNewHabitMsg newHabitMsg ->
            newHabitUpdate newHabitMsg model

        HandleSettingsMsg settingsMsg ->
            settingsUpdate settingsMsg model

        CheckIn id ->
            let
                addCheckin habit =
                    if habit.id == id then
                        Model.checkin model.now habit
                    else
                        habit
            in
                { model | habits = List.map addCheckin model.habits } ! []

        ToggleExpanded expandable ->
            if model.expanded == Just expandable then
                { model | expanded = Nothing } ! []
            else
                { model | expanded = Just expandable } ! []

        ResetCheckins ->
            let
                reset habit =
                    { habit | checkins = Model.pastCheckins model.now habit }
            in
                { model | habits = List.map reset model.habits } ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    AnimationFrame.times Tick



-- NEW HABIT


type NewHabitMsg
    = SetName String
    | SetUnit String
    | SetTarget String
    | SetInterval String


withFallback : (String -> Result String a) -> a -> String -> a
withFallback toWhatever fallback numStr =
    numStr
        |> toWhatever
        |> Result.toMaybe
        |> Maybe.withDefault fallback


newHabitUpdate : NewHabitMsg -> Model -> ( Model, Cmd Msg )
newHabitUpdate newHabitMsg model =
    let
        newHabit =
            model.newHabit

        setNewHabit newHabit =
            { model | newHabit = newHabit } ! []
    in
        case newHabitMsg of
            SetName name ->
                setNewHabit { newHabit | desc = name }

            SetUnit unit ->
                setNewHabit { newHabit | unit = unit }

            SetTarget targetStr ->
                let
                    target =
                        withFallback String.toFloat model.newHabit.target targetStr
                in
                    setNewHabit { newHabit | target = target }

            SetInterval intervalStr ->
                let
                    interval =
                        withFallback String.toFloat model.newHabit.interval intervalStr
                in
                    setNewHabit { newHabit | interval = interval }



-- SETTINGS


type SettingsMsg
    = SetColorGood String
    | SetColorBad String
    | SetColorText String
    | SetColorBorder String
    | SetSpacing String
    | SetAnimationMs String
    | SetBaseHeight String
    | ResetStyle


settingsUpdate : SettingsMsg -> Model -> ( Model, Cmd Msg )
settingsUpdate settingsMsg model =
    let
        oldStyle =
            model.style

        setInput field value newStyle =
            { model
                | style = newStyle
                , settingsInputs = Dict.insert field value model.settingsInputs
            }
                ! []
    in
        case settingsMsg of
            SetColorGood str ->
                let
                    colorGood =
                        withFallback Style.hexToColor model.style.colorGood str
                in
                    setInput "colorGood" str { oldStyle | colorGood = colorGood }

            SetColorBad str ->
                let
                    colorBad =
                        withFallback Style.hexToColor model.style.colorBad str
                in
                    setInput "colorBad" str { oldStyle | colorBad = colorBad }

            SetColorText str ->
                let
                    colorText =
                        withFallback Style.hexToColor model.style.colorText str
                in
                    setInput "colorText" str { oldStyle | colorText = colorText }

            SetColorBorder str ->
                let
                    colorBorder =
                        withFallback Style.hexToColor model.style.colorBorder str
                in
                    setInput "colorBorder" str { oldStyle | colorBorder = colorBorder }

            SetSpacing str ->
                let
                    spacing =
                        withFallback String.toInt model.style.spacing str
                in
                    setInput "spacing" str { oldStyle | spacing = spacing }

            SetAnimationMs str ->
                let
                    animationMs =
                        withFallback String.toInt model.style.animationMs str
                in
                    setInput "animationMs" str { oldStyle | animationMs = animationMs }

            SetBaseHeight str ->
                let
                    baseHeight =
                        withFallback String.toInt model.style.baseHeight str
                in
                    setInput "baseHeight" str { oldStyle | baseHeight = baseHeight }

            ResetStyle ->
                { model
                    | style = Style.defaultConfig
                    , settingsInputs = Dict.empty
                }
                    ! []

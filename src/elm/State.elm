module State
    exposing
        ( Msg(..)
        , init
        , update
        , subscriptions
        )

-- LIB

import AnimationFrame
import Json.Decode as D
import Time


-- INTERNAL

import Component.HabitForm as HabitForm
import Component.HabitList as HabitList
import Component.Settings as Settings
import Model


-- TEA BASE


type Msg
    = NoOp
    | Tick Time.Time
    | HandleHabitFormMsg HabitForm.Msg
    | HandleHabitListMsg HabitList.Msg
    | HandleSettingsMsg Settings.Msg
    | ToggleExpanded Model.Expandable


init : D.Value -> ( Model.Model, Cmd Msg )
init modelValue =
    let
        model =
            (D.decodeValue Model.decoder modelValue)
                |> Result.toMaybe
                |> Maybe.withDefault Model.empty
    in
        model ! []


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            model ! []

        Tick time ->
            { model | now = time } ! []

        HandleHabitListMsg habitListMsg ->
            let
                ( habitList, habitListCmd ) =
                    HabitList.update model.now habitListMsg model.habitList
            in
                { model | habitList = habitList }
                    ! [ Cmd.map HandleHabitListMsg habitListCmd ]

        HandleHabitFormMsg habitFormMsg ->
            let
                ( habitForm, habitFormCmd ) =
                    HabitForm.update habitFormMsg model.habitForm

                oldHabitList =
                    model.habitList

                habitList =
                    case habitFormMsg of
                        HabitForm.Save ->
                            { oldHabitList
                                | habits = oldHabitList.habits ++ [ model.habitForm.habit ]
                            }

                        _ ->
                            oldHabitList
            in
                { model | habitForm = habitForm, habitList = habitList }
                    ! [ Cmd.map HandleHabitFormMsg habitFormCmd ]

        HandleSettingsMsg settingsMsg ->
            let
                ( settings, settingsCmd ) =
                    Settings.update settingsMsg model.settings
            in
                { model | settings = settings }
                    ! [ Cmd.map HandleSettingsMsg settingsCmd ]

        ToggleExpanded expandable ->
            if model.expanded == Just expandable then
                { model | expanded = Nothing } ! []
            else
                { model | expanded = Just expandable } ! []


subscriptions : Model.Model -> Sub Msg
subscriptions _ =
    AnimationFrame.times Tick

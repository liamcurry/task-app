module View exposing (root)

-- LIBS

import Html as H
import Html.App as App
import Html.Attributes as A
import Html.Events as E


-- INTERNAL

import Component.HabitForm as HabitForm
import Component.HabitList as HabitList
import Component.Settings as Settings
import Model
import Model.Habit as Habit
import State as Msg
import Style as S


-- ROOT


root : Model.Model -> H.Html Msg.Msg
root model =
    let
        habits =
            model.habitList.habits

        style =
            model.settings.style

        totalCurrentRate : Float
        totalCurrentRate =
            habits
                |> List.map (\habit -> ( habit, Habit.rate model.now habit ))
                |> List.map snd
                |> List.sum

        totalTargetRate : Float
        totalTargetRate =
            habits
                |> List.map .target
                |> List.sum

        color : String
        color =
            totalTargetRate
                |> S.rateColor style.colorGood style.colorBad totalCurrentRate
                |> S.colorStyle
    in
        H.div
            [ S.classList
                [ ( S.AppContainer, True )
                , ( S.HitAllTargets, totalCurrentRate == totalTargetRate )
                ]
            , A.style [ ( "backgroundColor", color ) ]
            ]
            [ S.inline <| S.stylesWith style
            , H.header [ S.class S.AppHeader ]
                [ H.button
                    [ S.class S.Button
                    , E.onClick (Msg.ToggleExpanded Model.Settings)
                    ]
                    [ H.text "🛠" ]
                , H.h1 [ S.class S.AppTitle ]
                    [ H.text "Habits" ]
                , H.button
                    [ S.class S.Button
                    , E.onClick (Msg.ToggleExpanded Model.HabitForm)
                    ]
                    [ H.text "🞡" ]
                ]
            , if model.expanded == Just Model.Settings then
                App.map Msg.HandleSettingsMsg (Settings.view model.settings)
              else if model.expanded == Just Model.HabitForm then
                App.map Msg.HandleHabitFormMsg (HabitForm.view model.habitForm)
              else
                H.text ""
            , App.map Msg.HandleHabitListMsg (HabitList.view model.now style model.habitList)
            ]

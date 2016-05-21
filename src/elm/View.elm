module View exposing (root)

-- LIBS

import Css
import Color exposing (Color)
import Color.Mixing exposing (mix)
import Dict
import Html as H exposing (..)
import Html.App as App
import Html.Attributes as A exposing (style, href, placeholder, value)
import Html.Events as E exposing (onClick)
import String


-- INTERNAL

import Model exposing (Model, Habit, habitRate)
import State as Msg exposing (Msg(..))
import Style exposing (Class(..), class, classList, defaultConfig)
import Time.Delta as Delta


-- ROOT


root : Model -> Html Msg
root model =
    let
        totalCurrentRate : Float
        totalCurrentRate =
            model.habits
                |> List.map (\habit -> ( habit, habitRate model.now habit ))
                |> List.map snd
                |> List.sum

        totalTargetRate : Float
        totalTargetRate =
            model.habits
                |> List.map .target
                |> List.sum

        color : String
        color =
            totalTargetRate
                |> rateColor model.style.colorGood model.style.colorBad totalCurrentRate
                |> colorStyle
    in
        div
            [ classList
                [ ( AppContainer, True )
                , ( HitAllTargets, totalCurrentRate == totalTargetRate )
                ]
            , style [ ( "backgroundColor", color ) ]
            ]
            [ Style.inline <| Style.stylesWith model.style
            , header [ class AppHeader ]
                [ button [ class Button, onClick (ToggleExpanded Model.Settings) ]
                    [ text "🛠" ]
                , h1 [ class AppTitle ]
                    [ text "Habits" ]
                , button [ class Button, onClick (ToggleExpanded Model.NewHabit) ]
                    [ text "🞡" ]
                ]
            , if model.expanded == Just Model.Settings then
                settingsView model
              else if model.expanded == Just Model.NewHabit then
                newHabitView model
              else
                text ""
            , habitListView model
            ]



-- HABIT LIST


habitListView : Model -> Html Msg
habitListView model =
    div [ class HabitList ]
        (model.habits
            |> List.map (\habit -> ( habit, habitRate model.now habit ))
            |> List.map (habit model)
        )


habit : Model -> ( Habit, Float ) -> Html Msg
habit model ( habit, rate ) =
    let
        ( count, interval ) =
            Delta.calc habit.interval

        color =
            habit.target
                |> rateColor model.style.colorGood model.style.colorBad rate
                |> colorStyle
    in
        div
            [ classList
                [ ( Style.Habit, True )
                , ( HabitHitTarget, habit.target == rate )
                ]
            , onClick (CheckIn habit.id)
            , style [ ( "backgroundColor", color ) ]
            ]
            [ strong [ class HabitDesc ]
                [ text habit.desc ]
            , span [ class HabitRates ]
                [ span [ class HabitRateCounts ]
                    [ strong [ class HabitRateCurrent ]
                        [ text <| toString rate ]
                    , span [ class HabitRateSep ]
                        [ text " / " ]
                    , span [ class HabitRateTarget ]
                        [ text <| toString habit.target ]
                    ]
                , small [ class HabitInterval ]
                    [ text habit.unit
                    , text " per "
                    , text <| Delta.asString interval
                    ]
                ]
            ]



-- NEW HABIT


newHabitView : Model -> Html Msg.Msg
newHabitView model =
    div [ class NewHabit ]
        [ form [ E.onSubmit Msg.AddHabit ]
            [ App.map HandleNewHabitMsg <| newHabitFieldsView model ]
        ]


newHabitFieldsView : Model -> Html Msg.NewHabitMsg
newHabitFieldsView model =
    div []
        [ input
            [ placeholder "Name"
            , value model.newHabit.desc
            , E.onInput Msg.SetName
            ]
            []
        , input
            [ placeholder "Unit"
            , value model.newHabit.unit
            , E.onInput Msg.SetUnit
            ]
            []
        , input
            [ placeholder "Target"
            , value <| toString model.newHabit.target
            , E.onInput Msg.SetTarget
            ]
            []
        , input
            [ placeholder "Interval"
            , value <| toString model.newHabit.interval
            , E.onInput Msg.SetInterval
            ]
            []
        , H.button [ A.type' "submit" ] [ text "Add" ]
        ]



-- SETTINGS


settingsView : Model -> Html Msg
settingsView model =
    H.div [ class Settings ]
        [ App.map HandleSettingsMsg <| settingsFieldsView model
        , H.button [ E.onClick Msg.ResetCheckins ] [ H.text "Reset Checkins" ]
        ]


settingsFieldsView : Model -> Html Msg.SettingsMsg
settingsFieldsView model =
    let
        --input : String -> String -> (a -> String) -> (Style.Config -> a) -> String -> m -> Html m
        input type' toStr label getVal fieldName msg =
            H.label []
                [ H.strong [] [ H.text label ]
                , H.input
                    [ A.placeholder label
                    , A.type' type'
                    , A.value
                        <| Maybe.withDefault (toStr <| getVal model.style)
                            (Dict.get fieldName model.settingsInputs)
                    , E.onInput msg
                    ]
                    []
                ]

        colorInput =
            input "color" Style.colorToHex

        intInput =
            input "number" toString
    in
        H.fieldset []
            [ colorInput "Good" .colorGood "colorGood" Msg.SetColorGood
            , colorInput "Bad" .colorBad "colorBad" Msg.SetColorBad
            , colorInput "Text" .colorText "colorText" Msg.SetColorText
            , colorInput "Border" .colorBorder "colorBorder" Msg.SetColorBorder
            , intInput "Spacing" .spacing "spacing" Msg.SetSpacing
            , intInput "Speed" .animationMs "animationMs" Msg.SetAnimationMs
            , H.button [ E.onClick Msg.ResetStyle ] [ H.text "Reset Styles" ]
            ]



-- COLOR HELPERS


toColor : Css.Color -> Color
toColor color =
    Color.rgba color.red color.green color.blue color.alpha


rateColor : Css.Color -> Css.Color -> Float -> Float -> Color
rateColor goodColor badColor rate target =
    let
        diff =
            abs (target - rate)

        percent =
            if target == 0 && diff > 0 then
                1.0
            else
                min 1 (diff / target)
    in
        mix percent (badColor |> toColor) (goodColor |> toColor)


colorStyle : Color -> String
colorStyle color =
    color
        |> Color.toRgb
        |> (\rgba -> [ toString rgba.red, toString rgba.green, toString rgba.blue, toString rgba.alpha ])
        |> String.join ","
        |> (\rgba -> "rgba(" ++ rgba ++ ")")

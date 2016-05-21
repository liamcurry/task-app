module View exposing (root)

-- LIBS

import Css
import Color exposing (Color)
import Color.Mixing exposing (mix)
import Html exposing (..)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)
import String
import Time.Delta as Delta


-- INTERNAL

import Model exposing (Model, Habit, habitRate)
import State exposing (Msg(..))
import Style exposing (Class(..), class, classList, defaultConfig)


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


habit : Model -> ( Habit, Float ) -> Html Msg
habit model ( habit, rate ) =
    let
        ( count, interval ) =
            Delta.calc habit.interval

        color =
            habit.target
                |> rateColor defaultConfig.colorGood defaultConfig.colorBad rate
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
                        [ rate |> toString |> text ]
                    , span [ class HabitRateSep ]
                        [ text " / " ]
                    , span [ class HabitRateTarget ]
                        [ habit.target |> toString |> text ]
                    ]
                , small [ class HabitInterval ]
                    [ habit.unit |> Maybe.withDefault "sessions" |> text
                    , text " per "
                    , interval |> Delta.asString |> text
                    ]
                ]
            ]


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
                |> rateColor defaultConfig.colorGood defaultConfig.colorBad totalCurrentRate
                |> colorStyle
    in
        div
            [ classList
                [ ( AppContainer, True )
                , ( HitAllTargets, totalCurrentRate == totalTargetRate )
                ]
            , style [ ( "backgroundColor", color ) ]
            ]
            [ Style.defaultConfig
                |> Style.stylesWith
                |> Style.inline
            , header [ class AppHeader ]
                [ button [ class Button, onClick Reset ] [ text "🛠" ]
                , h1 [ class AppTitle ] [ text "Habits" ]
                , button [ class Button ] [ text "🞡" ]
                ]
            , habitListView model
            ]


habitListView : Model -> Html Msg
habitListView model =
    div [ class HabitList ]
        (model.habits
            |> List.map (\habit -> ( habit, habitRate model.now habit ))
            |> List.map (habit model)
        )

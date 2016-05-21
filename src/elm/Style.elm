module Style
    exposing
        ( Class(..)
        , styles
        , stylesWith
        , Config
        , defaultConfig
        , inline
        , class
        , classList
        )

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Helpers exposing (toCssIdentifier, identifierToString)
import Css.Namespace exposing (namespace)
import Html exposing (Html, Attribute, node, text)
import Html.Attributes as Attr
import String


type Class
    = AppContainer
    | HitAllTargets
    | AppHeader
    | AppTitle
    | Button
    | HabitList
    | Habit
    | HabitHitTarget
    | HabitDesc
    | HabitRates
    | HabitRateCounts
    | HabitRateCurrent
    | HabitRateSep
    | HabitRateTarget
    | HabitInterval


type alias Config =
    { fontFamilies : List String
    , colorGood : Color
    , colorBad : Color
    , colorText : Color
    , colorBorder : Color
    , spacing : Int
    , animationMs : Int
    , baseHeight : Int
    }


defaultConfig : Config
defaultConfig =
    { fontFamilies = [ "Muli", "Helvetica", "sans-serif" ]
    , colorGood = rgb 21 101 192
    , colorBad = rgb 198 40 40
    , colorText = rgb 255 255 255
    , colorBorder = (rgba 255 255 255 0.2)
    , spacing = 20
    , animationMs = 500
    , baseHeight = 60
    }


bgTransition config =
    property "transition" ((config.animationMs |> toString) ++ "ms background-color")


styles : List Snippet
styles =
    stylesWith defaultConfig


stylesWith : Config -> List Snippet
stylesWith config =
    [ everything
        [ boxSizing borderBox
        , margin zero
        , padding zero
        ]
    , html [ height (pct 100) ]
    , body
        [ color config.colorText
        , displayFlex
        , flexDirection column
        , fontFamilies config.fontFamilies
        , bgTransition config
        , height (pct 100)
        , children
            [ div
                [ displayFlex
                , flex (int 1)
                , flexDirection column
                ]
            ]
        ]
    , (.) AppHeader
        [ alignItems center
        , displayFlex
        , property "justify-content" "space-between"
        , height (config.baseHeight |> toFloat |> px)
        , lineHeight (config.baseHeight |> toFloat |> px)
        ]
    , (.) AppTitle
        [ border zero
        , borderColor config.colorBorder
        , borderStyle solid
        , borderLeftWidth (px 1)
        , borderRightWidth (px 1)
        , flex (int 1)
        , textAlign center
        ]
    , (.) Button
        [ backgroundColor transparent
        , border zero
        , color config.colorText
        , display block
        , fontSize (pct 200)
        , fontWeight bold
        , height (pct 100)
        , padding2 zero (config.spacing |> toFloat |> px)
        ]
    , (.) Habit
        [ alignItems center
        , borderBottom3 (px 1) solid config.colorBorder
        , displayFlex
        , padding2 (config.spacing |> toFloat |> (*) 0.5 |> px)
            (config.spacing |> toFloat |> px)
        , bgTransition config
        , firstOfType [ borderTop3 (px 1) solid config.colorBorder ]
        ]
    , (.) HabitDesc
        [ flex (int 1) ]
    , (.) HabitRates
        [ displayFlex
        , flexDirection column
        , textAlign right
        ]
    , (.) HabitInterval
        [ fontSize (pct 60)
        , opacity (float 0.7)
        , textTransform uppercase
        ]
    ]


inline : List Snippet -> Html a
inline snippets =
    node "style"
        []
        [ snippets
            |> namespace ""
            |> stylesheet
            |> Css.compile
            |> .css
            |> text
        ]


class : name -> Attribute msg
class name =
    name
        |> identifierToString ""
        |> Attr.class


classList : List ( class, Bool ) -> Attribute msg
classList list =
    list
        |> List.filter snd
        |> List.map fst
        |> List.map (identifierToString "")
        |> String.join " "
        |> Attr.class

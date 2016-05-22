module Style
    exposing
        ( Class(..)
        , styles
        , stylesWith
        , Config
        , defaultConfig
        , decoder
        , encode
        , inline
        , class
        , classList
        , colorToHex
        , hexToColor
        , toColor
        , rateColor
        , colorStyle
        )

import Color
import Color.Convert as Convert
import Color.Mixing as Mixing
import Css exposing (..)
import Css.Elements exposing (..)
import Css.Helpers exposing (toCssIdentifier, identifierToString)
import Css.Namespace exposing (namespace)
import Json.Decode as D exposing ((:=))
import Json.Encode as E
import Html exposing (Html, Attribute, node, text)
import Html.Attributes as Attr
import String


-- CONFIG


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
    , colorBorder = rgb 255 255 255
    , spacing = 20
    , animationMs = 500
    , baseHeight = 60
    }


decoder : D.Decoder Config
decoder =
    D.object8 Config
        ("fontFamilies" := D.list D.string)
        ("colorGood" := colorDecoder)
        ("colorBad" := colorDecoder)
        ("colorText" := colorDecoder)
        ("colorBorder" := colorDecoder)
        ("spacing" := D.int)
        ("animationMs" := D.int)
        ("baseHeight" := D.int)


encode : Config -> E.Value
encode config =
    E.object
        [ ( "fontFamilies", E.list <| List.map E.string config.fontFamilies )
        , ( "colorGood", encodeColor config.colorGood )
        , ( "colorBad", encodeColor config.colorBad )
        , ( "colorText", encodeColor config.colorText )
        , ( "colorBorder", encodeColor config.colorBorder )
        , ( "spacing", E.int config.spacing )
        , ( "animationMs", E.int config.animationMs )
        , ( "baseHeight", E.int config.baseHeight )
        ]


toCssColor : Color.Color -> Css.Color
toCssColor color =
    Color.toRgb color
        |> (\c -> rgb c.red c.green c.blue)


hexToColor : String -> Result String Css.Color
hexToColor hexStr =
    (Convert.hexToColor hexStr)
        |> Result.fromMaybe ("error parsing hex code: " ++ hexStr)
        |> (flip Result.andThen) (\color -> Ok <| toCssColor color)


colorToHex : Css.Color -> String
colorToHex color =
    Convert.colorToHex <| Color.rgb color.red color.green color.blue


colorDecoder : D.Decoder Color
colorDecoder =
    D.customDecoder D.string hexToColor


encodeColor : Color -> E.Value
encodeColor color =
    E.string <| Convert.colorToHex <| Color.rgb color.red color.green color.blue



-- CLASSES


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
    | HabitForm
    | Settings


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
        , property "cursor" "pointer"
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
        , property "cursor" "pointer"
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



-- HELPERS


toColor : Css.Color -> Color.Color
toColor color =
    Color.rgba color.red color.green color.blue color.alpha


rateColor : Css.Color -> Css.Color -> Float -> Float -> Color.Color
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
        Mixing.mix percent (badColor |> toColor) (goodColor |> toColor)


colorStyle : Color.Color -> String
colorStyle color =
    color
        |> Color.toRgb
        |> (\rgba -> [ toString rgba.red, toString rgba.green, toString rgba.blue, toString rgba.alpha ])
        |> String.join ","
        |> (\rgba -> "rgba(" ++ rgba ++ ")")

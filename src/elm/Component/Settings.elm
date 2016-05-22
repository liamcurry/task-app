module Component.Settings exposing (..)

-- LIB

import Dict
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D exposing ((:=))
import Json.Encode as E


-- INTERNAL

import Model.Inputs as Inputs
import Style as S


-- MODEL


type alias Model =
    { inputs : Inputs.Inputs
    , style : S.Config
    }


empty : Model
empty =
    { inputs = Inputs.empty
    , style = S.defaultConfig
    }


decoder : D.Decoder Model
decoder =
    D.object2 Model
        ("inputs" := Inputs.decoder)
        ("style" := S.decoder)


encode : Model -> E.Value
encode model =
    E.object
        [ ( "inputs", Inputs.encode model.inputs )
        , ( "style", S.encode model.style )
        ]



-- STATE


type Msg
    = SetColorGood String
    | SetColorBad String
    | SetColorText String
    | SetColorBorder String
    | SetSpacing String
    | SetAnimationMs String
    | SetBaseHeight String
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldStyle =
            model.style

        setInput field value newStyle =
            { model
                | style = newStyle
                , inputs = Dict.insert field value model.inputs
            }
                ! []
    in
        case msg of
            SetColorGood str ->
                let
                    colorGood =
                        Inputs.color model.style.colorGood str
                in
                    setInput "colorGood" str { oldStyle | colorGood = colorGood }

            SetColorBad str ->
                let
                    colorBad =
                        Inputs.color model.style.colorBad str
                in
                    setInput "colorBad" str { oldStyle | colorBad = colorBad }

            SetColorText str ->
                let
                    colorText =
                        Inputs.color model.style.colorText str
                in
                    setInput "colorText" str { oldStyle | colorText = colorText }

            SetColorBorder str ->
                let
                    colorBorder =
                        Inputs.color model.style.colorBorder str
                in
                    setInput "colorBorder" str { oldStyle | colorBorder = colorBorder }

            SetSpacing str ->
                let
                    spacing =
                        Inputs.int model.style.spacing str
                in
                    setInput "spacing" str { oldStyle | spacing = spacing }

            SetAnimationMs str ->
                let
                    animationMs =
                        Inputs.int model.style.animationMs str
                in
                    setInput "animationMs" str { oldStyle | animationMs = animationMs }

            SetBaseHeight str ->
                let
                    baseHeight =
                        Inputs.int model.style.baseHeight str
                in
                    setInput "baseHeight" str { oldStyle | baseHeight = baseHeight }

            Reset ->
                empty ! []



-- VIEWS


view : Model -> H.Html Msg
view model =
    H.div [ S.class S.Settings ]
        [ fieldsView model ]


fieldsView : Model -> H.Html Msg
fieldsView model =
    let
        input type' toStr label getVal fieldName msg =
            H.label [ S.class S.Input ]
                [ H.strong [ S.class S.InputLabel ] [ H.text label ]
                , H.input
                    [ S.class S.InputField
                    , A.placeholder label
                    , A.type' type'
                    , A.value
                        <| Maybe.withDefault (toStr <| getVal model.style)
                            (Dict.get fieldName model.inputs)
                    , E.onInput msg
                    ]
                    []
                ]

        colorInput =
            input "color" S.colorToHex

        intInput =
            input "number" toString
    in
        H.fieldset []
            [ colorInput "Good" .colorGood "colorGood" SetColorGood
            , colorInput "Bad" .colorBad "colorBad" SetColorBad
            , colorInput "Text" .colorText "colorText" SetColorText
            , colorInput "Border" .colorBorder "colorBorder" SetColorBorder
            , intInput "Spacing" .spacing "spacing" SetSpacing
            , intInput "Speed" .animationMs "animationMs" SetAnimationMs
            , H.button [ E.onClick Reset ] [ H.text "Reset" ]
            ]

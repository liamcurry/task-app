module Component.HabitForm
    exposing
        ( Model
        , empty
        , decoder
        , encode
        , Msg(..)
        , init
        , update
        , view
        )

-- LIB

import Json.Decode as D exposing ((:=))
import Json.Encode as E
import Html as H
import Html.Attributes as A
import Html.Events as E


-- INTERNAL

import Model.Habit as Habit
import Model.Inputs as Inputs
import Style as S


-- MODEL


type alias Model =
    { inputs : Inputs.Inputs
    , habit : Habit.Habit
    }


empty : Model
empty =
    { inputs = Inputs.empty
    , habit = Habit.empty
    }


decoder : D.Decoder Model
decoder =
    D.object2 Model
        ("inputs" := Inputs.decoder)
        ("habit" := Habit.decoder)


encode : Model -> E.Value
encode model =
    E.object
        [ ( "inputs", Inputs.encode model.inputs )
        , ( "habit", Habit.encode model.habit )
        ]



-- STATE


type Msg
    = SetName String
    | SetUnit String
    | SetTarget String
    | SetInterval String
    | Save


init : ( Model, Cmd Msg )
init =
    empty ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        habit =
            model.habit

        setInput field value newHabit =
            { model
                | habit = newHabit
                , inputs = Inputs.insert field value model.inputs
            }
                ! []
    in
        case msg of
            SetName str ->
                setInput "name" str { habit | desc = str }

            SetUnit str ->
                setInput "unit" str { habit | unit = str }

            SetTarget str ->
                let
                    target =
                        Inputs.float model.habit.target str
                in
                    setInput "target" str { habit | target = target }

            SetInterval str ->
                let
                    interval =
                        Inputs.float model.habit.interval str
                in
                    setInput "interval" str { habit | interval = interval }

            Save ->
                { model | habit = Habit.empty } ! []



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div [ S.class S.HabitForm ]
        [ H.form []
            [ fieldsView model ]
        ]


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
                        <| Maybe.withDefault (toStr <| getVal model.habit)
                            (Inputs.get fieldName model.inputs)
                    , E.onInput msg
                    ]
                    []
                ]

        colorInput =
            input "color" S.colorToHex

        numInput =
            input "number" toString

        strInput =
            input "text" (\s -> s)
    in
        H.form [ E.onSubmit Save ]
            [ strInput "Name" .desc "desc" SetName
            , strInput "Unit" .unit "unit" SetUnit
            , numInput "Target" .target "target" SetTarget
            , numInput "Interval" .interval "interval" SetInterval
            , H.button [ A.type' "submit" ]
                [ H.text "Save" ]
            ]

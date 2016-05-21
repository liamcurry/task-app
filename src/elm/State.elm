module State
    exposing
        ( init
        , update
        , subscriptions
        , Msg(..)
        )

import AnimationFrame
import Model exposing (..)
import Time exposing (Time)


type Msg
    = NoOp
    | CheckIn HabitId
    | Reset
    | Tick Time


init : Maybe Model -> ( Model, Cmd Msg )
init model =
    ( Maybe.withDefault emptyModel model
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        CheckIn id ->
            let
                addCheckin habit =
                    if habit.id == id then
                        checkin model.now habit
                    else
                        habit

                habits =
                    List.map addCheckin model.habits
            in
                ( { model | habits = habits }
                , Cmd.none
                )

        Reset ->
            let
                habits =
                    model.habits
                        |> List.map (\h -> { h | checkins = pastCheckins model.now h })
            in
                ( { model | habits = habits }
                , Cmd.none
                )

        Tick time ->
            ( { model | now = time }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    AnimationFrame.times Tick

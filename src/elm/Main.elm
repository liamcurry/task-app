port module Main exposing (..)

import Json.Decode as D
import Html.App as App
import Model exposing (Model)
import State exposing (Msg)
import View


main : Program D.Value
main =
    App.programWithFlags
        { init = State.init
        , view = View.root
        , update = (\msg model -> withSetStorage (State.update msg model))
        , subscriptions = State.subscriptions
        }


port setStorage : D.Value -> Cmd msg


port focus : String -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage ( model, cmds ) =
    model ! [ setStorage <| Model.encode model, cmds ]

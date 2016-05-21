port module Main exposing (..)

import Html.App as App
import Model exposing (Model)
import State exposing (Msg)
import View


main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = State.init
        , view = View.root
        , update = (\msg model -> withSetStorage (State.update msg model))
        , subscriptions = State.subscriptions
        }


port setStorage : Model -> Cmd msg


port focus : String -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage ( model, cmds ) =
    ( model, Cmd.batch [ setStorage model, cmds ] )

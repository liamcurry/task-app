module App.Habit (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Date exposing (Date)


type alias Id =
  Int


type alias Model =
  { id : Id
  }


type Action
  = NoOp


init : Model
init =
  {}


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "task" ]
    [ text "This is a habit" ]

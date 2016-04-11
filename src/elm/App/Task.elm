module App.Task (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (Effects)
import Date exposing (Date)


type alias Id =
  Int


type alias Model =
  { id : Id
  , description : String
  , completed : Bool
  , added : Date
  , starts : Date
  , deadline : Maybe Date
  }


type Action
  = NoOp


emptyModel : Model
emptyModel =
  { id = -1
  , description = ""
  , completed = False
  , added = Date.fromTime 0
  , starts = Date.fromTime 0
  , deadline = Nothing
  }


init : Model
init =
  emptyModel


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "task" ]
    [ text model.description ]

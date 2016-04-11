module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (Effects, Never)
import StartApp
import Task exposing (Task)
import App.Task


type alias Model =
  { tasks : List App.Task.Model
  }


type Action
  = NoOp
  | TaskAction App.Task.Id App.Task.Action


sampleTasks : List App.Task.Model
sampleTasks =
  let
    emptyTask =
      App.Task.emptyModel
  in
    [ { emptyTask
        | id = 1
        , description = "Floss"
        , completed = False
      }
    ]


emptyModel : Model
emptyModel =
  { tasks = sampleTasks }


init : ( Model, Effects Action )
init =
  ( emptyModel
  , Effects.none
  )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    TaskAction id taskAction ->
      let
        updateTask task =
          if task.id == id then
            App.Task.update taskAction task
          else
            ( task, Effects.none )

        updated =
          List.map updateTask model.tasks

        tasks =
          updated
            |> List.map fst

        fx =
          updated
            |> List.filter (\( t, f ) -> f /= Effects.none)
            |> List.map (\( t, f ) -> Effects.map (TaskAction t.id) f)
      in
        ( { model | tasks = tasks }
        , Effects.batch fx
        )


view : Signal.Address Action -> Model -> Html
view address model =
  let
    taskView task =
      App.Task.view (Signal.forwardTo address (TaskAction task.id)) task
  in
    div
      [ class "container" ]
      [ h1
          []
          [ text "Tasks" ]
      , ul
          []
          (List.map taskView model.tasks)
      ]


inputs : List (Signal Action)
inputs =
  []


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks

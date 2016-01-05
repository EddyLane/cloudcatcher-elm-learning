module Main where

import CloudcatcherThree exposing (init, update, view, initialModel, actions, inbox)
import StartApp
import Task
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, targetValue, on)
import Http
import Json.Decode as Json exposing(Decoder, (:=))
import Task exposing (andThen, Task)
import Dict

--app =
--  StartApp.start
--    { init = init
--    , update = update
--    , view = view
--    , inputs = []
--    }
--main
--main =
--  app.html

--port tasks : Signal (Task.Task Never ())
--port tasks =
--  app.tasks


model : Signal CloudcatcherThree.Model
model =
  Signal.foldp update initialModel actions.signal

formattedModel = 
    Signal.map .searchTerm model

port modelChanges : Signal String
port modelChanges = 
  formattedModel

main : Signal Html
main =
  Signal.map (view actions.address) model

--port getStorage : Maybe CloudcatcherThree.Model
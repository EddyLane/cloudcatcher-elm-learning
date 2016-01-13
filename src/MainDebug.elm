module MainDebug where

import CloudcatcherThree exposing (update, view, emptyModel, listToDict, modelDecoder, modelEncoder, Action(..))
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
import Debug


app =
  StartApp.start
    { init = (emptyModel, Effects.none), 
      update = update, 
      view = view, 
      inputs = []
    }    

main =
  app.html
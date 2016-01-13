module Main where

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

port fullModelChanges : Signal CloudcatcherThree.ModelOutput
port fullModelChanges = 
    Signal.map modelEncoder app.model


getImages : CloudcatcherThree.Model -> List String
getImages model = model.visiblePodcasts 
                  |> List.filterMap (\v -> Dict.get v model.podcasts) 
                  |> List.map .image

port incomingImages : Signal (List String)
port incomingImages = 
    Signal.map getImages app.model |> Signal.dropRepeats

port getStorage : Maybe CloudcatcherThree.ModelOutput

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port addImageTwo : Signal CloudcatcherThree.LocalImage

addImageTwoLogger = Signal.map AddImage addImageTwo

initialModel : CloudcatcherThree.Model
initialModel =
    case getStorage of
        Just data -> modelDecoder data
        Nothing -> emptyModel

app =
  StartApp.start
    { init = (initialModel, Effects.none), 
      update = update, 
      view = view, 
      inputs = [addImageTwoLogger]
    }    

main =
  app.html
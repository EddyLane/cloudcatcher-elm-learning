module Main where

import CloudcatcherThree exposing (update, view, emptyModel, listToDict, modelDecoder, modelEncoder, Action(..), router)
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
import Time

port fullModelChanges : Signal CloudcatcherThree.ModelOutput
port fullModelChanges = 
    app.model 
      |> Signal.sampleOn delta
      |> Signal.dropRepeats
      |> Signal.map modelEncoder

getImages : CloudcatcherThree.Model -> List String
getImages model = 
    model.visiblePodcasts 
      |> List.filterMap (\v -> Dict.get v model.podcasts) 
      |> List.map .image

port incomingImages : Signal (List String)
port incomingImages = 
    app.model 
      |> Signal.sampleOn delta
      |> Signal.dropRepeats
      |> Signal.map getImages 

port getStorage : Maybe CloudcatcherThree.ModelOutput

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

port images : Signal CloudcatcherThree.LocalImage

delta : Signal Time.Time
delta =
 Signal.map Time.inSeconds (Time.fps 10)

addImage = Signal.map AddImage images

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
      inputs = [addImage, router.signal]
    }    

port routeRunTask : Task () ()
port routeRunTask =
  router.run

main =
  app.html
module Main where

import CloudcatcherThree exposing (update, view, emptyModel, actions, inbox, listToDict)
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

--initialModel : CloudcatcherThree.Model
--initialModel =
--    let storage = 
--        case getStorage of
--            Just data -> modelDecoder data
--            Nothing -> emptyModel
--    in 
--        storage

--init : (CloudcatcherThree.Model, Effects Action)
--init = (  initialModel
--        , Effects.none
--        )

modelDecoder : CloudcatcherThree.ModelOutput -> CloudcatcherThree.Model
modelDecoder model =
    { model | podcasts = listToDict .id model.podcasts }
--
initialModel : CloudcatcherThree.Model
initialModel =
    let storage = 
        case getStorage of
            Just data -> modelDecoder data
            Nothing -> emptyModel
    in 
        storage

--init = (  initialModel
--        , Effects.none
--        )

--app =
--  StartApp.start
--    { init = init
--    , update = update
--    , view = view
--    , inputs = [formattedModelTwo]
--    }

--main =
--  app.html

port getStorage : Maybe CloudcatcherThree.ModelOutput

--port tasks : Signal (Task.Task Never ())
--port tasks =
--  app.tasks




model : Signal CloudcatcherThree.Model
model =
  Signal.foldp update initialModel actions.signal


modelEncoder : CloudcatcherThree.Model -> CloudcatcherThree.ModelOutput
modelEncoder model =
    { model | podcasts = Dict.values model.podcasts }

formattedModelTwo = 
    Signal.map modelEncoder model

port fullModelChanges : Signal CloudcatcherThree.ModelOutput
port fullModelChanges = 
    formattedModelTwo

main : Signal Html
main =
  Signal.map (view actions.address) model


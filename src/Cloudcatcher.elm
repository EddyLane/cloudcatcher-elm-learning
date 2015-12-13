module Cloudcatcher where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import String exposing (toInt)
import Http
import StartApp.Simple as StartApp
import Json.Decode as Json exposing ((:=))

-- UTILS

onInput : Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

parseInt : String -> Int
parseInt string =
  case String.toInt string of
    Ok value ->
      value
    Err error ->
      0

--searchForPodcast query =
--  Http.get "https://itunes.apple.com/search?term=" ++ query ++ "&media=podcast"

newPodcast : String -> Podcast
newPodcast name = 
  {
    name = name
  }

-- MODEL

type alias Model = 
    { entries : List Podcast,
      searchInput : String 
    }

type alias Podcast =
    { name : String }

initialModel : Model
initialModel = 
    { entries = 
        [
          newPodcast "Test Podcast"
        ],
      searchInput = "" 
    }


-- UPDATE

type Action
    = NoOp
    | UpdateSearchInput String

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> 
            model

        UpdateSearchInput content ->
            { model | searchInput = content }


-- VIEW

pageHeader : Html
pageHeader = h1 [] [ text "Cloudcatcher" ]

pageFooter : Html
pageFooter = 
    footer
        [ ]
        [ a [ href "https://google.com" ] [ text "Cloudcatcher" ] ]

searchForm: Address Action -> Model -> Html
searchForm address model = 
    div [ class "form-inline" ]
        [ input 
            [ type' "text",
              class "form-control",
              placeholder "Search for a podcast",
              value model.searchInput,
              name "search",
              autofocus True,
              onInput address UpdateSearchInput
            ]
            [ ],
            button [ class "btn btn-primary" ] [ text "Search" ]

        ]

podcastListItem : Address Action -> Podcast -> Html
podcastListItem address podcast = 
  li [ ] 
     [ text podcast.name ]

podcastList: Address Action -> List Podcast -> Html
podcastList address entries = 
  let
    entryItems = List.map (podcastListItem address) entries
  in
    ul [ ] entryItems

view : Address Action -> Model -> Html
view address model = 
    div
    [ class "container" ]
    [ pageHeader,
      searchForm address model,
      podcastList address model.entries,
      pageFooter
    ]

-- WIRE TEH APP TOGETHER!

main: Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
     update = update
    }
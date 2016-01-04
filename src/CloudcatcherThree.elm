module CloudcatcherThree where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, targetValue, on)
import Http
import Json.Decode as Json exposing(Decoder, (:=))
import Task exposing (andThen, Task)
import Dict

-- MODEL

type alias PodcastDict = Dict.Dict Int Podcast

type alias Podcast =
    { name : String, 
      aritstName : String, 
      image : String,
      id : Int, 
      feedUrl : String
    }

type alias Model = 
    { podcasts : PodcastDict,
      visiblePodcasts : List Int,
      searchTerm : String
    }

-- UPDATE

type Action 
    = AddPodcasts PodcastDict 
    | UpdateSearchInput String
    | SubmitSearch String

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    AddPodcasts new -> 
        ({ model | 
            podcasts = Dict.union new model.podcasts,
            visiblePodcasts = Dict.keys new
         }
         , Effects.none
        )

    UpdateSearchInput term ->
        ({ model | searchTerm = term }
         , Effects.none
        )

    SubmitSearch term -> 
        ({ model | searchTerm = term }
         , getSearchResults term
        )  

-- EFFECTS

handleSearchResults : Maybe (List Podcast) -> Action
handleSearchResults podcast = 
    case podcast of
        Just e -> AddPodcasts (listToDict .id e)
        Nothing -> AddPodcasts Dict.empty

getSearchResults : String -> Effects Action
getSearchResults query = 
  Http.get podcasts (searchUrl query)
    |> Task.toMaybe
    |> Task.map handleSearchResults
    |> Effects.task

podcast : Json.Decoder (Podcast)
podcast = 
    Json.object5 Podcast
        ("name" := Json.string )
        ("artistName" := Json.string )
        ("image" := Json.string )
        ("id" := Json.int ) 
        ("feedUrl" := Json.string )

podcasts : Json.Decoder (List Podcast)
podcasts = "results" := Json.list podcast

-- VIEW

searchForm: Signal.Address Action -> String -> Html
searchForm address term = 
    div [ class "form-inline" ]
        [ input 
            [ type' "text",
              class "form-control",
              placeholder "Search for a podcast",
              value term,
              name "search",
              autofocus True,
              onInput address UpdateSearchInput
            ]
            [ ]
        , button 
            [ class "btn btn-primary"
            , onClick address (SubmitSearch term)
            ] 
            [ text "Search" ]
        ]

podcastListItem : Podcast-> Html
podcastListItem podcast = 
    a  [ href "#" ] 
       [ text podcast.name ]

podcastList: Signal.Address Action -> List Podcast -> Html
podcastList address entries = 
  let
    entryItems = List.map podcastListItem (List.sortBy .name entries)
  in
    div [ class "list-group" ] entryItems

visiblePodcasts : Dict.Dict comparable a -> List Int -> List a
visiblePodcasts haystack needles = 
  List.filterMap (\v -> Dict.get v haystack) needles

view : Signal.Address Action -> Model -> Html
view address model = 
    div [] 
        [ 
            searchForm address model.searchTerm,
            podcastList address (visiblePodcasts model.podcasts model.visiblePodcasts) 
        ]

-- UTILS

(=>) = (,)

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

listToDict : (a -> comparable) -> List a -> Dict.Dict comparable a
listToDict getKey values = Dict.fromList (List.map (\v -> (getKey v, v)) values)

searchUrl : String -> String
searchUrl term = 
  Http.url "http://127.0.0.1:9000/v1/podcasts"
    ["term" => term]

-- WIRE UP

init : (Model, Effects Action)
init = ( Model Dict.empty [] ""
        , Effects.none
        )
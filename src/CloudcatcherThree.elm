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

type alias Episode = 
    { title: String }    

type alias Model = 
    { podcasts : PodcastDict,
      visiblePodcasts : List Int,
      searchTerm : String,
      selectedPodcast: Maybe Int,
      subscribedPodcasts : List Int
    }

type alias ModelOutput = 
    { podcasts : List Podcast,
      visiblePodcasts : List Int,
      searchTerm : String,
      selectedPodcast: Maybe Int,
      subscribedPodcasts : List Int
    }

emptyModel : Model
emptyModel = Model Dict.empty [] "" Nothing []

-- UPDATE

type Action 
    = NoOp 
    | AddPodcasts PodcastDict 
    | UpdateSearchInput String
    | SubmitSearch String
    | SelectPodcast (Maybe Int)
    | TogglePodcastSubscribed Int

update : Action -> Model -> (Model, Effects Action)
update action model =
   case action of

    NoOp ->
        ( model
        , Effects.none 
        )

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

    SelectPodcast id ->
        ({ model | selectedPodcast = id }
         , Effects.none
        )

    TogglePodcastSubscribed id ->
      ( let
          subscribe = { model | subscribedPodcasts = id :: model.subscribedPodcasts }
          unsubscribe = { model | subscribedPodcasts = List.filter (\e -> e /= id) model.subscribedPodcasts }
        in
          if (List.member id model.subscribedPodcasts) then unsubscribe else subscribe
      , Effects.none
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

podcastListItem : Signal.Address Action -> Bool -> Bool -> Podcast-> Html
podcastListItem address isSelected isSubscribed podcast = 
  let
    listItemStyle = if isSelected then "list-group-item active" else "list-group-item"
    subscribedClass = if isSubscribed then "glyphicon glyphicon-star pull-right" else ""
  in
    a [ href "#",
        onClick address (SelectPodcast (Just podcast.id)), 
        class listItemStyle
      ]
      [ text podcast.name,
        span [ class subscribedClass ] []
      ]

podcastList: Signal.Address Action -> List Podcast -> List Int -> Maybe Int -> Html
podcastList address entries subscribedPodcasts selectedPodcast = 
  let
    isSelected e = e.id == Maybe.withDefault 0 selectedPodcast
    isSubscribed e = List.member e.id subscribedPodcasts
    entryItems = List.map (\e -> (podcastListItem address (isSelected e) (isSubscribed e) e)) (List.sortBy .name entries)
  in
    div [ class "list-group" ] entryItems

podcastDetails: Signal.Address Action -> Bool -> Podcast -> Html
podcastDetails address subscribed podcast = 
    div [ class "page-header" ] [
      h3 [] [ text podcast.name, small [] [ text podcast.aritstName ] ],
      img [ src podcast.image ] [],
      button [ class (if subscribed then "btn active" else "btn btn-primary"),
               onClick address (TogglePodcastSubscribed podcast.id)
             ]
             [ text (if subscribed then "Unsubscribe" else "Subscribe") ]
    ]


-- MAIN

view : Signal.Address Action -> Model -> Html
view address model = 
  let
    visiblePodcasts = List.filterMap (\v -> Dict.get v model.podcasts) model.visiblePodcasts
    createPodcastList = podcastList address visiblePodcasts model.subscribedPodcasts model.selectedPodcast
    createSearchForm = searchForm address model.searchTerm
    podcastDisplay = 
      case Dict.get (Maybe.withDefault 0 model.selectedPodcast) model.podcasts  of
        Just e -> podcastDetails address (List.member e.id model.subscribedPodcasts) e
        Nothing -> div [] []
  in
    div [ class "container-fluid" ] 
        [ 
          div [ class "col-md-6 col-sm-6 col-lg-6"]
          [ createSearchForm,
            createPodcastList
          ],
          div [ class "col-md-6 col-sm-6 col-lg-6"]
          [
            podcastDisplay
          ]
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
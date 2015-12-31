module CloudcatcherTwo where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, targetValue, on)
import Http
import Json.Decode as Json exposing(Decoder, (:=))
import Task exposing (andThen, Task)
import Dict

-- UTIL

filterVisible : Dict.Dict comparable a -> List Int -> Dict.Dict comparable a
filterVisible haystack needles = 
  Dict.filter (\v a -> (List.member v needles)) haystack

filterVisibleQuick : Dict.Dict comparable a -> List Int -> List a
filterVisibleQuick haystack needles = 
  List.filterMap (\v -> Dict.get v haystack) needles

-- The transformation function
listToDict : (a -> comparable) -> List a -> Dict.Dict comparable a
listToDict getKey values = Dict.fromList (List.map (\v -> (getKey v, v)) values)

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

-- MODEL
type alias Collection = List Podcast

type alias PodcastDict = Dict.Dict Int Podcast

type alias Podcast =
    { name : String
    , aritstName : String 
    , image : String
    , id : Int
    }

type alias Model =
    { topic : String
    , gifUrl : String
    , entries: Collection
    , searchInput: String
    , selectedPodcast: Maybe Int
    , entriesDict : PodcastDict 
    , visiblePodcasts : List Int
    , subscribedPodcasts : List Int
    }



init : String -> (Model, Effects Action)
init topic =
  ( Model topic "assets/waiting.gif" [] "" Nothing Dict.empty [] []
  , Effects.none
  )

-- UPDATE
type Action
    = RequestMore
    | SubmitSearch String
    | NewGif (Maybe String)
    | UpdateSearchInput String 
    | SelectPodcast (Maybe Int)
    | SetResults (Maybe Collection)
    | SubscribeToPodcast Int
    | UnsubscribePodcast Int



concatResults : List a -> Maybe (List a) -> List a
concatResults current new = List.append current (Maybe.withDefault [] new)

--addNewResults : List a -> Maybe (List a) -> List a
--addNewResults current new = List.filter

addEntries : PodcastDict -> Collection -> PodcastDict
--addEntries current new = Dict.union current (listToDict .id new)
addEntries current new = 
  listToDict .id new
  |> Dict.union current




update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestMore ->
      (model, getRandomGif model.topic)

    NewGif maybeUrl ->
      ({ model | gifUrl = (Maybe.withDefault model.gifUrl maybeUrl) }
        , Effects.none
      )
    
    --SetResults results ->
    --  ({ model | entries = (concatResults model.entries results) }
    --    , Effects.none
    --  )

    SetResults results ->
      ({ model | 
          entriesDict = addEntries model.entriesDict (Maybe.withDefault [] results),
          visiblePodcasts = List.map .id (Maybe.withDefault [] results)
        }
        , Effects.none
      )


    SubscribeToPodcast id ->
      ({ model | subscribedPodcasts = List.append [id] model.subscribedPodcasts
      }
      , Effects.none
      )


    UnsubscribePodcast id ->
      ( let
          remainingEntries = List.filter (\e -> e /= id) model.subscribedPodcasts
        in
          { model | subscribedPodcasts = remainingEntries }
      , Effects.none
      )

      --( Model model.topic model.gifUrl (Maybe.withDefault model.entries results) model.searchInput
      --, Effects.none
      --)

    UpdateSearchInput value ->
      ({ model | searchInput = value }
        , Effects.none
      )
  
    SelectPodcast id ->
      ({ model | selectedPodcast = id  }
        , Effects.none
      )

    SubmitSearch value -> 
      ({ model | searchInput = value }
        , getSearchResults model.searchInput
      )  


-- VIEW

-- HEADER / FOOTER

(=>) = (,)

pageHeader : Signal.Address Action -> Model -> Html
pageHeader address model = nav [ class "navbar navbar-default" ] 
                 [ 
                    div [ class "container-fluid" ] 
                        [
                            div [ class "navbar-form navbar-left" ]
                                [
                                    searchForm address model
                                ]
                        ]
                 ]

pageFooter : Html
pageFooter = 
    footer
        [ ]
        [ a [ href "https://google.com" ] [ text "Cloudcatcher" ] ]

searchForm: Signal.Address Action -> Model -> Html
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
            [ ]
          , button 
          [ class "btn btn-primary"
            , onClick address (SubmitSearch model.searchInput)
          ] 
          [ text "Search" ]
        ]


-- LEFT HAND SIDE

podcastListItem : Signal.Address Action -> Podcast -> Html
podcastListItem address podcast = 
  a  [ class "list-group-item"
      , href "#" 
      , onClick address (SelectPodcast (Just podcast.id))
      ] 
     [ text podcast.name 
     ]


podcastClasses : Bool -> String
podcastClasses active = if active then "list-group-item active" else "list-group-item"

podcastListItemStyle : Podcast -> Maybe Int -> String
podcastListItemStyle podcast selectedPodcast = podcastClasses (podcastIsActive selectedPodcast podcast)

podcastIsActive : Maybe Int -> Podcast -> Bool
podcastIsActive selectedPodcast podcast = Maybe.withDefault 0 selectedPodcast == podcast.id

podcastListItemTwo : Signal.Address Action -> Maybe Int -> List Int -> Podcast-> Html
podcastListItemTwo address selectedPodcast subscribedPodcasts podcast = 
  a  [ class (podcastListItemStyle podcast selectedPodcast)
      , href "#" 
      , onClick address (SelectPodcast (Just podcast.id))
      ] 
     [ 
        text podcast.name,
        if (List.member podcast.id subscribedPodcasts) then
          span [ class "glyphicon glyphicon-star pull-right"] []
         else 
          span [] []
     ]

podcastList: Signal.Address Action -> List Podcast -> Maybe Int -> List Int -> Html
podcastList address entries selectedPodcast subscribedPodcasts = 
  let
    entryItems = List.map (podcastListItemTwo address selectedPodcast subscribedPodcasts) (List.sortBy .name entries)
  in
    ul [ class "list-group" ] entryItems



-- RIGHT HAND SIDE


handleClick : Podcast -> Bool -> Action
handleClick podcast subscribed =
  if subscribed then
    UnsubscribePodcast podcast.id
  else
    SubscribeToPodcast podcast.id  

podcastDisplay : Signal.Address Action -> Podcast -> Bool -> Html
podcastDisplay address podcast subscribed = div [ class "page-header" ] [
    h3 [] [ text podcast.name, small [] [ text podcast.aritstName ] ],

    img [ src podcast.image ] [],

    div [] [

      button [ class (if subscribed then "btn btn-primary btn-lg pull-right active" else "btn btn-primary btn-lg pull-right")
               , onClick address (handleClick podcast subscribed) 
              ]
              [ text (if subscribed then "Unsubscribe" else "Subscribe") ]
    ]
  ]

rightColumnDisplay : Signal.Address Action -> Maybe Podcast -> List Int -> Html
rightColumnDisplay address podcast subscriptionIds = 
  div [] 
      [ 
          case podcast of
            Just value -> (podcastDisplay address value) (List.member value.id subscriptionIds)
            Nothing -> div [][ text "Select a podcast" ]
      ]


-- ROOT

view : Signal.Address Action -> Model -> Html
view address model = 
    div [] 
        [ 
            pageHeader address model,
            div
            [ class "container-fluid" ]
            [
              div [ class "col-md-6 col-sm-6 col-lg-6"]
              [ podcastList address (filterVisibleQuick model.entriesDict model.visiblePodcasts) model.selectedPodcast model.subscribedPodcasts
              ],
              div [ class "col-md-6 col-sm-6 col-lg-6"]
              [
                rightColumnDisplay address (Dict.get (Maybe.withDefault 0 model.selectedPodcast) model.entriesDict) model.subscribedPodcasts
              ]
            ]
        ]

headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]


imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "width" => "200px"
    , "height" => "200px"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++ url ++ "')")
    ]

-- EFFECTS



podcasts : Json.Decoder (List Podcast)
podcasts =
  let podcast =
    Json.object4 Podcast
            ("name" := Json.string )
            ("artistName" := Json.string )
            ("image" := Json.string )
            ("id" := Json.int ) 
  in 
    "results" := Json.list podcast

getRandomGif : String -> Effects Action
getRandomGif topic =
  Http.get decodeUrl (randomUrl topic)
    |> Task.toMaybe
    |> Task.map NewGif
    |> Effects.task

searchUrl : String -> String
searchUrl term = 
  Http.url "http://127.0.0.1:9000/v1/podcasts"
    ["term" => term]

getSearchResults : String -> Effects Action
getSearchResults query = 
  Http.get podcasts (searchUrl query)
    |> Task.toMaybe
    |> Task.map SetResults
    |> Effects.task

randomUrl : String -> String
randomUrl topic =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]

decodeUrl : Json.Decoder String
decodeUrl =
  Json.at ["data", "image_url"] Json.string
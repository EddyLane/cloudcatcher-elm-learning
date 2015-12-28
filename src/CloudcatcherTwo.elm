module CloudcatcherTwo where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, targetValue, on)
import Http
import Json.Decode as Json exposing(Decoder, (:=))
import Task

-- UTIL

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

-- MODEL
type alias Collection = List Podcast

type alias Podcast =
    { name : String,
      aritstName : String }

type alias Model =
    { topic : String
    , gifUrl : String
    , entries: Collection
    , searchInput: String
    }



init : String -> (Model, Effects Action)
init topic =
  ( Model topic "assets/waiting.gif" [] ""
  , Effects.none
  )


-- UPDATE

type Action
    = RequestMore
    | SubmitSearch String
    | NewGif (Maybe String)
    | UpdateSearchInput String 
    | SetResults (Maybe Collection)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestMore ->
      (model, getRandomGif model.topic)

    NewGif maybeUrl ->
      ({ model | gifUrl = (Maybe.withDefault model.gifUrl maybeUrl) }
        , Effects.none
      )
    
    SetResults results ->
      ( Model model.topic model.gifUrl (Maybe.withDefault model.entries results) model.searchInput
      , Effects.none
      )

    UpdateSearchInput value ->
      ({ model | searchInput = value }
        , Effects.none
      )
  
    SubmitSearch value -> 
      ({ model | searchInput = value }
        , getSearchResults model.searchInput
      )  


-- VIEW

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

podcastListItem : Signal.Address Action -> Podcast -> Html
podcastListItem address podcast = 
  li [ class "list-group-item" ] 
     [ text podcast.name 
     ]

podcastList: Signal.Address Action -> List Podcast -> Html
podcastList address entries = 
  let
    entryItems = List.map (podcastListItem address) entries
  in
    ul [ class "list-group" ] entryItems


view : Signal.Address Action -> Model -> Html
view address model = 
    div [] 
        [ 
            pageHeader address model,
            div
            [ class "container-fluid" ]
            [
              div [ class "col-md-6 col-sm-6 col-lg-6"]
              [ podcastList address model.entries
              ],
              div [ class "col-md-6 col-sm-6 col-lg-6"]
              [ text "Otherside" ]
              
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
    Json.object2 Podcast
            ("name" := Json.string )
            ("artistName" := Json.string )
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
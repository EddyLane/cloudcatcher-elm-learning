module Cloudcatcher where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import String exposing (toInt)
import Http
import StartApp.Simple as StartApp
import Json.Decode as Decode exposing (Decoder, (:=))
import Debug
import Result exposing (..)

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


--decoder : Decoder Podcast
--decoder = Decode.object1 Podcast
--            ("collectionName" := Decode.string )

--decoderCol : Decoder (List (Podcast))
--decoderCol = Decode.object1 identity
--            ("results" := Decode.list decoder)


podcasts : Decoder (List Podcast)
podcasts =
  let podcast =
    Decode.object2 Podcast
            ("collectionName" := Decode.string )
            ("artworkUrl100" := Decode.string )
  in 
    "results" := Decode.list podcast

newPodcast : String -> String -> Podcast
newPodcast name imageUrl = 
  {
    collectionName = name, 
    imgUrl = imageUrl
  }

-- MODEL

type alias Collection = List Podcast

type alias Model = 
    { entries : List Podcast,
      searchInput : String 
    }

type alias Podcast =
    { collectionName : String,
      imgUrl : String }

--testData = "{\"results\": [{ \"collectionName\": \"Test Podcast\" }, { \"collectionName\": \"Test Podcast Two\" }]}"
testData = "{\"resultCount\":1,\"results\": [{\"wrapperType\":\"track\", \"kind\":\"podcast\", \"artistId\":825701899, \"collectionId\":496893300, \"trackId\":496893300, \"artistName\":\"DevChat.tv\", \"collectionName\":\"JavaScript Jabber\", \"trackName\":\"JavaScript Jabber\", \"collectionCensoredName\":\"JavaScript Jabber\", \"trackCensoredName\":\"JavaScript Jabber\", \"artistViewUrl\":\"https://itunes.apple.com/us/artist/devchat.tv/id825701899?mt=2&uo=4\", \"collectionViewUrl\":\"https://itunes.apple.com/us/podcast/javascript-jabber/id496893300?mt=2&uo=4\", \"feedUrl\":\"http://feeds.feedwrench.com/JavaScriptJabber.rss\", \"trackViewUrl\":\"https://itunes.apple.com/us/podcast/javascript-jabber/id496893300?mt=2&uo=4\", \"artworkUrl30\":\"http://is5.mzstatic.com/image/thumb/Music2/v4/7c/50/e9/7c50e99f-5f2a-9f41-1eaa-116fcee0eb68/source/30x30bb.jpg\", \"artworkUrl60\":\"http://is5.mzstatic.com/image/thumb/Music2/v4/7c/50/e9/7c50e99f-5f2a-9f41-1eaa-116fcee0eb68/source/60x60bb.jpg\", \"artworkUrl100\":\"http://is5.mzstatic.com/image/thumb/Music2/v4/7c/50/e9/7c50e99f-5f2a-9f41-1eaa-116fcee0eb68/source/100x100bb.jpg\", \"collectionPrice\":0.00, \"trackPrice\":0.00, \"trackRentalPrice\":0, \"collectionHdPrice\":0, \"trackHdPrice\":0, \"trackHdRentalPrice\":0, \"releaseDate\":\"2015-12-09T16:00:00Z\", \"collectionExplicitness\":\"notExplicit\", \"trackExplicitness\":\"notExplicit\", \"trackCount\":189, \"country\":\"USA\", \"currency\":\"USD\", \"primaryGenreName\":\"Training\", \"radioStationUrl\":\"https://itunes.apple.com/station/idra.496893300\", \"artworkUrl600\":\"http://is5.mzstatic.com/image/thumb/Music2/v4/7c/50/e9/7c50e99f-5f2a-9f41-1eaa-116fcee0eb68/source/600x600bb.jpg\", \"genreIds\":[\"1470\", \"26\", \"1304\", \"1318\", \"1480\"], \"genres\":[\"Training\", \"Podcasts\", \"Education\", \"Technology\", \"Software How-To\"]}]}"

initialModel : Model
initialModel = 
    { 
      entries = withDefault [] ( Decode.decodeString podcasts testData ),
      searchInput = "" 
    }


-- UPDATE

type Action
    = NoOp
    | UpdateSearchInput String
    | SetResults Collection

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> 
            model

        UpdateSearchInput content ->
            { model | searchInput = content }

        SetResults results ->
            { model | entries = results }


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
     [ img [ src podcast.imgUrl ] [ ], 
       text podcast.collectionName 
     ]




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

main: Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
     update = update
    }


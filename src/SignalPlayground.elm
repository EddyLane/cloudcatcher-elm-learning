import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Window
import Signal
import Time

drawCircle : Float -> Float -> Form
drawCircle x y =
  circle 50
    |> filled red
    |> move (x, y)


view : (Int, Int) -> (Int, Int) -> Element
view (w, h) (mouseX, mouseY) =
  let
    (w', h') = (toFloat w, toFloat h)
    x = toFloat mouseX - (w' / 2)
    y = (h' / 2) - toFloat mouseY
  in
    collage w h [ drawCircle x y ]

update : Model -> Model -> Model
update update _ = update

mouse : Signal (Int, Int) 
mouse = 
  let
    delta = Time.fps 30
  in
    Signal.sampleOn delta Mouse.position


type alias Model = (Int, Int)

initialModel : Model
initialModel = (150, 150)

model : Signal Model
model = Signal.foldp update initialModel mouse

main : Signal Element
main = Signal.map2 view Window.dimensions model

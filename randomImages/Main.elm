import ListOfImages exposing (init, update, view, clockTime)
import Html
import Graphics.Element
main : Signal Graphics.Element.Element
main = Signal.map view (Signal.foldp update (init 1 10 10) clockTime)





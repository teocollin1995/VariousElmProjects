module Wacamole where
import Random
import Signal
import Debug
import Time
import Graphics.Input as I
import Graphics.Element as E
import Graphics.Collage as C
import Color exposing (rgb)
import Text as T

--model
type alias Hole = (Int, Bool)
--Hole number -  is there a mole there?
type alias State = (Int, List Hole )
--score, list of holes


numberOfHoles : Int
numberOfHoles  = 7

initState : State
initState = (0, List.map (\x -> (x, True)) [0..(numberOfHoles - 1)])

--signals
type Event = Next Int | Wac Int

moleChanges : Signal.Signal Int
moleChanges = Signal.map (fst) (Signal.map (Random.generate (Random.int 0 (numberOfHoles - 1))) (Signal.map (\x -> Random.initialSeed (round x)) (Time.every (0.7 *Time.second))))


events : Signal Event 
events = Signal.merge (Signal.map (Next) moleChanges) (wacs.signal)


wacs = Signal.mailbox (Wac (-1))

--upstate

upstate : Event -> State -> State
upstate event (n, holes) = 
  case event of 
    Next i ->  (n, List.indexedMap (\a (x,y) -> if a == i then (x,True) else (x, False)) holes)
    Wac j -> case (List.drop j holes) of 
               x::xs ->  case x of 
                          (_, True) -> (n+1, List.map (\(x,y) -> (x, False)) holes)
                          (_, False) -> ((n-1), holes)
               [] -> Debug.log "bad index" (n,holes)


---view stuff
emptyHole : E.Element
emptyHole = C.collage 100 100 [C.filled (rgb 0 255 0) (C.circle 50) ]


filledHole : E.Element
filledHole = C.collage 100 100 [C.filled (rgb 0 255 0) (C.circle 50), C.toForm (E.fittedImage 100 100 "mole.png")]


moleButton : E.Element
moleButton index =
  I.customButton (Signal.message wacs.address (Wac index)) filledHole filledHole emptyHole


noMoleButton : E.Element
noMoleButton index =
  I.customButton (Signal.message wacs.address (Wac index)) emptyHole emptyHole emptyHole

renderHole hole = 
  case hole of
    (i, True) -> moleButton i
    (i, False) -> noMoleButton i


view : State -> E.Element
view (score, holes) = E.centered (T.fromString ("Score is: " ++ toString score)) `E.below` E.flow E.left (List.map renderHole holes)
--main

main : E.Element
main = Signal.map view (Signal.foldp upstate initState events)
   



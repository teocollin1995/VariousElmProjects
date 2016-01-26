module Missiles where
import Time
import Signal
import Mouse
import Window
import Debug
import Graphics.Element as E
import Graphics.Collage as C
import Color exposing (rgb)
type alias Missile = (Float, Float, List (Float,Float))
start : Missile -> Float
start (x,y,z) = x
end : Missile -> Float
end (x,y,z) = y
mPath : Missile -> List (Float,Float)
mPath (x,y,z) = z
genMissile : Float -> Float -> Missile
genMissile x1 x2 = (x1,x2, [(x1,0)])

type Armed = Armed Float | Disarmed 
type alias State = (Time.Time, List (Missile), Armed)
initState : State
initState = (0, [], Disarmed)
stateTime : State -> Time.Time
stateTime (x,y,z) = x
getMissiles : State -> List (Missile)
getMissiles (x,y,z) = y
getArmedState : State -> Armed
getArmedState (x,y,z) = z

type Event = Arm Float | Fire Float | Tick Time.Time
{-

-}
missileFire : (Int, Int) -> (Int, Int) -> Event
missileFire (x,y) (w,h) = 
  let
    x1 = toFloat (x - (w//2))
    y1 = toFloat (y - (h//2))
  in
    if y1 < 10 && -10 < y1 then 
      if x1 < 0 then Fire x1
      else Arm x1
    else Tick (-1)

clockTime : Signal Event
clockTime = Signal.map (Tick) (Time.every (0.1 * Time.second))

missileEvents : Signal Event
missileEvents =  (Signal.map2 missileFire (Signal.sampleOn (Mouse.clicks) (Mouse.position)) Window.dimensions)

events = Signal.merge clockTime missileEvents

upMissile : Float -> Missile -> Missile
upMissile dt m = 
  let
    x2 = end m
    x1 = start m 
    lastcord = case (List.head (mPath m)) of 
                 Just x -> x
                 Nothing -> Debug.crash "This should never happen"
    lastx = fst lastcord
    deltax = (dt / Time.second) * 20
    newx = lastx - deltax
    newy = (-0.001) * (newx - x2) * (newx - x1)
  in
    Debug.log "ggg" (x1,x2, (newx,newy) :: (mPath m))
  

notoldMissile : Missile -> Bool
notoldMissile m = 
  let 
      lastcord = case (List.head (mPath m)) of 
                 Just x -> x
                 Nothing -> Debug.crash "This should never happen"

  in
    (snd lastcord) > -30 

hitMissile : Missile -> Bool
hitMissile m =
  let 
      lastcord = case (List.head (mPath m)) of 
                 Just x -> x
                 Nothing -> Debug.crash "This should never happen"
  in
    (snd lastcord) < 0 && (fst lastcord) < 0


upstate : Event -> State -> State
upstate e state = 
  case e of 
    Tick t -> if t >0 then 
                if stateTime state == 0 then (t, getMissiles state, getArmedState state)
                else 
                  let deltat = t - (stateTime state)
                      newM = List.filter (notoldMissile) (List.map (upMissile deltat) (getMissiles state))
                  in (t, newM, getArmedState state)
              else
                (stateTime state ,getMissiles state, getArmedState state)
    
    Arm newx -> (stateTime state, getMissiles state, Armed newx)
    Fire newx -> case (getArmedState state) of 
                   Armed startx -> (stateTime state, (genMissile startx newx) :: (getMissiles state), Disarmed)
                   _ -> (stateTime state, getMissiles state, Disarmed)




view : (Int, Int) -> State -> E.Element
view (w,h) state = 
  let
    xaxis = C.traced (C.solid (rgb 0 0 0)) (C.segment (toFloat (-w//2), 0)  ((toFloat (w//2)), 0))
    yaxis = C.traced (C.solid (rgb 0 0 0)) (C.segment (0, (toFloat (-h//2)))  (0, toFloat (h//2)))
    missiles = getMissiles state
    hitmissiles = List.filter hitMissile missiles
    incoming = List.filter (\x -> not (hitMissile x) ) missiles
    im = List.map (\x -> C.traced (C.dashed (rgb 0 255 255)) (C.path (mPath x) )) incoming
    hitCirles = List.map (\m -> C.moveX (end m) (C.filled (rgb 255 0 0) (C.circle 15))) hitmissiles
    
    
    
    

  in
    C.collage w h ([xaxis,yaxis] ++ im ++ hitCirles)


main = Signal.map2 view Window.dimensions (Signal.foldp upstate initState events)



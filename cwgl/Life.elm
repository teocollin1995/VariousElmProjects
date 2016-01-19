module Life where
import Array as Arr exposing (Array)
import Debug
import Graphics.Element as E exposing (Element)
import Graphics.Collage as C
import Signal as S
import Time
import Mouse
import Window
import Color
import Keyboard

--This is the game logic section


type alias Cell = {nb: Int, life: Bool}

--number of neighboors + Life
--True = aliave -- 

type alias Game = ((Int, Int), Array (Array Cell))
---n rows, m cols, Matrix of cells where 0,0 is upper left hand corner 

rows : Game -> Int 
rows = (\x -> fst (fst x))
cols : Game -> Int
cols = (\x -> snd (fst x))

--initates a cell with 0 neightbors and life n (NB) -> 0 is okay for the inital thing because we calculate NB counts then update life
initCell : Bool -> Cell
initCell n = 
  {nb = 0, life = n}


---n rows, n col, to game where everyone is dead
initGameEmpty : Int -> Int -> Game
initGameEmpty n m = 
  ((n,m), Arr.initialize n (always (Arr.initialize m (always (initCell False)))))

--turns a row of true/false into an array of cells
initRow : Array (Bool) -> Array (Cell)
initRow instructions =
  Arr.map initCell instructions


--Maybe Game because the intructions could be invalid and I may want to add an ability to preset intructions
--Instructions are an array of true or false where true in each spot is replaced with a living cell and false a dead cell
initGame : Array (Array Bool) -> Maybe (Game)
initGame instructions = 
  let 
    rowLengths = Arr.map (Arr.length) instructions
    firstLength =  case (Arr.get 0 rowLengths) of
                     Nothing -> 0
                     Just x -> x
    consistentRowLength = if firstLength /= 0 then List.all (\x -> x == firstLength) (Arr.toList rowLengths) else False
    --
  in
    if consistentRowLength then Just ((firstLength, Arr.length instructions), (Arr.map (\x -> initRow x) instructions)) else Nothing
    

  
matrixMap : (a -> b) -> Array (Array a) -> Array (Array b)
matrixMap f matrix = 
  Arr.map (\x -> Arr.map f x) matrix


--indexedMap but for a matrix where the function takes the row and then the col of an items position in the matrix
coordinateMap : (Int -> Int -> a -> a) -> Array (Array a) -> Array (Array a)
coordinateMap f matrix =
  Arr.indexedMap (\r array -> Arr.indexedMap (\c i -> f r c i ) array ) matrix


--get the life of a cell at row r and col c -> 1 if aliave nad 0 else (so we can easilly add them togeather)
lifeGet : Int -> Int -> Array (Array Cell) -> Int
lifeGet r c matrix = 
  let 
    row = Arr.get r matrix
  in
    case row of
      Nothing -> 0
      Just x -> case (Arr.get c x) of 
                  Nothing -> 0
                  Just z -> if z.life then 1 else 0


--update the number of living neighboors of a cell at row r and col c
-- treats 
updateNB : Int -> Int -> Array (Array Cell) -> Cell
updateNB r c matrix = 
  let
    l = case (Arr.get r matrix) of
          Just x -> case (Arr.get c x) of 
                      Just x -> x
                      _ -> Debug.crash "Not a cell" 
                          -- this should never be triggered ever
                          -- it exists to make the program compile
          _ -> Debug.crash "Not a cell" -- same thing
                     
    neighs = [(r-1,c-1),(r+1,c+1),(r-1,c+1),(r+1,c-1),(r,c+1),(r,c-1),(r-1,c),(r+1,c)] -- the coords around the cell r,c
    newNB =(List.foldr (+) 0 (List.map (\x -> lifeGet (fst x) (snd x) matrix) neighs))
  in
    {nb=newNB, life=l.life}


--update the nb count for every cell in the game  
updateGameNB : Game -> Game
updateGameNB game = 
  let 
    cells = snd game
    newCells = coordinateMap (\r c i -> updateNB r c cells) cells
  in
    (fst game, newCells)


--set value at a (r,c)
setMatrix : Int -> Int -> a -> Array (Array a) -> Array (Array a)
setMatrix r c new matrix =
  let 
    row = case (Arr.get r matrix) of
            Just x -> x 
            _ -> Debug.crash "Crap" -- again, it shouldn't be possible for this one to be called
    newrow = Arr.set c new row 
  in
    Arr.set r newrow matrix


--set the cell life at (r,c)                               
setCellLife : Int -> Int -> Bool -> Game -> Game
setCellLife r c life ((n,m), matrix) = 
  if n <= r || m <= c 
  then Debug.log "attempted to set cell life at invalid location" ((n,m), matrix) --I'm not sure if this can happen or not so it is made to be useful
  else ((n,m),(setMatrix r c {nb=0, life=life} matrix))
    


--updates a cell's life based on the number of neighboors 
updateCellLife : Cell -> Cell
updateCellLife {nb, life} = 
  case life of 
    True -> {nb=nb, life=nb == 2 || nb == 3 }
    False -> {nb=nb, life=nb == 3}


--updates the life status of every single cell using updateCellLife
updateGameLife game =
  let 
    cells = snd game
    newCells = matrixMap updateCellLife cells
  in
    (fst game, newCells)


--play around of the game -> update NB counts -> update life status of each cell
nround : Game -> Game
nround game = 
  updateGameLife (updateGameNB game)


--gets the matrix of cells from each game
gameToArray g = 
  case g of 
    ((_,_), a) -> a
  

--- end of game logic -- everything past this is FRP


type alias Playing = Bool -- should the game be moving from round to round
type alias State = (Game, Int, Playing)
---game, round, playing?


type Event = Tick Time.Time |Msg Bool  | MousePosition ((Int, Int), (Int, Int))
-- time update, playing update, mounse position and window size update


--Signalof mouse positions and window positions that only is emitted when the mouse is clicked
clickPositions : Signal ((Int, Int), (Int, Int))
clickPositions = S.map2 (\x y -> (x,y)) (S.sampleOn Mouse.clicks Mouse.position) (Window.dimensions )


--signals for a new round every second
clockTime : Signal Time.Time
clockTime = Time.every Time.second


--signal false whenever the space is pressed -> used to pause the game
startSignal : Signal Bool
startSignal = S.map (not) Keyboard.space


--all signals merged togeather
events : Signal Event
events = S.mergeMany [S.map Tick clockTime, S.map Msg startSignal, S.map MousePosition clickPositions  ]


--updates the state based on signals
upstate : Event -> State -> State
upstate event (game, round, playing) =
  case event of 
    Tick _ -> if playing then (nround game, round + 1, playing) else (game, round, playing)
    Msg newState -> (game, round, newState)
    MousePosition ((colp, rowp), (w,h)) -> let 
      a = (rowp // ((h//rows game)), colp // (w//(cols game))) --calculates the row and col of the cell that is being clicked on
      newr = (fst a)
      newc = (snd a)
   in if playing 
      then (game,round, playing) 
      else ((( setCellLife newr newc (1/=(lifeGet newr newc (snd game))) ( game))), round, playing)



rowInst = Arr.fromList ((List.repeat 20 False) ++ [True,True,True,True,True] ++ (List.repeat 20 False)) --example starting row
matrix = Arr.repeat 45 rowInst --example matrix based on row


--because initgame returns Just game (because I may want to allow users to specific intitial game state)
g = case (initGame matrix) of
      Just x -> x
      _ -> initGameEmpty 10 10

--Starting state of the game -> uses g which uses matrix which uses rowinst
initState : State
initState = (g, 0, True) 


--turns a cell into either a green or a white RECTANGLE outlined in black
cellSquare : Float -> Float -> Cell -> Element
cellSquare height length cell = 
  let
    shape = (C.rect length height)
    outlined =  C.outlined (C.solid (Color.rgb 0 0 0)) shape
    form = (if cell.life then C.filled (Color.rgb 0 255 0) shape else C.filled (Color.rgb 255 255 255) shape)
  in
    C.collage (round length)(round height) [form,outlined]


--window height and width -> game state -> display
view : (Int, Int) -> State -> Element
view  (w,h) (game,round,playing) = 
  let 
    rows = fst (fst game)
    cols = snd (fst game)
    rech = toFloat (h// rows) --calculates height of a cell rectange based on height pixels and number of rows
    recw = toFloat (w// cols) --calculates width of a cell rectange based on width pixels and number of cols
    matrix = gameToArray game --get the matrix of the game
    display = E.flow E.down (Arr.toList (Arr.map (\x -> E.flow E.right (List.map (\c -> cellSquare rech recw c) (Arr.toList x))) matrix))
    --[cell 0,0 : cell 0, 1] .... 
    --[cell 1,0 : cell 1,1] ... 
  in
    display
    
    
    
main : S.Signal Element
main = S.map2 view Window.dimensions (S.foldp upstate initState events )

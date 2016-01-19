module ListOfImages where 
import Html exposing (..)
import Html.Attributes exposing (..)
import Graphics.Element
import Signal
import Random

import Images
clockTime : Signal Float

clockTime = Images.clockTime
maxCount : number
maxCount = 20

type alias Model = 
                 {
                   imageList : List (Float, Images.Model) --time,image,id
                 , count : Int
                 , life : Float
                 , seed : Random.Seed
                 }

init : Int -> Int -> Int -> Model
init count start startSeed = Model (initList count start startSeed) count 0 (Random.initialSeed 100)
initList : number -> Int -> Int -> List (Float, Images.Model)
initList count start startSeed = if count == 0 then [] else (0, (Images.init start startSeed)) :: (initList (count - 1) (start - 1) (startSeed - 1))
  

update : Float -> Model -> Model
update a model = 
  let 
    (imageDuration, newSeed) = Random.generate (Random.float 5000 15000) model.seed
    (systemMult, newSeed2) = Random.generate (Random.int 2 6) newSeed
    systemDuration = (toFloat systemMult) *  imageDuration
  in
    if model.life == 0 then Model (List.map (\(x,y) -> (a,y)) model.imageList) model.count a newSeed2
    else if a > systemDuration + model.life then 
           if model.count <= maxCount then 
             Model ((a, Images.init 30 (round a) ) :: model.imageList ) (model.count + 1) a newSeed2
           else Model (List.take (maxCount - 5) (List.reverse (List.sortBy (\(x,y) -> x) model.imageList))) (maxCount - 5) a newSeed2
         else 
           let 
             expired = List.map ( \(time, img) -> (a, Images.update a img)) (List.filter ( \(x,y) -> a >= imageDuration + x) model.imageList)
             okay = List.filter ( \(x,y) -> a < imageDuration + x) model.imageList
           in Model (expired ++ okay) model.count model.life newSeed2
          
-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)



viewHelper images rowsize = if images /= [] then ((Graphics.Element.flow Graphics.Element.left (List.take rowsize images))  `Graphics.Element.below` ( viewHelper (List.drop rowsize images) rowsize )) else Graphics.Element.empty


view model = viewHelper (List.map (\(x,y) -> Images.view y) model.imageList) 5


-- view : Model -> Html
-- view model = 
--   div []
--         [
--          div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
--              (List.map Images.view (List.map (\(x,y) -> y) model.imageList))


--         ]
  

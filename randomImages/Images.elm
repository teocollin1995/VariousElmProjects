module Images where


import Html
import Html.Attributes exposing (style)
import Graphics.Element
import Random
import Time
min : Int
min = 0 -- set this
max : Int
max = 148 -- set this - should be largest index of image in assets/
gen : Random.Generator Int
gen = Random.int min max
clockTime : Signal Time.Time
clockTime = Time.every (2 * Time.second)


--MODEL
type alias Model = 
  { 
    image : Int,
    seed : Random.Seed
  }


init : Int -> Int -> Model
init s seed = Model s (Random.initialSeed seed)

type Action = Next
update: Float -> Model -> Model
update action model =
  case action of 
    _ ->
          let nxt = Random.generate gen model.seed
          in Model (fst nxt) (snd nxt)
     


            
-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)
view : Model -> Graphics.Element.Element 
view model = 
  Graphics.Element.fittedImage 500 500 ("assets/" ++ (toString model.image))






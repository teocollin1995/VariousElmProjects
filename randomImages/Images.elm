module Images where


import Html
import Html.Attributes exposing (style)
import Graphics.Element
import Random
import Time
min : Int
min = 0
max : Int
max = 148
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



-- view model =
--   div [ style [ "width" => "200px" ] ]
--     [ h2 [headerStyle] [text (toString model.image)]
--     , div [imgStyle ("assets/" ++ (toString model.image))] []
--     ]


-- headerStyle : Attribute
-- headerStyle =
--   style
--     [ "width" => "200px"
--     , "text-align" => "center"
--     ]


-- imgStyle : String -> Attribute
-- imgStyle loc =
--   style
--     [ "display" => "inline-block"
--     , "width" => "500px"
--     , "height" => "500px"
--     , "background-position" => "center center"
--     , "background-size" => "cover"
--     , "background-image" => ("url('" ++ loc ++ "')")
--     ]



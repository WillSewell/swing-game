import Keyboard
import Window
import Debug
import Math.Vector2 as V2

-- MODEL
type Mario = { x:Float, y:Float, vx:Float, vy:Float, dir:String }
type Point = { x:Float, y:Float, w:Float, h:Float, len: Float }
type Game = { mario:Mario, point:Point }

defaultGame : Game
defaultGame =
  { mario = { x=150, y=100, vx=0, vy=0, dir="right" },
    point = { x=0, y=10, w=10, h=10, len = 100 } }


-- UPDATE -- ("m" is for Mario)
jump : { x:Int, y:Int } -> Mario -> Mario
jump {y} m  = if y > 0 && m.y == 0 then { m | vy <- 5 } else m

gravity : Float -> Mario -> Mario
gravity t m = { m | vy <- m.vy - t/4 }

-- assume tethered, else do physics
tether : Float -> Point -> Mario -> Mario
tether t p m = let dist = distance (m.x, m.y) (p.x, p.y)
                   _ = Debug.log "m" m
                   mPos = V2.vec2 (Debug.log "origMX" m.x) (Debug.log "origMY" m.y)
                   pPos = V2.vec2 (Debug.log "origPX" p.x) (Debug.log "origPY" p.y)
                   mRelPoint = V2.sub mPos pPos
                   --_ = Debug.log "subbedY" (V2.getY mRelPoint)
                   normalisedVect = V2.normalize mRelPoint
                   --_ = Debug.log "normY" (V2.getY normalisedVect)
                   --_ = Debug.log "normX" (V2.getX normalisedVect)
                   scaledVect = V2.scale p.len normalisedVect
                   --_ = Debug.log "scaledY" (V2.getY scaledVect)
                   finalPos = V2.add scaledVect pPos
                   fPX = V2.getX finalPos
                   fPY = V2.getY finalPos
               in Debug.log "fM" { m | x <- fPX, y <- fPY, vx <- (fPX - p.x)/t, vy <- (fPY - p.y)/t }

physics : Float -> Mario -> Mario
physics t m = { m | x <- m.x + t*m.vx , y <- m.y + t*m.vy }

walk : { x:Int, y:Int } -> Mario -> Mario
walk {x} m  = { m | vx <- toFloat x
                  , dir <- if | x < 0     -> "left"
                              | x > 0     -> "right"
                              | otherwise -> m.dir }

distance : (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

step : (Float,{ x:Int, y:Int }) -> Game -> Game
step (t,dir) {mario,point} = { mario = mario |> jump dir |> walk dir |> gravity t |> physics t |> tether t point, point = point}

-- DISPLAY
render : (Int,Int) -> Game -> Element
render (w',h') {mario,point} =
  let (w,h) = (toFloat w', toFloat h')
      verb  = if | mario.y  >  0 -> "jump"
                 | mario.vx /= 0 -> "walk"
                 | otherwise     -> "stand"
      src   = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in collage w' h'
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
      , rect point.w point.h |> filled (rgb 255 255 255)
                             |> move (point.x, point.y)
      , segment (point.x,point.y) (mario.x,mario.y) |> traced defaultLine
      , segment (-1000,0) (1000,0) |> traced defaultLine
      , segment (0,-1000) (0,1000) |> traced defaultLine
      , circle point.len |> outlined (solid (rgb 50 150 200))
                         |> move (point.x, point.y)
      , toForm (image 35 35 src) |> move (mario.x, (Debug.log "FINAL Y" mario.y))
      ]

-- MARIO
input : Signal (Float,{ x:Int, y:Int })
input = let delta = lift (\t -> t/20) (fps 24)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

main  = lift2 render Window.dimensions (foldp step defaultGame input)

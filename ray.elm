import Vector
import Mouse
import Random
import Time
import Signal
import Array
import Color

drawPoint : (Int,Int, Float) -> Form
drawPoint (x,y,val) =  
            square 2
            |> filled (Color.greyscale (1-val))
            |> move (toFloat x,toFloat y)


screen : Int -> Int -> [Form]
screen w h = 
    let half x = floor ((toFloat x)/2) in
    let div x y = floor ((toFloat x)/(toFloat y)) in
    let array = Array.initialize (h*w) (\n -> ( rem n w - (half w),(half h) - (div n h), sin ((toFloat (rem n w))/(toFloat w))   )) in
    let array = Array.map rayTrace array in
    Array.toList (Array.map drawPoint array)
    
width = 800
height = 600
main = collage width height (screen width height)


clipMin = 100
clipMax = 1000

ray : (Int, Int) -> Vector.Vec3
ray (x, y) = Vector.unit ( (toFloat x), (toFloat y), clipMin)

ballZ = 300
ballR = 150

lightPos : Vector.Vec3
lightPos = (100, 2000, 200)

rayTrace : (Int,Int,Float) -> (Int,Int,Float)
rayTrace (x,y,val) = let ballPos = (0,0,ballZ) in
                     let dot = Vector.dot ballPos (ray (x,y)) in
                     let d = sqrt( ballZ*ballZ - dot*dot) in
                     if d > ballR then (x,y,0) else (x,y, shade d (ray (x,y)))


rayDistance d = (sqrt(ballZ*ballZ - d*d) - sqrt(ballR*ballR - d*d))

ambient = 0.4

shine x = if  |  x > 0.97   -> 1
              |  x > 0.93   -> 0.9
              |  x > 0.8   -> 0.8
              |  x > 0.5   -> 0.6
              |  x > 0.2   -> 0.4
              |  x > 0.1   -> 0.3
              |  otherwise ->  0.2

shade d i =
    let ray = Vector.mul (rayDistance d) i in
    let lightRelPos = Vector.sub lightPos ray in
    let lightRelPosUnit = Vector.unit lightRelPos in
    let sphereNormal = Vector.unit (Vector.sub ray (0,0,ballZ)) in
    let ilum = (Vector.dot lightRelPosUnit sphereNormal) in
    (0.8 * (shine ilum)) + 0.2*ilum
             

--    (0.6*(Vector.dot lightRelPosUnit sphereNormal) + ambient)

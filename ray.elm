import Vector
import Array
import Color


--- Functions


{- drawPoint : returns a pixel (size 2 square) Form 
               of color 'val' at position (x,y)   -}
drawPoint : (Int,Int, Float) -> Form
drawPoint (x,y,val) =  
            square 2
            |> filled (Color.greyscale (1-val))
            |> move (toFloat x,toFloat y)



{- screen : generates the rendered screen. For a given width/height
            it genereates the pixel positions, calls the ray-tracer 
            to calculate the pixel's value, returning a list 
            of Forms containing the final image.                  -}
screen : Int -> Int -> [Form]
screen w h = 
    let half x = floor ((toFloat x)/2) 
        div x y = floor ((toFloat x)/(toFloat y)) 
        array = Array.initialize (h*w) (\n -> ( rem n w - (half w),(half h) - (div n h), 0  ))
    in
        Array.map rayTrace array |> Array.map drawPoint 
                                 |> Array.toList
-- ==   Array.toList (Array.map drawPoint (Array.map rayTrace array))

{- ray : given a screen pixel (x,y), returns a unit vector 
         representing the direction of the ray to be 
         traced. -}
ray : (Int, Int) -> Vector.Vec3
ray (x, y) = Vector.unit ( (toFloat x), (toFloat y), clipMin)


{- rayTrace : Given a screen pixel, checks if the 
              ray intersects with the sphere, and 
              calculates the pixel's value.      -}
rayTrace : (Int,Int,Float) -> (Int,Int,Float)
rayTrace (x,y,val) = 
    let ballPos = (0,0,ballZ)
        dot = Vector.dot ballPos (ray (x,y))
        d = sqrt( ballZ*ballZ - dot*dot)
    in                  
        if d > ballR then (x,y,0) else (x,y, shade d (ray (x,y)))


{- rayDistance : calculates the distance between the camera (i.e., 
                 the origin, and the intersection point. That is  
                 the distance 'travelled' by the ray. -}
rayDistance d = (sqrt(ballZ*ballZ - d*d) - sqrt(ballR*ballR - d*d))


{- shade : Given intersection parameter 'd' and ray direction 'i', 
           calculate the pixel's value, i.e., calculate the 
           shading. 

           It calculates the difuse shading, i.., Normal DOT lightPos  -}
shade d i =
    let ray = Vector.mul (rayDistance d) i
        lightRelPos = Vector.sub lightPos ray 
        lightRelPosUnit = Vector.unit lightRelPos 
        sphereNormal = Vector.unit (Vector.sub ray (0,0,ballZ)) 
        ilum = (Vector.dot lightRelPosUnit sphereNormal) 
    in
        (0.4 * (shine ilum)) + 0.4*ilum + 0.2


{- shine : Treshold shading. -}
shine x = if  |  x > 0.97   -> 1
              |  x > 0.93   -> 0.9
              |  x > 0.8   -> 0.8
              |  x > 0.5   -> 0.6
              |  x > 0.2   -> 0.4
              |  x > 0.1   -> 0.3
              |  otherwise ->  0.2
             

-- Parameters and main


---- Clip volume parameters
clipMin = 100
clipMax = 1000

---- Sphere parameters
ballZ = 300
ballR = 150

---- Light parameters
lightPos : Vector.Vec3
lightPos = (100, 100, 0)


---- Screen paremeters
width = 800
height = 600

---- main collage
main = collage width height (screen width height)
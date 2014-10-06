{-  

Vector.elm : A 3-D vector module.

Author : Daniel Fortes, daniel.mbfm@gmail.com

-}

module Vector where 

type Vec3 = (Float, Float, Float)

i : Vec3
i = (1,0,0)
j : Vec3
j = (0,1,0)
k : Vec3
k = (0,0,1)

add : Vec3 -> Vec3 -> Vec3
add (x,y,z) (u,v,w) = (x+u,y+v,z+w)

mul : Float -> Vec3 -> Vec3
mul a (x,y,z) = (a*x,a*y,a*z)

sub : Vec3 -> Vec3 -> Vec3
sub a b = add a (mul -1 b)

dot : Vec3 -> Vec3 -> Float
dot (x,y,z) (u,v,w) = x*u + y*v + z*w

abs : Vec3 -> Float
abs v = sqrt (dot v v)

unit : Vec3 -> Vec3
unit v = mul (1/(abs v)) v


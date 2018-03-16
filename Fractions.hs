module Fractions where 
-------------------Fractions type----------------------------


data Frac = Frac {
    numerator :: Int
    ,
    dominator :: Int

    } deriving (Show, Eq)



frac_simplify (Frac _ 0) = Frac 0 0
frac_simplify (Frac 0 _) = Frac 0 0
frac_simplify (Frac a b) = if a < 0 && b <0 then (frac_simplify (Frac (-a) (-b))) else
                           Frac (quot a gcd) (quot b gcd)
                               where gcd = euclid a b
                                     euclid x y =
                                         if x < 0 then  (-1) * (euclid (-x) y) else
                                         if y < 0 then  (-1) * (euclid x (-y)) else
                                         if x == y then x else
                                         if x < y  then (euclid x (y - x)) else
                                         euclid (x - y) x

instance Num Frac where



    (+) (Frac a b) (Frac 0 0)
        = Frac a b
    (+) (Frac 0 0) (Frac a b)
        = Frac a b
    (+) (Frac a b) (Frac x y)
        = frac_simplify $! Frac (a*y + x * b) (y*b)

    (*) (Frac a b) (Frac x y)
        = frac_simplify $! Frac (a*x) (b*y)

    (-) (Frac a b) (Frac x y)
        = frac_simplify $! (Frac a b) + (Frac (-1) 1) * (Frac x y)

    abs  (Frac a b)
        = Frac (abs a) (abs b)

    signum (Frac a b)
        =    if a == 0  || b == 0 then Frac 0 0    else
              if a*b < 0 then Frac (-1) 1 else
              Frac 1 1


    fromInteger n
         = Frac (fromIntegral n) 1


divideFrac (Frac a b) (Frac x y)
    = frac_simplify $! (Frac a b) * (Frac y x)

intToFrac
    = \n -> Frac n 1

fracToFloat (Frac a b) = (fromIntegral  a) / (fromIntegral b)

recipFrac (Frac a b) = Frac b a
---------------------Proper fractions type---------------------

data Prop = Prop {

    whole :: Int
    ,
    remainder :: Frac
    } deriving (Show, Eq)

prop_simplify (Prop n (Frac 0 0)) = Prop n (Frac 0 0)
prop_simplify (Prop n (Frac a b)) = Prop (n + n') (frac_simplify $!  Frac (a - (n' * b)) b)
        where n' = quot a b



instance Num Prop where

    (+) (Prop n (Frac a b)) (Prop n' (Frac x y) )
         = prop_simplify $! Prop (n + n') ((Frac a b) + (Frac x y))
    (*) p1 p2
         = prop_simplify $! fractToProp $! (propToFrac p1 ) * (propToFrac p2 )

    (-) (Prop n (Frac a b)) (Prop n' (Frac x y) )
         = prop_simplify $! Prop (n - n') ((Frac a b) - (Frac x y))

    abs (Prop n (Frac a b))
         = Prop (abs n) (abs (Frac a b))

    signum (Prop n (Frac a b))
         = Prop (signum n) (Frac 0 0)


    fromInteger n
        = Prop (fromIntegral n) (Frac 0 0)

divideProp p1 p2
    = fractToProp $! divideFrac (propToFrac p1) (propToFrac p2)
intToProp
    = \n -> Prop n  (Frac 0 0)
fractToProp
    = \f -> prop_simplify $! Prop 0 (frac_simplify f)
propToFrac
    = \(Prop n (Frac a b)) ->frac_simplify $!  Frac ((b*n)+a) b

---------------------values--------------------------

piFrac = Frac 355 113 

eFrac  = Frac 87 32

---------------------Main----------------------------

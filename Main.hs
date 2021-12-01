import MathInfo
import Scalar
import Vector
import NumberList

val :: (RealFloat a) => Scalar a
val = complex 5.78 0.111

vec :: (RealFloat a) => Vector a
vec = value $ vector [real 1, real 2, real 3, real 4, complex 3.3 5]

main :: IO ()
main = putStrLn $ show $ scos val

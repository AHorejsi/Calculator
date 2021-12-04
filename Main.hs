import MathInfo
import Scalar
import Vector
import NumberList
import Data.Number.BigFloat

val1 :: BigScalar
val1 = real 7.3

val2 :: BigScalar
val2 = complex 4.5 (-0.9)

vec :: BigVector
vec = value $ vector [real 1, complex 2 6, real 3, real 4, complex 3.3 5]

main :: IO ()
main = putStrLn $ show $ spow val1 val2

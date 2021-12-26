import MathInfo
import BigScalar
import BigVector
import BigList

import Data.Number.Fixed

num1 :: Fixed Prec50
num1 = -1

val1 :: BigScalar
val1 = real $ -1

val2 :: BigScalar
val2 = complex 4.5 (-0.9)

val3 :: BigScalar
val3 = quaternion 7.9 6.1 0.4 8.3

vec1 :: BigVector
vec1 = value $ vlist [real 1, complex 2 6, real 3, real 4, complex 3.3 5, real 1]

list1 :: BigList
list1 = llist [real 1, real 2, real 1, real 1, real 2, real 3, real 2, complex 1 2, complex 1 2, complex 1 2, complex 1 2, complex 1 2]

main :: IO ()
main = putStrLn $ "\n" ++ (show $ sasin val1) ++ "\n"

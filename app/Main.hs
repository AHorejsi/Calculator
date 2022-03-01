import MathInfo
import BigScalar
import BigVector
import BigList
import BigMatrix
import Data.Char
import Data.Bits

list1 :: BigList
list1 = llist [integer 1, integer 2, integer 3, integer 3, integer 4]

list2 :: BigList
list2 = llist [integer 2, integer 3, integer 4, integer 3]

val1 :: BigScalar
val1 = integer $ -1501

val2 :: BigScalar
val2 = integer 1000

mat1 :: BigMatrix
mat1 = mlist [[real $ -6.5, integer 9, real 1.7],[real $ -7.8, real 66.5, real 4.07],[real $ -8.99, real 4.5, real $ -0.8]]

main :: IO ()
main = putStrLn $ "\n" ++ (show $ lIsSorted list1) ++ "\n"

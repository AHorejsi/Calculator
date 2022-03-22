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
val2 = integer 1001

mat1 :: BigMatrix
mat1 = mlist [[integer 1, integer 2, integer 3],[integer 4, integer 5, integer 6],[integer 7, integer 8, integer 9]]

main :: IO ()
main = putStrLn $ "\n" ++ (show $ spow one imagI) ++ "\n"

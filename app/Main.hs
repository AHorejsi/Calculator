import Stringify
import MathInfo
import BigScalar
import BigVector
import BigList
import BigMatrix
import MathEntity
import Parse

val1 :: BigScalar
val1 = complex 9 (9543.35431)

three :: BigScalar
three = integer 3

vec1 :: BigVector
vec1 = value $ vlist [integer 1, integer 2, integer 3]

list1 :: BigList
list1 = llist [integer 1, integer 2, integer 3, integer 4]

mat1 :: BigMatrix
mat1 = mlist [[integer 1, integer 2, integer 3, integer 10],[integer 4, integer 5, integer 6, integer 11],[integer 7, integer 8, integer 9, integer 12],[integer 13, integer 14, integer 15, integer 16]]

true :: MathEntity
true = makeBool True

false :: MathEntity
false = makeBool False

main :: IO ()
main = putStrLn $ "\n" ++ (stringify vec1) ++ "\n" ++ (stringify list1) ++ "\n" ++ (stringify true) ++ "\n" ++ (stringify false) ++ "\n"

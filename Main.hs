import MathInfo
import BigScalar
import BigVector
import BigList
import BigMatrix
import Data.Char
import Data.Bits

mat1 :: BigMatrix
mat1 = mlist [[integer 1, integer 2, integer 3],[integer 4, integer 5, integer 6]]

mat2 :: BigMatrix
mat2 = mlist [[integer 1, integer 2],[integer 3, integer 4],[integer 5, integer 6]]

mat3 :: BigMatrix
mat3 = mlist [[integer 1, integer 2, integer 3],[integer 4, integer 5, integer 6], [integer 7, integer 8, integer 9]]

val1 :: BigScalar
val1 = integer 5000

main :: IO ()
main = putStrLn $ "\n" ++ (toBinary val1)  ++ "\n"

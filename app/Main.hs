import MathInfo
import BigScalar
import BigVector
import BigList
import BigMatrix
import Debug

val1 :: BigScalar
val1 = quaternion 9 (-9543.35431) 43452 (-4534.433333)

three :: BigScalar
three = integer 3

mat1 :: BigMatrix
mat1 = mlist [[integer 1, integer 2, integer 3, integer 10],[integer 4, integer 5, integer 6, integer 11],[integer 7, integer 8, integer 9, integer 12],[integer 13, integer 14, integer 15, integer 16]]

main :: IO ()
main = putStrLn $ "\n" ++ (show $ withValue mat1) ++ "\n" ++ (show $ mSwapCols mat1 two zero) ++ "\n"

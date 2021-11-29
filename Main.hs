import MathInfo
import Scalar
import Vector

val :: (RealFloat a) => Scalar a
val = realValue 2

main :: IO ()
main = putStrLn $ show $ satan val

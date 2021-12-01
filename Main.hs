import MathInfo
import Scalar
import Vector

val :: (RealFloat a) => Scalar a
val = real 7

main :: IO ()
main = putStrLn $ show $ sasinh val

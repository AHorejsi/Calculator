import MathInfo
import Scalar
import Vector

val :: (RealFloat a) => Scalar a
val = complex 2 0

main :: IO ()
main = putStrLn $ show $ sasin val

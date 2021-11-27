import MathInfo
import Scalar
import Vector
import Data.HashSet (HashSet, fromList)

com :: (RealFloat a) => Scalar a
com = complexValue 1.2 3.4

main :: IO ()
main = putStrLn $ show $ sacos imagI

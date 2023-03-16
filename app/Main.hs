{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant $" #-}

import qualified BigNumber as BN

val1 :: BN.BigNumber
val1 = BN.makeReal 50

val2 :: BN.BigNumber
val2 = BN.makeReal 0.33

val3 :: BN.BigNumber
val3 = BN.makeComplex (9.43562009786000005433) (5.573944567346245)

val4 :: BN.BigNumber
val4 = BN.makeQuaternion (57893.008943534123645798) (-563.05340534798564213) (-8.935345254652314878979431) (10.4857384655089403285904318590222)

main :: IO ()
main = putStrLn $ "\n" ++ (show a) ++ "\n" -- Computes some huge number that takes a long time to complete
    where a = BN.exponential val3

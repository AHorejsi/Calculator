{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant $" #-}

import qualified Stringify as S
import qualified Actions as A
import qualified BigNumber as BN
import qualified Data.Number.BigFloat as BF

val0 :: BN.BigNumber (BF.BigFloat BF.Prec50)
val0 = BN.exponential $ BN.multiply BN.imagI BN.piValue

main :: IO ()
main = putStrLn $ "\n" ++ str0 ++ "\n" ++ str1 ++ "\n"
    where str0 = show $ BN.rounded val0 (BN.real 50)
          str1 = show $ val0

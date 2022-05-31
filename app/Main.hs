{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant $" #-}

module Main (
    main
) where
    import Prelude hiding (div, pow)
    import Stringify
    import MathInfo
    import CalcSettings
    import BigScalar
    import BigVector
    import BigMatrix
    import Actions
    import Parse
    
    val1 :: BigScalar
    val1 = integer 94750231

    val2 :: BigScalar
    val2 = real 10.0000023459999

    val3 :: BigScalar
    val3 = complex (9.43562009786000005433) (47563.573944567346245)

    val4 :: BigScalar
    val4 = quaternion (57893.008943534123645798) (563.05340534798564213) (8.935345254652314878979431) (10.4857384655089403285904318590222)

    vec1 :: BigVector
    vec1 = fromList [integer 1, integer $ -2, integer 3, integer $ -1, integer 5]

    mat1 :: BigMatrix
    mat1 = mlist [[integer 1, integer 2, integer 3, integer 10],[integer 4, integer 5, integer 6, integer 11],[integer 7, integer 8, integer 9, integer 12],[integer 13, integer 14, integer 15, integer 16]]

    printer :: (Stringifier a) => a -> String
    printer = stringify (prints settings)

    main :: IO ()
    main = putStrLn $ "\n" ++ (show minVal) ++ "\n" ++ (show maxVal) ++ "\n"
        where minVal = findMin vec1
              maxVal = findMax vec1

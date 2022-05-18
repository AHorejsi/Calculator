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
    import BigList
    import BigMatrix
    import MathEntity
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
    vec1 = value $ vlist [integer 1, integer 2, integer 3]

    list1 :: BigList
    list1 = llist [integer $ -1, integer 1, integer 2, integer 3, integer 4, integer 3, integer 10, integer 9]

    mat1 :: BigMatrix
    mat1 = mlist [[integer 1, integer 2, integer 3, integer 10],[integer 4, integer 5, integer 6, integer 11],[integer 7, integer 8, integer 9, integer 12],[integer 13, integer 14, integer 15, integer 16]]

    true :: MathEntity
    true = makeBool True

    false :: MathEntity
    false = makeBool False

    printSets :: PrintSettings
    printSets = prints settings

    main :: IO ()
    main = putStrLn strs
        where strs = "\n" ++ (stringify printSets val1) ++ "\n" ++ (stringify printSets val2) ++ "\n" ++ (stringify printSets val3) ++ "\n" ++ (stringify printSets val4) ++ "\n"

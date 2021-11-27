{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    MathError(
        DivideByZero,
        ZeroToPowerOfZero,
        LogarithmOfZero,
        InvalidType
    ),
    MathResult,
    withValue,
    withError,
    isSuccess,
    isFailure,
    mathValue,
    mathError,
    combineErrors,
    (==),
    (/=),
    show
) where
    import GHC.Generics (Generic)
    import Data.Hashable (Hashable, hash)
    import Data.HashSet (HashSet, unions, empty, insert)

    data MathError = 
        DivideByZero |
        ZeroToPowerOfZero |
        LogarithmOfZero |
        InvalidType 
        deriving (Eq, Show, Enum, Generic)

    data MathResult a = Success {
        mathValue :: a
    } | Failure {
        mathError :: HashSet MathError
    } deriving (Eq, Show)

    instance Hashable MathError

    withValue :: a -> MathResult a
    withValue value = Success value

    withError :: MathError -> MathResult a
    withError errorValue = Failure $ insert errorValue empty

    isSuccess :: MathResult a -> Bool
    isSuccess Success{} = True
    isSuccess Failure{} = False

    isFailure :: MathResult a -> Bool
    isFailure Success{} = False
    isFailure Failure{} = True

    combineErrors :: HashSet MathError -> HashSet MathError -> MathResult a
    combineErrors first second = Failure $ unions [first, second]

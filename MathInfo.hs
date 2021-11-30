{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    MathError(
        DivideByZero,
        ZeroToPowerOfZero,
        LogarithmOfZero,
        LogarithmBaseOfZero,
        InvalidIndex,
        InvalidLength,
        InvalidType
    ),
    MathResult,
    withValue,
    withError,
    withErrorSet,
    isSuccess,
    isFailure,
    value,
    errorSet,
    combine,
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
        LogarithmBaseOfZero |
        InvalidIndex |
        InvalidLength |
        InvalidType
        deriving (Eq, Show, Enum, Generic)

    data MathResult a = Success {
        _val :: a
    } | Failure {
        _errors :: HashSet MathError
    } deriving (Eq, Show)

    instance Hashable MathError

    withValue :: a -> MathResult a
    withValue value = Success value

    withError :: MathError -> MathResult a
    withError errorValue = withErrorSet $ insert errorValue empty

    withErrorSet :: HashSet MathError -> MathResult a
    withErrorSet errorSet = Failure errorSet

    value :: MathResult a -> a
    value (Success val) = val
    value Failure{} = error "Invalid result"

    errorSet :: MathResult a -> HashSet MathError
    errorSet result
        | isSuccess result = empty
        | otherwise = _errors result

    isSuccess :: MathResult a -> Bool
    isSuccess Success{} = True
    isSuccess Failure{} = False

    isFailure :: MathResult a -> Bool
    isFailure result = not $ isSuccess result

    combine :: MathResult a -> MathResult b -> (a -> b -> MathResult c) -> MathResult c
    combine left right func
        | (isSuccess left) && (isSuccess right) = func leftValue rightValue
        | (isFailure left) && (isFailure right) = Failure $ unions [leftErrorSet, rightErrorSet]
        | isFailure left = withErrorSet leftErrorSet
        | isFailure right = withErrorSet rightErrorSet
        where leftValue = value left
              rightValue = value right
              leftErrorSet = errorSet left
              rightErrorSet = errorSet right

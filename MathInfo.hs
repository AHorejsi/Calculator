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
    resolveLeft,
    resolveRight,
    computeLeft,
    computeRight,
    combine,
    hash,
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

    _convert :: MathResult a -> MathResult b
    _convert result
        | isFailure result = withErrorSet $ errorSet result
        | otherwise = error "Result is valid"

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

    resolveLeft :: MathResult a -> b -> (a -> b -> MathResult c) -> MathResult c
    resolveLeft left right func
        | isSuccess left = func leftValue right
        | otherwise = _convert left
        where leftValue = value left
              leftErrorSet = errorSet left

    resolveRight :: a -> MathResult b -> (a -> b -> MathResult c) -> MathResult c
    resolveRight left right func
        | isSuccess right = func left rightValue
        | otherwise = _convert right
        where rightValue = value right

    computeLeft :: MathResult a -> b -> (a -> b -> c) -> MathResult c
    computeLeft left right func
        | isSuccess left = withValue $ func leftValue right
        | otherwise = _convert left
        where leftValue = value left

    computeRight :: a -> MathResult b -> (a -> b -> c) -> MathResult c
    computeRight left right func
        | isSuccess right = withValue $ func left rightValue
        | otherwise = _convert right
        where rightValue = value right

    combine :: MathResult a -> MathResult b -> (a -> b -> MathResult c) -> MathResult c
    combine left right func
        | (isSuccess left) && (isSuccess right) = func leftValue rightValue
        | (isFailure left) && (isFailure right) = Failure $ unions [leftErrorSet, rightErrorSet]
        | isFailure left = _convert left
        | isFailure right = _convert right
        where leftValue = value left
              rightValue = value right
              leftErrorSet = errorSet left
              rightErrorSet = errorSet right

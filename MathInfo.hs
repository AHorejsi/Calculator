{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    MathError(
        DivideByZero,
        ZeroToPowerOfZero,
        LogarithmOfZero,
        LogarithmBaseOfZero,
        InvalidIndex,
        InvalidLength,
        UnequalDimensions,
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
    convert,
    hash,
    (==),
    (/=),
    show
) where
    import GHC.Generics
    import Data.Hashable
    import Data.HashSet

    data MathError =
        DivideByZero |
        ZeroToPowerOfZero |
        LogarithmOfZero |
        LogarithmBaseOfZero |
        InvalidIndex |
        InvalidLength |
        UnequalDimensions |
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
    withError errorValue = withErrorSet $ singleton errorValue

    withErrorSet :: HashSet MathError -> MathResult a
    withErrorSet errorSet = Failure errorSet

    convert :: MathResult a -> MathResult b
    convert result
        | isFailure result = withErrorSet $ errorSet result
        | otherwise = error "Result is valid"

    value :: MathResult a -> a
    value result
        | isSuccess result = _val result
        | otherwise = error "Result is invalid"

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
        | otherwise = convert left
        where leftValue = value left
              leftErrorSet = errorSet left

    resolveRight :: a -> MathResult b -> (a -> b -> MathResult c) -> MathResult c
    resolveRight left right func
        | isSuccess right = func left rightValue
        | otherwise = convert right
        where rightValue = value right

    computeLeft :: MathResult a -> b -> (a -> b -> c) -> MathResult c
    computeLeft left right func
        | isSuccess left = withValue $ func leftValue right
        | otherwise = convert left
        where leftValue = value left

    computeRight :: a -> MathResult b -> (a -> b -> c) -> MathResult c
    computeRight left right func
        | isSuccess right = withValue $ func left rightValue
        | otherwise = convert right
        where rightValue = value right

    combine :: MathResult a -> MathResult b -> (a -> b -> MathResult c) -> MathResult c
    combine left right func
        | (isSuccess left) && (isSuccess right) = func leftValue rightValue
        | (isFailure left) && (isFailure right) = Failure $ unions [leftErrorSet, rightErrorSet]
        | isFailure left = convert left
        | isFailure right = convert right
        where leftValue = value left
              rightValue = value right
              leftErrorSet = errorSet left
              rightErrorSet = errorSet right

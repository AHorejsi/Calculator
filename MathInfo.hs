{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    MathError(
        DivideByZero,
        ZeroToPowerOfZero,
        LogarithmOfZero,
        LogarithmBaseOfZero,
        InvalidIndex,
        InvalidLength,
        UnequalLength,
        InvalidType
    ),
    MathResult,
    withValue,
    withError,
    withErrorSet,
    withErrorList,
    isSuccess,
    isFailure,
    value,
    errorSet,
    resolve,
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
    import Data.Hashable as Hashable
    import Data.HashSet as HashSet

    data MathError =
        DivideByZero |
        ZeroToPowerOfZero |
        LogarithmOfZero |
        LogarithmBaseOfZero |
        InvalidIndex |
        InvalidLength |
        UnequalLength |
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
    withError errorValue = withErrorSet $ HashSet.singleton errorValue

    withErrorSet :: HashSet MathError -> MathResult a
    withErrorSet errorSet = Failure errorSet

    withErrorList :: [MathError] -> MathResult a
    withErrorList errorList
        | (length errorList) /= (HashSet.size errorSet) = error "Duplicate errors"
        | otherwise = withErrorSet errorSet
        where errorSet = HashSet.fromList errorList

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
        | isSuccess result = HashSet.empty
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
    resolveRight left right func = resolveLeft right left (flip func)

    resolve :: MathResult a -> (a -> b) -> MathResult b
    resolve result func
        | isSuccess result = withValue $ func resultValue
        | otherwise = convert result
        where resultValue = value result

    computeLeft :: MathResult a -> b -> (a -> b -> c) -> MathResult c
    computeLeft left right func
        | isSuccess left = withValue $ func leftValue right
        | otherwise = convert left
        where leftValue = value left

    computeRight :: a -> MathResult b -> (a -> b -> c) -> MathResult c
    computeRight left right func = computeLeft right left (flip func)

    combine :: MathResult a -> MathResult b -> (a -> b -> MathResult c) -> MathResult c
    combine left right func
        | (isSuccess left) && (isSuccess right) = func leftValue rightValue
        | (isFailure left) && (isFailure right) = withErrorSet $ HashSet.unions [leftErrorSet, rightErrorSet]
        | isFailure left = convert left
        | isFailure right = convert right
        where leftValue = value left
              rightValue = value right
              leftErrorSet = errorSet left
              rightErrorSet = errorSet right

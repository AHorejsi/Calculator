{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    MathError(
        DivideByZero,
        ZeroToPowerOfZero,
        LogarithmOfZero,
        LogarithmBaseOfZero,
        InvalidIndex,
        InvalidLength,
        ZeroLength,
        UnequalLength,
        NoncomparableType,
        InvalidType
    ),
    MathResult,
    UnaryOperation,
    ErrableUnaryOperation,
    BinaryOperation,
    ErrableBinaryOperation,
    withValue,
    withError,
    withErrorSet,
    withErrorList,
    isSuccess,
    isFailure,
    value,
    errorSet,
    unResolve,
    binResolveLeft,
    binResolveRight,
    errUnResolve,
    errBinResolveLeft,
    errBinResolveRight,
    binCombine,
    errBinCombine,
    convert,
    hash,
    (==),
    (/=),
    show
) where
    import GHC.Generics
    import Text.Printf
    import Data.Hashable as Hashable
    import Data.HashSet as HashSet

    data MathError =
        DivideByZero |
        ZeroToPowerOfZero |
        LogarithmOfZero |
        LogarithmBaseOfZero |
        InvalidIndex |
        InvalidLength |
        ZeroLength |
        UnequalLength |
        NoncomparableType |
        InvalidType
        deriving (Eq, Show, Enum, Generic)

    data MathResult a = Success {
        _val :: a
    } | Failure {
        _errors :: HashSet MathError
    } deriving (Eq)

    instance (Show a) => Show (MathResult a) where
        show (Success val) = printf "Success{ %s }" (show val)
        show (Failure errors) = printf "Failure{ %s }" (_str $ HashSet.toList errors)

    type UnaryOperation a b = (a -> b)
    type ErrableUnaryOperation a b  = (a -> MathResult b)
    type BinaryOperation a b c  = (a -> b -> c)
    type ErrableBinaryOperation a b c = (a -> b -> MathResult c)

    instance Hashable MathError

    _str :: [MathError] -> String
    _str [] = ""
    _str [error] = show error
    _str (error:errors) = (show error) ++ ", " ++ (_str errors)

    withValue :: a -> MathResult a
    withValue = Success

    withError :: MathError -> MathResult a
    withError errorValue = withErrorSet $ HashSet.singleton errorValue

    withErrorSet :: HashSet MathError -> MathResult a
    withErrorSet = Failure

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
    isFailure = not . isSuccess

    unResolve :: MathResult a -> UnaryOperation a b -> MathResult b
    unResolve result func
        | isSuccess result = withValue $ func resultValue
        | otherwise = convert result
        where resultValue = value result

    binResolveLeft :: MathResult a -> b -> BinaryOperation a b c -> MathResult c
    binResolveLeft left right func
        | isSuccess left = withValue $ func leftValue right
        | otherwise = convert left
        where leftValue = value left

    binResolveRight :: a -> MathResult b -> BinaryOperation a b c -> MathResult c
    binResolveRight left right func = binResolveLeft right left (flip func)

    errUnResolve :: MathResult a -> ErrableUnaryOperation a b -> MathResult b
    errUnResolve result func
        | isSuccess result = func resultValue
        | otherwise = convert result
        where resultValue = value result

    errBinResolveLeft :: MathResult a -> b -> ErrableBinaryOperation a b c -> MathResult c
    errBinResolveLeft left right func
        | isSuccess left = func leftValue right
        | otherwise = convert left
        where leftValue = value left

    errBinResolveRight :: a -> MathResult b -> ErrableBinaryOperation a b c -> MathResult c
    errBinResolveRight left right func = errBinResolveLeft right left (flip func)

    binCombine :: MathResult a -> MathResult b -> BinaryOperation a b c -> MathResult c
    binCombine left right func
        | (isSuccess left) && (isSuccess right) = withValue $ func leftValue rightValue
        | (isFailure left) && (isFailure right) = withErrorSet $ HashSet.unions [leftErrorSet, rightErrorSet]
        | isFailure left = convert left
        | isFailure right = convert right
        where leftValue = value left
              rightValue = value right
              leftErrorSet = errorSet left
              rightErrorSet = errorSet right

    errBinCombine :: MathResult a -> MathResult b -> ErrableBinaryOperation a b c -> MathResult c
    errBinCombine left right func
        | (isSuccess left) && (isSuccess right) = func leftValue rightValue
        | (isFailure left) && (isFailure right) = withErrorSet $ HashSet.unions [leftErrorSet, rightErrorSet]
        | isFailure left = convert left
        | isFailure right = convert right
        where leftValue = value left
              rightValue = value right
              leftErrorSet = errorSet left
              rightErrorSet = errorSet right

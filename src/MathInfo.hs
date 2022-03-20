{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    MathError(
        DivideByZero,
        ZeroToPowerOfZero,
        LogarithmOfZero,
        LogarithmBaseOfZero,
        InvalidIndex,
        NotMultipliableMatrices,
        NonsquareMatrix,
        ZeroDeterminant,
        NullVector,
        InvalidLength,
        ZeroLength,
        UnequalLength,
        NoncomparableType,
        UnsortedList,
        IncompatibleTypes,
        InvalidType
    ),
    Result,
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
    H.hash,
    H.hashWithSalt,
    (==),
    (/=),
    show
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Hashable as H
    import qualified Data.HashSet as HS

    data MathError =
        DivideByZero |
        ZeroToPowerOfZero |
        LogarithmOfZero |
        LogarithmBaseOfZero |
        InvalidIndex |
        NullVector |
        NonsquareMatrix |
        NotMultipliableMatrices |
        ZeroDeterminant |
        InvalidLength |
        ZeroLength |
        UnequalLength |
        NoncomparableType |
        UnsortedList |
        InvalidType |
        IncompatibleTypes
        deriving (Enum, G.Generic, Eq, Show)

    data Result a = Success {
        _val :: a
    } | Failure {
        _errors :: HS.HashSet MathError
    } deriving (Eq)

    instance (Show a) => Show (Result a) where
        show (Success val) = TP.printf "Success{ %s }" (show val)
        show (Failure errors) = TP.printf "Failure{ %s }" (_str $ HS.toList errors)

    type UnaryOperation a b = a -> b
    type ErrableUnaryOperation a b = a -> Result b
    type BinaryOperation a b c = a -> b -> c
    type ErrableBinaryOperation a b c = a -> b -> Result c

    instance H.Hashable MathError where
        hashWithSalt salt value = H.hashWithSalt salt (fromEnum value)

    _str :: [MathError] -> String
    _str [] = ""
    _str [error] = show error
    _str (error:errors) = (show error) ++ ", " ++ (_str errors)

    withValue :: a -> Result a
    withValue = Success

    withError :: MathError -> Result a
    withError errorValue = withErrorSet $ HS.singleton errorValue

    withErrorSet :: HS.HashSet MathError -> Result a
    withErrorSet = Failure

    withErrorList :: [MathError] -> Result a
    withErrorList errorList
        | (length errorList) /= (HS.size errors) = error "Duplicate errors"
        | otherwise = withErrorSet errors
        where errors = HS.fromList errorList

    convert :: Result a -> Result b
    convert fail@Failure{} = withErrorSet $ errorSet fail
    convert Success{} = error "Result is valid"

    value :: Result a -> a
    value (Success val) = val
    value (Failure errors) = error "Result is invalid"

    errorSet :: Result a -> HS.HashSet MathError
    errorSet Success{} = HS.empty
    errorSet (Failure errors) = errors

    isSuccess :: Result a -> Bool
    isSuccess Success{} = True
    isSuccess Failure{} = False

    isFailure :: Result a -> Bool
    isFailure = not . isSuccess

    unResolve :: Result a -> UnaryOperation a b -> Result b
    unResolve (Success val) func = withValue $ func val
    unResolve fail@Failure{} _ = convert fail

    binResolveLeft :: Result a -> b -> BinaryOperation a b c -> Result c
    binResolveLeft (Success leftVal) rightVal func = withValue $ func leftVal rightVal
    binResolveLeft fail@Failure{} _ _ = convert fail

    binResolveRight :: a -> Result b -> BinaryOperation a b c -> Result c
    binResolveRight left right func = binResolveLeft right left (flip func)

    errUnResolve :: Result a -> ErrableUnaryOperation a b -> Result b
    errUnResolve (Success val) func = func val
    errUnResolve fail@Failure{} _ = convert fail

    errBinResolveLeft :: Result a -> b -> ErrableBinaryOperation a b c -> Result c
    errBinResolveLeft (Success leftVal) rightVal func = func leftVal rightVal
    errBinResolveLeft fail@Failure{} _ _ = convert fail

    errBinResolveRight :: a -> Result b -> ErrableBinaryOperation a b c -> Result c
    errBinResolveRight left right func = errBinResolveLeft right left (flip func)

    binCombine :: Result a -> Result b -> BinaryOperation a b c -> Result c
    binCombine (Success leftVal) (Success rightVal) func = withValue $ func leftVal rightVal
    binCombine (Failure leftErrorSet) (Failure rightErrorSet) _ = withErrorSet $ HS.unions [leftErrorSet, rightErrorSet]
    binCombine leftFail@Failure{} _ _ = convert leftFail
    bincCombine _ rightFail@Failure{} _ = convert rightFail

    errBinCombine :: Result a -> Result b -> ErrableBinaryOperation a b c -> Result c
    errBinCombine (Success leftVal) (Success rightVal) func = func leftVal rightVal
    errBinCombine (Failure leftErrorSet) (Failure rightErrorSet) _ = withErrorSet $ HS.unions [leftErrorSet, rightErrorSet]
    errBinCombine leftFail@Failure{} _ _ = convert leftFail
    errBinCombine _ rightFail@Failure{} _ = convert rightFail

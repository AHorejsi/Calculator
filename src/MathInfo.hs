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

    data MathResult a = Success {
        _val :: a
    } | Failure {
        _errors :: HS.HashSet MathError
    } deriving (Eq)

    instance (Show a) => Show (MathResult a) where
        show (Success val) = TP.printf "Success{ %s }" (show val)
        show (Failure errors) = TP.printf "Failure{ %s }" (_str $ HS.toList errors)

    type UnaryOperation a b = a -> b
    type ErrableUnaryOperation a b = a -> MathResult b
    type BinaryOperation a b c = a -> b -> c
    type ErrableBinaryOperation a b c = a -> b -> MathResult c

    instance H.Hashable MathError where
        hashWithSalt salt value = H.hashWithSalt salt (fromEnum value)

    _str :: [MathError] -> String
    _str [] = ""
    _str [error] = show error
    _str (error:errors) = (show error) ++ ", " ++ (_str errors)

    withValue :: a -> MathResult a
    withValue = Success

    withError :: MathError -> MathResult a
    withError errorValue = withErrorSet $ HS.singleton errorValue

    withErrorSet :: HS.HashSet MathError -> MathResult a
    withErrorSet = Failure

    withErrorList :: [MathError] -> MathResult a
    withErrorList errorList
        | (length errorList) /= (HS.size errors) = error "Duplicate errors"
        | otherwise = withErrorSet errors
        where errors = HS.fromList errorList

    convert :: MathResult a -> MathResult b
    convert fail@Failure{} = withErrorSet $ errorSet fail
    convert Success{} = error "Result is valid"

    value :: MathResult a -> a
    value (Success val) = val
    value (Failure errors) = error "Result is invalid"

    errorSet :: MathResult a -> HS.HashSet MathError
    errorSet Success{} = HS.empty
    errorSet (Failure errors) = errors

    isSuccess :: MathResult a -> Bool
    isSuccess Success{} = True
    isSuccess Failure{} = False

    isFailure :: MathResult a -> Bool
    isFailure = not . isSuccess

    unResolve :: MathResult a -> UnaryOperation a b -> MathResult b
    unResolve (Success val) func = withValue $ func val
    unResolve fail@Failure{} _ = convert fail

    binResolveLeft :: MathResult a -> b -> BinaryOperation a b c -> MathResult c
    binResolveLeft (Success leftVal) rightVal func = withValue $ func leftVal rightVal
    binResolveLeft fail@Failure{} _ _ = convert fail

    binResolveRight :: a -> MathResult b -> BinaryOperation a b c -> MathResult c
    binResolveRight left right func = binResolveLeft right left (flip func)

    errUnResolve :: MathResult a -> ErrableUnaryOperation a b -> MathResult b
    errUnResolve (Success val) func = func val
    errUnResolve fail@Failure{} _ = convert fail

    errBinResolveLeft :: MathResult a -> b -> ErrableBinaryOperation a b c -> MathResult c
    errBinResolveLeft (Success leftVal) rightVal func = func leftVal rightVal
    errBinResolveLeft fail@Failure{} _ _ = convert fail

    errBinResolveRight :: a -> MathResult b -> ErrableBinaryOperation a b c -> MathResult c
    errBinResolveRight left right func = errBinResolveLeft right left (flip func)

    binCombine :: MathResult a -> MathResult b -> BinaryOperation a b c -> MathResult c
    binCombine (Success leftVal) (Success rightVal) func = withValue $ func leftVal rightVal
    binCombine (Failure leftErrorSet) (Failure rightErrorSet) _ = withErrorSet $ HS.unions [leftErrorSet, rightErrorSet]
    binCombine leftFail@Failure{} _ _ = convert leftFail
    bincCombine _ rightFail@Failure{} _ = convert rightFail

    errBinCombine :: MathResult a -> MathResult b -> ErrableBinaryOperation a b c -> MathResult c
    errBinCombine (Success leftVal) (Success rightVal) func = func leftVal rightVal
    errBinCombine (Failure leftErrorSet) (Failure rightErrorSet) _ = withErrorSet $ HS.unions [leftErrorSet, rightErrorSet]
    errBinCombine leftFail@Failure{} _ _ = convert leftFail
    errBinCombine _ rightFail@Failure{} _ = convert rightFail

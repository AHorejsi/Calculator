{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    ComputationError(
        InvalidValue,
        InvalidType,
        InvalidState
    ),
    ComputationResult,
    UnaryAction,
    ErrableUnaryAction,
    BinaryAction,
    ErrableBinaryAction,
    TernaryAction,
    ErrableTernaryAction,
    withValue,
    withError,
    withErrorSet,
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
    convert
) where
    import qualified Data.Hashable as H
    import qualified Data.HashSet as HS
    import qualified Text.Printf as TP
    import qualified Data.Foldable as F
    import qualified Data.List as L
    import qualified Stringify as Str

    data ComputationError =
        InvalidState |
        InvalidValue |
        InvalidType
        deriving (Enum, Eq, Show)

    data ComputationResult a = SuccessfulComputation {
        _val :: a
    } | FailedComputation {
        _errors :: HS.HashSet ComputationError


    } deriving (Eq)

    type UnaryAction a b = a -> b
    type ErrableUnaryAction a b = a -> ComputationResult b
    type BinaryAction a b c = a -> b -> c
    type ErrableBinaryAction a b c = a -> b -> ComputationResult c
    type TernaryAction a b c d = a -> b -> c -> d
    type ErrableTernaryAction a b c d = a -> b -> c -> ComputationResult d

    instance H.Hashable ComputationError where
        hashWithSalt salt value = H.hashWithSalt salt (fromEnum value)

    instance Str.Stringifier ComputationError where
        stringify err = show err

    instance (Show a) => Show (ComputationResult a) where
        show result = _str result show show

    instance (Str.Stringifier a) => Str.Stringifier (ComputationResult a) where
        stringify result = _str result Str.stringify Str.stringify

    _str :: ComputationResult a -> (a -> String) -> (ComputationError -> String) -> String
    _str (SuccessfulComputation val) valueConverter _ = TP.printf "SuccessfulComputation { %s }" (valueConverter val)
    _str (FailedComputation errors) _ errorConverter = TP.printf "FailedComputation { %s }" finalString
        where errorList = F.toList errors
              stringList = fmap errorConverter errorList
              commaSeparated = L.intersperse "," stringList
              finalString = concat commaSeparated

    withValue :: a -> ComputationResult a
    withValue = SuccessfulComputation

    withError :: ComputationError -> ComputationResult a
    withError errorType = withErrorSet $ HS.singleton errorType

    withErrorSet :: HS.HashSet ComputationError -> ComputationResult a
    withErrorSet = FailedComputation

    convert :: ComputationResult a -> ComputationResult b
    convert (FailedComputation errors) = withErrorSet errors
    convert SuccessfulComputation{} = error "ComputationResult is valid"

    value :: ComputationResult a -> a
    value (SuccessfulComputation val) = val
    value FailedComputation{} = error "ComputationResult is invalid"

    errorSet :: ComputationResult a -> HS.HashSet ComputationError
    errorSet (FailedComputation errors) = errors
    errorSet SuccessfulComputation{} = HS.empty

    isSuccess :: ComputationResult a -> Bool
    isSuccess SuccessfulComputation{} = True
    isSuccess FailedComputation{} = False

    isFailure :: ComputationResult a -> Bool
    isFailure = not . isSuccess

    unResolve :: ComputationResult a -> UnaryAction a b -> ComputationResult b
    unResolve (SuccessfulComputation val) func = withValue $ func val
    unResolve fail@FailedComputation{} _ = convert fail

    binResolveLeft :: ComputationResult a -> b -> BinaryAction a b c -> ComputationResult c
    binResolveLeft (SuccessfulComputation leftVal) rightVal func = withValue $ func leftVal rightVal
    binResolveLeft fail@FailedComputation{} _ _ = convert fail

    binResolveRight :: a -> ComputationResult b -> BinaryAction a b c -> ComputationResult c
    binResolveRight left right func = binResolveLeft right left (flip func)

    errUnResolve :: ComputationResult a -> ErrableUnaryAction a b -> ComputationResult b
    errUnResolve (SuccessfulComputation val) func = func val
    errUnResolve fail@FailedComputation{} _ = convert fail

    errBinResolveLeft :: ComputationResult a -> b -> ErrableBinaryAction a b c -> ComputationResult c
    errBinResolveLeft (SuccessfulComputation leftVal) rightVal func = func leftVal rightVal
    errBinResolveLeft fail@FailedComputation{} _ _ = convert fail

    errBinResolveRight :: a -> ComputationResult b -> ErrableBinaryAction a b c -> ComputationResult c
    errBinResolveRight left right func = errBinResolveLeft right left (flip func)

    binCombine :: ComputationResult a -> ComputationResult b -> BinaryAction a b c -> ComputationResult c
    binCombine (SuccessfulComputation leftVal) (SuccessfulComputation rightVal) func = withValue $ func leftVal rightVal
    binCombine (FailedComputation leftErrors) (FailedComputation rightErrors) _ = withErrorSet unionErrors
        where unionErrors = HS.unions [leftErrors, rightErrors]
    binCombine leftFail@FailedComputation{} _ _ = convert leftFail
    binCombine _ rightFail@FailedComputation{} _ = convert rightFail

    errBinCombine :: ComputationResult a -> ComputationResult b -> ErrableBinaryAction a b c -> ComputationResult c
    errBinCombine (SuccessfulComputation leftVal) (SuccessfulComputation rightVal) func = func leftVal rightVal
    errBinCombine (FailedComputation leftErrors) (FailedComputation rightErrors) _ = withErrorSet unionErrors
        where unionErrors = HS.unions [leftErrors, rightErrors]
    errBinCombine leftFail@FailedComputation{} _ _ = convert leftFail
    errBinCombine _ rightFail@FailedComputation{} _ = convert rightFail

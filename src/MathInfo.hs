{-# LANGUAGE DeriveGeneric #-}

module MathInfo (
    Error(
        InvalidValue,
        InvalidType,
        InvalidState
    ),
    Result,
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

    data Error =
        InvalidState |
        InvalidValue |
        InvalidType
        deriving (Enum, Eq, Show)

    data Result a = Success {
        _val :: a
    } | Failure {
        _errors :: HS.HashSet Error
    } deriving (Eq)

    type UnaryAction a b = a -> b
    type ErrableUnaryAction a b = a -> Result b
    type BinaryAction a b c = a -> b -> c
    type ErrableBinaryAction a b c = a -> b -> Result c
    type TernaryAction a b c d = a -> b -> c -> d
    type ErrableTernaryAction a b c d = a -> b -> c -> Result d

    instance H.Hashable Error where
        hashWithSalt salt value = H.hashWithSalt salt (fromEnum value)

    instance Str.Stringifier Error where
        stringify err = show err

    instance (Show a) => Show (Result a) where
        show result = _str result show show

    instance (Str.Stringifier a) => Str.Stringifier (Result a) where
        stringify result = _str result Str.stringify Str.stringify

    _str :: Result a -> (a -> String) -> (Error -> String) -> String
    _str (Success val) valueConverter _ = TP.printf "Success { %s }" (valueConverter val)
    _str (Failure errors) _ errorConverter = TP.printf "Failure { %s }" finalString
        where errorList = F.toList errors
              stringList = fmap errorConverter errorList
              commaSeparated = L.intersperse "," stringList
              finalString = concat commaSeparated

    withValue :: a -> Result a
    withValue = Success

    withError :: Error -> Result a
    withError errorType = withErrorSet $ HS.singleton errorType

    withErrorSet :: HS.HashSet Error -> Result a
    withErrorSet = Failure

    convert :: Result a -> Result b
    convert (Failure errors) = withErrorSet errors
    convert Success{} = error "Result is valid"

    value :: Result a -> a
    value (Success val) = val
    value Failure{} = error "Result is invalid"

    errorSet :: Result a -> HS.HashSet Error
    errorSet (Failure errors) = errors
    errorSet Success{} = HS.empty

    isSuccess :: Result a -> Bool
    isSuccess Success{} = True
    isSuccess Failure{} = False

    isFailure :: Result a -> Bool
    isFailure = not . isSuccess

    unResolve :: Result a -> UnaryAction a b -> Result b
    unResolve (Success val) func = withValue $ func val
    unResolve fail@Failure{} _ = convert fail

    binResolveLeft :: Result a -> b -> BinaryAction a b c -> Result c
    binResolveLeft (Success leftVal) rightVal func = withValue $ func leftVal rightVal
    binResolveLeft fail@Failure{} _ _ = convert fail

    binResolveRight :: a -> Result b -> BinaryAction a b c -> Result c
    binResolveRight left right func = binResolveLeft right left (flip func)

    errUnResolve :: Result a -> ErrableUnaryAction a b -> Result b
    errUnResolve (Success val) func = func val
    errUnResolve fail@Failure{} _ = convert fail

    errBinResolveLeft :: Result a -> b -> ErrableBinaryAction a b c -> Result c
    errBinResolveLeft (Success leftVal) rightVal func = func leftVal rightVal
    errBinResolveLeft fail@Failure{} _ _ = convert fail

    errBinResolveRight :: a -> Result b -> ErrableBinaryAction a b c -> Result c
    errBinResolveRight left right func = errBinResolveLeft right left (flip func)

    binCombine :: Result a -> Result b -> BinaryAction a b c -> Result c
    binCombine (Success leftVal) (Success rightVal) func = withValue $ func leftVal rightVal
    binCombine (Failure leftErrors) (Failure rightErrors) _ = withErrorSet unionErrors
        where unionErrors = HS.unions [leftErrors, rightErrors]
    binCombine leftFail@Failure{} _ _ = convert leftFail
    binCombine _ rightFail@Failure{} _ = convert rightFail

    errBinCombine :: Result a -> Result b -> ErrableBinaryAction a b c -> Result c
    errBinCombine (Success leftVal) (Success rightVal) func = func leftVal rightVal
    errBinCombine (Failure leftErrors) (Failure rightErrors) _ = withErrorSet unionErrors
        where unionErrors = HS.unions [leftErrors, rightErrors]
    errBinCombine leftFail@Failure{} _ _ = convert leftFail
    errBinCombine _ rightFail@Failure{} _ = convert rightFail

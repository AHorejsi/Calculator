{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Actions (
    ComputationError(
        InvalidInput,
        DivideByZero,
        LogOfZero,
        NonsquareMatrix,
        DeterminantOfZero,
        NotInteger,
        NotReal,
        NotComplex,
        NegativeInput,
        NotComparable,
        NotConvertible,
        IndexOutOfBounds,
        UnequalDimensions
    ),
    Computation,
    UnaryAction,
    BinaryAction,
    TernaryAction,
    UnaryPredicate,
    BinaryPredicate,
    TernaryPredicate,
    ErrableUnaryAction,
    ErrableBinaryAction,
    ErrableTernaryAction,
    (.:),
    (.::),
    success,
    failure,
    isSuccess,
    isFailure,
    convert,
    value,
    asUnary,
    asBinary,
    asTernary,
    asErrableUnary,
    asErrableBinary,
    asErrableTernary,
    resolveUnary,
    resolveErrableUnary,
    resolveBinary,
    resolveErrableBinary,
    resolveTernary,
    resolveErrableTernary
) where
    import qualified Text.Printf as TP

    -- | Represents the reason for why a computation failed
    data ComputationError =
        -- | Indicates that the associated computation cannot accept negative values
        NegativeInput |
        -- | Indicates that the associated computation tried to divide by zero
        DivideByZero |
        -- | Indicates that the associated computation tried to compute the logarithm of zero
        LogOfZero |
        -- | Indicates that the associated computation cannot use a nonsquare matrix
        NonsquareMatrix |
        -- | Indicates that the associated computation cannot use a matrix with a determinant of zero
        DeterminantOfZero |
        -- | Indicates that the associated computation required an integer, but was provided something else
        NotInteger |
        -- | Indicates that the associated computation required a real number, but was provided something else
        NotReal |
        -- | Indicates that the associated computation required a complex number, but was provided something else
        NotComplex |
        -- | Indicates that the associated computation required a comparable type, but was provided something else
        NotComparable |
        -- | Indicates that the associated computation did not allow the conversion from one type to another
        NotConvertible |
        -- | Indicates that the associated computation received an index that was outside the bounds of the structure in question
        IndexOutOfBounds |
        -- | Indicates that the associated computation required multiple structures of the same dimensions, but received some that were not
        UnequalDimensions |
        -- | Indicates that the associated computation received input that was invalid for an unspecified reason
        InvalidInput
        deriving (Enum, Show)

    -- | Represents the result of a computation that may have succeeded or failed. Similar to
    -- | the 'Maybe' type except the failure type contains information about what went wrong
    data Computation a =
        -- | Represents a successful computation that produced valid output
        Success {
            -- | End result of a successful computation
            _val :: a
        } |
        -- | Represents a failed computation that could not be computed
        Failure {
            -- | Type of error that occurred during the computation
            _err :: ComputationError,
            -- | Description, in plain english, detailing the error
            _message :: String
        }

    instance (Show a) => Show (Computation a) where
        show comp
            | isSuccess comp = show $ _val comp
            | otherwise = TP.printf "ERROR: %s\n\t%s" errType errMsg
            where errType = show $ _err comp
                  errMsg = _message comp

    -- | Represents a computation that takes 1 input. Guaranteed to succeed
    type UnaryAction a b = a -> b
    -- | Represents a computation that takes 2 inputs. Guaranteed to succeed
    type BinaryAction a b c = a -> b -> c
    -- | Represents a computation that takes 3 inputs. Guaranteed to succeed
    type TernaryAction a b c d = a -> b -> c -> d

    -- | Represents a predicate function that takes 1 input
    type UnaryPredicate a = UnaryAction a Bool
    -- | Represents a predicate function that takes 2 inputs
    type BinaryPredicate a b = BinaryAction a b Bool
    -- | Represents a predicate function that takes 3 inputs
    type TernaryPredicate a b c = TernaryAction a b c Bool

    -- | Represents a computation that takes 1 input. Could fail on invalid inputs
    type ErrableUnaryAction a b = UnaryAction a (Computation b)
    -- | Represents a computation that takes 2 inputs. Could fail on invalid inputs
    type ErrableBinaryAction a b c = BinaryAction a b (Computation c)
    -- | Represents a computation that takes 3 inputs. Could fail on invalid inputs
    type ErrableTernaryAction a b c d = TernaryAction a b c (Computation d)

    class Addable a where
        plus :: a -> a -> Computation a

    class Subtractable a where
        minus :: a -> a -> Computation a

    class Multipliable a where
        multiply :: a -> a -> Computation a

    class Divisible a where
        divide :: a -> a -> Computation a


    -- | Function composition for 2-input functions
    (.:) :: UnaryAction c d -> BinaryAction a b c -> BinaryAction a b d
    (.:) f g = (f . ) . g

    -- | Function composition for 3-input functions
    (.::) :: UnaryAction d e -> TernaryAction a b c d -> TernaryAction a b c e
    (.::) f g = ((f . ) . ) . g

    -- | Constructs a successful 'Computation'
    success :: a -> Computation a
    success = Success

    -- | Constructs a failed 'Computation'
    failure :: ComputationError -> String -> Computation a
    failure = Failure

    -- | Converts a failed computation to another 'Computation' type
    -- | Throws exception if provided a successful computation
    convert :: Computation a -> Computation b
    convert (Failure err msg) = failure err msg
    convert _ = error "Computation is valid"

    -- | Retrieves the value from a successful 'Computation'
    -- | Throws exception if provided a failed computation
    value :: Computation a -> a
    value (Success val) = val
    value _ = error "Computation is not valid"

    -- | Determines if the given 'Computation' succeeded
    isSuccess :: Computation a -> Bool
    isSuccess Success{} = True
    isSuccess _ = False

    -- | Determines if the given 'Computation' failed
    isFailure :: Computation a -> Bool
    isFailure = not . isSuccess

    -- | Converts an errable unary action to a non-errable unary action
    -- | The non-errable action can throw an exception if an error occurs
    asUnary :: ErrableUnaryAction a b -> UnaryAction a b
    asUnary action = value . action

    -- | Converts an errable binary action to a non-errable binary action
    -- | The non-errable action can throw an exception if an error occurs
    asBinary :: ErrableBinaryAction a b c -> BinaryAction a b c
    asBinary action = value .: action

    -- | Converts an errable ternary action to a non-errable ternary action
    -- | The non-errable action can throw an exception if an error occurs
    asTernary :: ErrableTernaryAction a b c d -> TernaryAction a b c d
    asTernary action = value .:: action

    -- | Converts a non-errable unary action to an errable unary action
    asErrableUnary :: UnaryAction a b -> ErrableUnaryAction a b
    asErrableUnary action = success . action

    -- | Converts a non-errable binary action to an errable binary action
    asErrableBinary :: BinaryAction a b c -> ErrableBinaryAction a b c
    asErrableBinary action = success .: action

    -- | Converts a non-errable ternary action to an errable ternary action
    asErrableTernary :: TernaryAction a b c d -> ErrableTernaryAction a b c d
    asErrableTernary action = success .:: action

    -- | Applies the given unary action to the given 'Computation' if it was successful.
    -- | Otherwise, the failed 'Computation' is returned
    resolveUnary :: Computation a -> UnaryAction a b -> Computation b
    resolveUnary comp action = resolveErrableUnary comp (asErrableUnary action)

    -- | Applies the given errable unary action to the given 'Computation' if it was successful.
    -- | Otherwise, the failed 'Computation' is returned
    resolveErrableUnary :: Computation a -> ErrableUnaryAction a b -> Computation b
    resolveErrableUnary comp action
        | isFailure comp = convert comp
        | otherwise = action (_val comp)

    -- | Applies the given binary function to the given 'Computation' instances if they both were successful.
    -- | Otherwise, the leftmost failed 'Computation' is returned 
    resolveBinary :: Computation a -> Computation b -> BinaryAction a b c -> Computation c
    resolveBinary left right action = resolveErrableBinary left right (asErrableBinary action)

    -- | Applies the given errable binary function to the given 'Computation' instances if they both were successful.
    -- | Otherwise, the leftmost failed 'Computation' is returned
    resolveErrableBinary :: Computation a -> Computation b -> ErrableBinaryAction a b c -> Computation c
    resolveErrableBinary left right action
        | isFailure left = convert left
        | isFailure right = convert right
        | otherwise = action (_val left) (_val right)

    -- | Applies the given ternary function to the given 'Computation' instances if they all were successful.
    -- | Otherwise, the leftmost failed 'Computation' is returned
    resolveTernary :: Computation a -> Computation b -> Computation c -> TernaryAction a b c d -> Computation d
    resolveTernary first second third action = resolveErrableTernary first second third (asErrableTernary action)

    -- | Applies the given errable ternary function to the given 'Computation' instances if they all were successful.
    -- | Otherwise, the leftmost failed 'Computation' is returned
    resolveErrableTernary :: Computation a -> Computation b -> Computation c -> ErrableTernaryAction a b c d -> Computation d
    resolveErrableTernary first second third action
        | isFailure first = convert first
        | isFailure second = convert second
        | isFailure third = convert third
        | otherwise = action (_val first) (_val second) (_val third)

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module BigNumber (
    BigNumber,
    UnaryNumberAction,
    ErrableUnaryNumberAction,
    BinaryNumberAction,
    ErrableBinaryNumberAction,
    TernaryNumberAction,
    ErrableTernaryNumberAction,
    asNumber,
    asIntegral,
    real,
    complex,
    quaternion,
    isInteger,
    isReal,
    isComplex,
    isQuaternion,
    zeroValue,
    oneValue,
    twoValue,
    tenValue,
    negOneValue,
    halfValue,
    piValue,
    eValue,
    imagI,
    imagJ,
    imagK,
    realCoef,
    imag0Coef,
    imag1Coef,
    imag2Coef,
    plus,
    minus,
    multiply,
    negate,
    divide,
    inverse,
    absolute,
    normalize,
    power,
    squareRoot,
    naturalLogarithm,
    logarithm,
    exponential,
    modulo,
    remainder,
    integerDivide,
    gcd,
    lcm,
    factorial,
    choose,
    permutations,
    conjugate,
    arg,
    vectorPart,
    sine,
    cosine,
    tangent,
    sineHyperbolic,
    cosineHyperbolic,
    tangentHyperbolic,
    arcsine,
    arccosine,
    arctangent,
    arctangent2,
    arcsineHyperbolic,
    arccosineHyperbolic,
    arctangentHyperbolic,
    minVal,
    maxVal,
    roundUp,
    roundDown,
    roundOff,
    rounded,
    sigfig
) where
    import Prelude hiding (gcd, lcm, negate, print)
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.List as L
    import qualified Data.Maybe as M
    import qualified Data.Hashable as H
    import qualified Actions as A
    import qualified Stringify as P

    -- | Represents a scalar value
    data BigNumber a =
        -- | Represents a scalar value that is a real number
        BigReal {
            -- | The real coefficient of this real number
            _rreal :: a
        } |
        -- | Represents a scalar value that is a complex number
        BigComplex {
            -- | The real coefficient of this complex number
            _creal :: a,
            -- | The imaginary coefficient of this complex number
            _cimag0 :: a
        } |
        -- | Represents a scalar value that is a quaternion
        BigQuaternion {
            -- | The real coefficient of this quaternion
            _qreal :: a,
            -- | The first imaginary coefficient of this quaternion
            _qimag0 :: a,
            -- | The second imaginary coefficient of this quaternion
            _qimag1 :: a,
            -- | The third imaginary coefficient of this quaternion
            _qimag2 :: a
        }
        deriving (G.Generic, Eq)

    -- | Represents an action that takes in a 'BigNumber' and outputs a 'BigNumber'
    type UnaryNumberAction n = A.UnaryAction (BigNumber n) (BigNumber n)
    -- | Represents an errable action that takes in a 'BigNumber' and outputs a 'BigNumber'
    type ErrableUnaryNumberAction n = A.ErrableUnaryAction (BigNumber n) (BigNumber n)
    -- | Represents an action that takes in 2 instances of 'BigNumber' and outputs a 'BigNumber'
    type BinaryNumberAction n = A.BinaryAction (BigNumber n) (BigNumber n) (BigNumber n)
    -- | Represents an errable action that takes in 2 instances of 'BigNumber' and outputs a 'BigNumber'
    type ErrableBinaryNumberAction n = A.ErrableBinaryAction (BigNumber n) (BigNumber n) (BigNumber n)
    -- | Represents an action that takes in 3 instances of 'BigNumber' and outputs a 'BigNumber'
    type TernaryNumberAction n = A.TernaryAction (BigNumber n) (BigNumber n) (BigNumber n) (BigNumber n)
    -- | Represents an action that takes in 3 instances of 'BigNumber' and outputs a 'BigNumber'
    type ErrableTernaryNumberAction n = A.ErrableTernaryAction (BigNumber n) (BigNumber n) (BigNumber n) (BigNumber n)

    instance (H.Hashable a) => H.Hashable (BigNumber a) where
        hashWithSalt salt (BigReal realVal) = H.hashWithSalt salt realVal
        hashWithSalt salt (BigComplex realVal imag0Val) = imag0Hash
            where realHash = H.hashWithSalt salt realVal
                  imag0Hash = H.hashWithSalt realHash imag0Val
        hashWithSalt salt (BigQuaternion realVal imag0Val imag1Val imag2Val) = imag2Hash
            where realHash = H.hashWithSalt salt realVal
                  imag0Hash = H.hashWithSalt realHash imag0Val
                  imag1Hash = H.hashWithSalt imag0Hash imag1Val
                  imag2Hash = H.hashWithSalt imag1Hash imag2Val

    instance (Show a) => Show (BigNumber a) where
        show (BigReal realVal) = TP.printf "Real(%s)" (show realVal)
        show (BigComplex realVal imag0Val) = TP.printf "Complex(%s,%s)" (show realVal) (show imag0Val)
        show (BigQuaternion realVal imag0Val imag1Val imag2Val) = TP.printf "Quaternion(%s,%s,%s,%s)" (show realVal) (show imag0Val) (show imag1Val) (show imag2Val)

    -- | Returns the 'String', which represents a real number, with its sign in front
    _signedStr :: String -> String
    _signedStr val
        | '-' == (head val) = val
        | otherwise = '+' : val

    -- | Converts an 'Integral' to a 'BigNumber'
    asNumber :: (Integral a, Num b, Eq b) => a -> BigNumber b
    asNumber = real . fromIntegral

    -- | Converts a 'BigNumber' to an 'Integral'. Fails if the given 'BigNumber' does not represent a valid integer
    asIntegral :: (Integral a, RealFrac b) => BigNumber b -> a
    asIntegral (BigReal realVal)
        | intPart == realVal = floorVal
        | otherwise = error "Input is not in the integer set"
        where floorVal = floor realVal
              intPart = fromIntegral floorVal
    asIntegral _ = error "Input is not in the integer set"

    -- | Constructs a real 'BigNumber'
    real :: (Num a, Eq a) => a -> BigNumber a
    real = BigReal

    -- | Constructs a complex 'BigNumber'
    complex :: (Num a, Eq a) => a -> a -> BigNumber a
    complex realVal 0 = real realVal
    complex realVal imag0Val = BigComplex realVal imag0Val

    -- | Constructs a quaternion 'BigNumber'
    quaternion :: (Num a, Eq a) => a -> a -> a -> a -> BigNumber a
    quaternion realVal 0 0 0 = real realVal
    quaternion realVal imag0Val 0 0 = complex realVal imag0Val
    quaternion realVal imag0Val imag1Val imag2Val = BigQuaternion realVal imag0Val imag1Val imag2Val

    -- | Checks if the underlying type of the given 'BigNumber' is 'BigReal'
    _isExactReal :: BigNumber a -> Bool
    _isExactReal BigReal{} = True
    _isExactReal _ = False

    -- | Checks if the underlying type of the given 'BigNumber' is 'BigComplex'
    _isExactComplex :: BigNumber a -> Bool
    _isExactComplex BigComplex{} = True
    _isExactComplex _ = False

    -- | Checks if the underlying type of the given 'BigNumber' is 'BigQuaternion'
    _isExactQuaternion :: BigNumber a -> Bool
    _isExactQuaternion BigQuaternion{} = True
    _isExactQuaternion _ = False

    -- | Checks if the given 'BigNumber' falls within the set of all integers
    isInteger :: (RealFrac a) => BigNumber a -> Bool
    isInteger (BigReal realVal) = realVal == floorVal
        where floorVal = fromIntegral $ floor realVal
    isInteger _ = False

    -- | Checks if this 'BigNumber' falls within the set of all real numbers
    isReal :: (RealFrac a) => BigNumber a -> Bool
    isReal = _isExactReal

    -- | Checks if the given 'BigNumber' falls within the set of all complex numbers
    isComplex :: (RealFrac a) => BigNumber a -> Bool
    isComplex val = (isReal val) || (_isExactComplex val)

    -- | Checks if the given 'BigNumber' falls within the set of all quaternions
    isQuaternion :: (RealFrac a) => BigNumber a -> Bool
    isQuaternion val = (isComplex val) || (_isExactQuaternion val)

    -- | The constant zeroValue as a 'BigNumber'
    zeroValue :: (Num a) => BigNumber a
    zeroValue = BigReal 0

    -- | The constant oneValue as a 'BigNumber'
    oneValue :: (Num a) => BigNumber a
    oneValue = BigReal 1

    -- | The constant twoValue as a 'BigNumber'
    twoValue :: (Num a) => BigNumber a
    twoValue = BigReal 2

    -- | The constant tenValue as a 'BigNumber'
    tenValue :: (Num a) => BigNumber a
    tenValue = BigReal 10

    -- | The constant negative oneValue as a 'BigNumber'
    negOneValue :: (Num a) => BigNumber a
    negOneValue = BigReal $ -1

    -- | The constant oneValue-halfValue as a 'BigNumber'
    halfValue :: (Fractional a) => BigNumber a
    halfValue = BigReal 0.5

    -- | The constant pi as a 'BigNumber'
    piValue :: (Floating a) => BigNumber a
    piValue = BigReal pi

    -- | The constant e as a 'BigNumber'
    eValue :: (Floating a) => BigNumber a
    eValue = BigReal $ exp 1

    -- | The constant i as a 'BigNumber'
    imagI :: (Num a) => BigNumber a
    imagI = BigComplex 0 1

    -- | The constant j as a 'BigNumber'
    imagJ :: (Num a) => BigNumber a
    imagJ = BigQuaternion 0 0 1 0

    -- | The constant k as a 'BigNumber'
    imagK :: (Num a) => BigNumber a
    imagK = BigQuaternion 0 0 0 1

    -- | Gets the real coefficient as its generic type
    _realPart :: BigNumber a -> a
    _realPart (BigReal realVal) = realVal
    _realPart (BigComplex realVal _) = realVal
    _realPart (BigQuaternion realVal _ _ _) = realVal

    -- | Gets the imaginary coefficient as its generic type
    _imag0Part ::(Num a) => BigNumber a -> a
    _imag0Part BigReal{} = 0
    _imag0Part (BigComplex _ imag0Val) = imag0Val
    _imag0Part (BigQuaternion _ imag0Val _ _) = imag0Val

    -- | Gets the real coefficient
    realCoef :: (Num a, Eq a) => BigNumber a -> BigNumber a
    realCoef (BigReal realVal) = real realVal
    realCoef (BigComplex realVal _) = real realVal
    realCoef (BigQuaternion realVal _ _ _) = real realVal

    -- | Gets the coefficient in front of 'i'
    imag0Coef :: (Num a, Eq a) => BigNumber a -> BigNumber a
    imag0Coef BigReal{} = zeroValue
    imag0Coef (BigComplex _ imag0Val) = real imag0Val
    imag0Coef (BigQuaternion _ imag0Val _ _) = real imag0Val

    -- | Gets the coefficient in front of 'j'
    imag1Coef :: (Num a, Eq a) => BigNumber a -> BigNumber a
    imag1Coef BigReal{} = zeroValue
    imag1Coef BigComplex{} = zeroValue
    imag1Coef (BigQuaternion _ _ imag1Val _) = real imag1Val

    -- | Gets the coefficient in front of 'k'
    imag2Coef :: (Num a, Eq a) => BigNumber a -> BigNumber a
    imag2Coef BigReal{} = zeroValue
    imag2Coef BigComplex{} = zeroValue
    imag2Coef (BigQuaternion _ _ _ imag2Val) = real imag2Val

    -- | Converts the given 'BigReal' to a mathematically equal 'BigComplex'. If the given value is not a real number, then throws error
    _forceComplex :: (Num a) => BigNumber a -> BigNumber a
    _forceComplex (BigReal realVal) = BigComplex realVal 0
    _forceComplex _ = error "Not a real number"

    -- | Adds the two given numbers together
    plus :: (Num a, Eq a) => BigNumber a -> BigNumber a -> BigNumber a
    plus (BigReal leftReal) (BigReal rightReal) = real $ leftReal + rightReal
    plus (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) rightImag0
    plus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
    plus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) (rightReal + rightImag0)
    plus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
    plus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
    plus left right = plus right left

    -- | Subtracts the first argument by the second argument
    minus :: (Num a, Eq a) => BigNumber a -> BigNumber a -> BigNumber a
    minus (BigReal leftReal) (BigReal rightReal) = real $ leftReal - rightReal
    minus (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal - rightReal) (-rightImag0)
    minus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)
    minus (BigComplex leftReal leftImag0) (BigReal rightReal) = complex (leftReal - rightReal) leftImag0
    minus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex (leftReal - rightReal) (leftImag0 - rightImag0)
    minus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)
    minus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigReal rightReal) = quaternion (leftReal - rightReal) leftImag0 leftImag1 leftImag2
    minus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2
    minus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)

    -- | Multiplies the two given numbers together
    multiply :: (Num a, Eq a) => BigNumber a -> BigNumber a -> BigNumber a
    multiply (BigReal leftReal) (BigReal rightReal) = real $ leftReal * rightReal
    multiply (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal * rightReal) (leftReal * rightImag0)
    multiply (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)
    multiply (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex realResult imag0Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
    multiply (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftReal * rightImag1 - leftImag0 * rightImag2
              imag2Result = leftReal * rightImag2 + leftImag0 * rightImag1
    multiply (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftImag1 * rightReal + leftImag2 * rightImag0
              imag2Result = -leftImag1 * rightImag0 + leftImag2 * rightReal
    multiply (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1
              imag1Result = leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0
              imag2Result = leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal
    multiply left right = multiply right left

    -- | Multiplies the given number bby negative oneValue
    negate :: (Num a, Eq a) => BigNumber a -> BigNumber a
    negate = multiply negOneValue

    -- | Divides the first argument by the second argument
    divide :: (Fractional a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    divide _ (BigReal 0) = A.failure A.DivideByZero "Cannot divide by zero"
    divide (BigReal leftReal) (BigReal rightReal) = (A.success . real) $ leftReal / rightReal
    divide left right@BigComplex{} = divide numerator denominator
        where rightConj = conjugate right
              numerator = multiply left rightConj
              denominator = multiply right rightConj
    divide (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = A.success $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = leftReal * rightReal / denominator
              imag0Result = -leftReal * rightImag0 / denominator
              imag1Result = -leftReal * rightImag1 / denominator
              imag2Result = -leftReal * rightImag2 / denominator
    divide (BigComplex leftReal leftImag0) (BigReal rightReal) = A.success $ complex (leftReal / rightReal) (leftImag0 / rightReal)
    divide (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = A.success $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
              imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
              imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
              imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator
    divide (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigReal rightReal) = A.success $ quaternion (leftReal / rightReal) (leftImag0 / rightReal) (leftImag1 / rightReal) (leftImag2 / rightReal)
    divide (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = A.success $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
              imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
              imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
              imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator

    -- | Computes the multiplicative inverse of the given 'BigNumber'
    inverse :: (Fractional a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    inverse (BigReal 0) = A.failure A.DivideByZero "Zero does not have an inverse"
    inverse (BigReal realVal) = A.success $ real $ 1 / realVal
    inverse (BigComplex realVal imag0Val) = A.success $ complex (realVal / denominator) (-imag0Val / denominator)
        where denominator = (realVal ^^ 2) + (imag0Val ^^ 2)
    inverse quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = divide conjVal denominator
        where conjVal = conjugate quat
              denominator = real $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val

    -- | Computes the distance of the given 'BigNumber' from the origin
    absolute :: (Floating a, Eq a) => BigNumber a -> BigNumber a
    absolute (BigReal realVal) = real $ abs realVal
    absolute (BigComplex realVal imag0Val) = (real . sqrt) $ realVal * realVal + imag0Val * imag0Val
    absolute (BigQuaternion realVal imag0Val imag1Val imag2Val) = (real . sqrt) $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val

    normalize :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    normalize val = divide val (absolute val)

    -- | Computes the result of a binary action that takes two integers as input
    _binaryIntAction :: (Integral a, RealFrac b) => A.BinaryAction a a a -> BigNumber b -> BigNumber b -> A.Computation (BigNumber b)
    _binaryIntAction action left@BigReal{} right@BigReal{}
        | not $ (isInteger left) && (isInteger right) = A.failure A.NotInteger "Given action is only possible on integers"
        | otherwise = (A.success . asNumber) $ action leftInt rightInt
        where leftInt = asIntegral left
              rightInt = asIntegral right
    _binaryIntAction _ _ _ = A.failure A.NotInteger "Given action is only possible on integers"
    
    -- | Computes the modulo of the given numbers. Only works if both arguments are integers
    modulo :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    modulo _ (BigReal 0) = A.failure A.DivideByZero "Modulo by 0 is uncomputable"
    modulo left right
        | A.isFailure result = A.failure A.NotInteger "Operator mod is only possible on integers"
        | otherwise = result
        where result = _binaryIntAction mod left right

    -- | Computes the remainder of dividing the first argument by the second argument
    remainder :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    remainder _ (BigReal 0) = A.failure A.DivideByZero "Division by 0 is uncomputable, therefore there is no remainder for any division by 0"
    remainder left right
        | A.isFailure result = A.failure A.NotInteger "Function rem is only possible on integers"
        | otherwise = result
        where result = _binaryIntAction rem left right

    -- | Performs integer division on the given arguments. Only works if both arguments are integers
    integerDivide :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    integerDivide _ (BigReal 0) = A.failure A.DivideByZero "Integer division by zeroValue is uncomputable"
    integerDivide left right
        | A.isFailure result = A.failure A.NotInteger "Integer division is only possible on integers"
        | otherwise = result
        where result = _binaryIntAction div left right

    -- | Computes the greatest common divisor of the given arguments. Only works if both arguments are integers. Does not work if both arguments are zero
    gcd :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    gcd (BigReal 0) (BigReal 0) = A.failure A.InvalidInput "(0, 0) has no greatest common divisor because all integers are valid divisors"
    gcd first@BigReal{} second@BigReal{}
        | not $ (isInteger first) && (isInteger second) = A.failure A.NotInteger "Function gcd only works on integers"
        | otherwise = (A.success . asNumber) $ _gcdHelper firstInt secondInt
        where firstInt = asIntegral first
              secondInt = asIntegral second
    gcd _ _ = A.failure A.NotInteger "Function gcd only applies to integers"

    -- | Helper function for gcd function
    _gcdHelper :: (Integral a, Eq a) => a -> a -> a
    _gcdHelper first second
        | 0 == second = first
        | 0 == first = second
        | otherwise = _gcdHelper second (mod first second)

    -- | Computes the least common multiple of both arguments. Only works if both arguments are integers
    lcm :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    lcm (BigReal 0) (BigReal 0) = A.success zeroValue
    lcm left right = A.resolveErrableBinary prod gcdVal integerDivide
        where prod = A.success $ multiply left right
              gcdVal = gcd left right

    -- | Computes the factorial of the argument. Only works on on nonnegative integers
    factorial :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    factorial real@BigReal{}
        | not $ isInteger real = A.failure A.NotInteger "Only nonnegative integers have factorials"
        | int < 0 = A.failure A.NegativeInput "Only nonnegative integers have factorials"
        | otherwise = (A.success . asNumber) $ _factorialHelper int
        where int = asIntegral real
    factorial _ = A.failure A.NotInteger "Only integers have factorials"
    
    -- | Helper function for 'factorial' function
    _factorialHelper :: (Integral a) => a -> a
    _factorialHelper 0 = 1
    _factorialHelper intVal = intVal * (_factorialHelper $ intVal - 1)

    choose :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    choose choices picks = A.resolveErrableBinary a d integerDivide
        where a = factorial choices
              b = factorial picks
              c = factorial $ minus choices picks
              d = A.resolveBinary b c multiply

    permutations :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    permutations choices picks = A.resolveErrableBinary a b integerDivide
        where a = factorial choices
              b = factorial $ minus choices picks

    power :: (RealFloat a) => BigNumber a -> BigNumber a -> BigNumber a
    power (BigReal 0) (BigReal 0) = oneValue
    power (BigReal 0) _ = zeroValue
    power left right = exponential $ multiply (A.value $ naturalLogarithm left) right

    squareRoot :: (RealFloat a) => BigNumber a -> BigNumber a
    squareRoot = (`power` halfValue)

    naturalLogarithm :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    naturalLogarithm (BigReal 0) = A.failure A.LogOfZero "Logarithm of zero is uncomputable"
    naturalLogarithm (BigReal realVal)
        | realVal < 0 = A.success $ complex (log $ -realVal) pi
        | otherwise = (A.success . real) $ log realVal
    naturalLogarithm com@BigComplex{} = A.success $ complex realPart imag0Part
        where realPart = (log . _rreal) $ absolute com
              imag0Part = (_rreal . A.value) $ arg com
    naturalLogarithm quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = A.resolveBinary f e plus
        where a = vectorPart quat
              b = absolute quat
              c = A.value $ divide (real realVal) b
              d = A.value $ arccosine c
              e = A.success $ multiply (A.value $ normalize a) d
              f = naturalLogarithm b

    logarithm :: (RealFloat a, Ord a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    logarithm base val = A.resolveErrableBinary valArg baseArg divide
        where baseArg = naturalLogarithm base
              valArg = naturalLogarithm val

    logarithm10 :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    logarithm10 = logarithm tenValue

    logarithm2 :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    logarithm2 = logarithm twoValue
    
    exponential :: (Floating a, Eq a) => BigNumber a -> BigNumber a
    exponential (BigReal 0) = oneValue
    exponential (BigReal realVal) = real $ exp realVal
    exponential (BigComplex realVal imag0Val) = multiply (exponential $ real realVal) (complex (cos imag0Val) (sin imag0Val))
    exponential quat@BigQuaternion{} = multiply e d
        where a = vectorPart quat
              b = absolute quat
              c = multiply (A.value $ normalize a) (A.value $ sine b)
              d = plus (A.value $ cosine b) c
              e = exponential $ realCoef quat

    conjugate :: (Num a, Eq a) => BigNumber a -> BigNumber a
    conjugate val@BigReal{} = val
    conjugate (BigComplex realVal imag0Val) = complex realVal (-imag0Val)
    conjugate (BigQuaternion realVal imag0Val imag1Val imag2Val) = quaternion realVal (-imag0Val) (-imag1Val) (-imag2Val)

    arg :: (RealFloat a) => BigNumber a -> A.Computation (BigNumber a)
    arg BigReal{} = A.success zeroValue
    arg (BigComplex 0 imag0Val)
        | imag0Val < 0 = A.success $ multiply (negate piValue) halfValue
        | imag0Val > 0 = A.success $ multiply piValue halfValue
    arg (BigComplex realVal imag0Val) = (A.success . real) $ atan2 imag0Val realVal
    arg _ = A.failure A.NotComplex "Complex argument can only be applied to scalars that are in the set of all complex numbers"

    vectorPart :: (Num a, Eq a) => BigNumber a -> BigNumber a
    vectorPart BigReal{} = zeroValue
    vectorPart (BigComplex _ imag0Val) = complex 0 imag0Val
    vectorPart (BigQuaternion _ imag0Val imag1Val imag2Val) = quaternion 0 imag0Val imag1Val imag2Val

    sine :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    sine (BigReal realVal) = (A.success . real) $ sin realVal
    sine (BigComplex realVal imag0Val) = A.success $ complex ((sin realVal) * (cosh imag0Val)) ((cos realVal) * (sinh imag0Val))
    sine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    cosine :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    cosine (BigReal realVal) = (A.success . real) $ cos realVal
    cosine (BigComplex realVal imag0Val) = A.success $ complex ((cos realVal) * (cosh imag0Val)) (-(sin realVal) * (sinh imag0Val))
    cosine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    tangent :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    tangent val = A.resolveErrableBinary sinValue cosValue divide
        where sinValue = sine val
              cosValue = cosine val

    sineHyperbolic :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    sineHyperbolic (BigReal realVal) = (A.success . real) $ sinh realVal
    sineHyperbolic (BigComplex realVal imag0Val) = A.success $ complex ((sinh realVal) * (cos imag0Val)) ((cosh realVal) * (sin imag0Val))
    sineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    cosineHyperbolic :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    cosineHyperbolic (BigReal realVal) = (A.success . real) $ cosh realVal
    cosineHyperbolic (BigComplex realVal imag0Val) = A.success $ complex ((cosh realVal) * (cos imag0Val)) ((sinh realVal) * (sin imag0Val))
    cosineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    tangentHyperbolic :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    tangentHyperbolic val = A.resolveErrableBinary sinhValue coshValue divide
        where sinhValue = sineHyperbolic val
              coshValue = cosineHyperbolic val

    arcsine :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arcsine val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . real) $ asin realVal
        | otherwise = arcsine $ _forceComplex val
    arcsine com@BigComplex{} = A.success c
        where a = minus oneValue (multiply com com)
              b = plus (multiply imagI com) (squareRoot a)
              c = multiply (negate imagI) (A.value $ naturalLogarithm b)
    arcsine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arccosine :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arccosine val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . real) $ acos realVal
        | otherwise = arccosine $ _forceComplex val
    arccosine com@BigComplex{} = A.resolveBinary leftArg rightArg multiply
        where a = minus (multiply com com) oneValue
              b = plus com (squareRoot a)
              leftArg = A.success $ negate imagI
              rightArg = naturalLogarithm b
    arccosine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arctangent :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arctangent val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . real) $ atan realVal
        | otherwise = arctangent $ _forceComplex val
    arctangent com@BigComplex{} = A.success $ multiply e d
        where a = multiply imagI com
              b = plus oneValue a
              c = minus oneValue a
              d = minus (A.value $ naturalLogarithm c) (A.value $ naturalLogarithm b)
              e = multiply imagI halfValue
    arctangent _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arctangent2 :: (RealFloat a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    arctangent2 (BigReal leftReal) (BigReal rightReal) = (A.success . real) $ atan2 leftReal rightReal
    arctangent2 _ _ = A.failure A.NotComplex "Function arctan2 can only be applied to real numbers"

    arcsineHyperbolic :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arcsineHyperbolic val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . real) $ asinh realVal
        | otherwise = arcsineHyperbolic $ _forceComplex val
    arcsineHyperbolic com@BigComplex{} = A.resolveErrableBinary asinValue rightArg divide
        where asinValue = arcsine $ multiply imagI com
              rightArg = A.success imagI
    arcsineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arccosineHyperbolic :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arccosineHyperbolic val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . real) $ acosh realVal
        | otherwise = arccosineHyperbolic $ _forceComplex val
    arccosineHyperbolic com@BigComplex{} = A.resolveBinary leftArg acosValue multiply
        where acosValue = arccosine com
              leftArg = A.success imagI
    arccosineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arctangentHyperbolic :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arctangentHyperbolic val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . real) $ atanh realVal
        | otherwise = arctangentHyperbolic $ _forceComplex val
    arctangentHyperbolic com@BigComplex{} = A.resolveErrableBinary atanValue rightArg divide
        where atanValue = arctangent $ multiply imagI com
              rightArg = A.success imagI
    arctangentHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    minVal :: (Ord a, Num a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    minVal (BigReal leftReal) (BigReal rightReal) = (A.success . real) $ min leftReal rightReal
    minVal _ _ = A.failure A.NotComparable "Real numbers are the only scalars that have a total ordering and therefore have a minimum value"

    maxVal :: (Ord a, Num a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    maxVal (BigReal leftReal) (BigReal rightReal) = (A.success . real) $ max leftReal rightReal
    maxVal _ _ = A.failure A.NotComparable "Real numbers are the only scalars that have a total ordering and therefore have a maximum value"

    roundDown :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    roundDown (BigReal realVal) = (A.success . asNumber) $ floor realVal
    roundDown _ = A.failure A.NotReal "Only real numbers can be rounded up/down/off to the nearest integer"

    roundUp :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    roundUp (BigReal realVal) = (A.success . asNumber) $ ceiling realVal
    roundUp _ = A.failure A.NotReal "Only real numbers can be rounded up/down/off to the nearest integer"

    roundOff :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    roundOff = roundDown . (plus halfValue)

    rounded :: (RealFrac a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    rounded val places@BigReal{}
        | not $ isInteger places = A.failure A.NotInteger "Number of decimal places must be an integer"
        | placesInt < 0 = A.failure A.NegativeInput "Cannot round to a negative number of decimal places"
        | isInteger val = A.success val
        | otherwise = A.success $ _fromList roundedList
        where placesInt = asIntegral places
              list = _toList val
              roundedList = fmap (_roundedHelper placesInt) list
    rounded _ _ = A.failure A.NotInteger "The number of decimal places to round to must be a nonnegative integer"

    sigfig :: (RealFrac a, Floating a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    sigfig val amount@BigReal{}
        | not $ isInteger amount = A.failure A.NotInteger "Number of significant figures must be an integer"
        | amountInt < 1 = A.failure A.NegativeInput "Cannot round to nonpositive number of significant figures"
        | otherwise = A.success $ _fromList sigfigList
        where amountInt = asIntegral amount
              list = _toList val
              sigfigList = fmap (_sigfigHelper amountInt) list
    sigfig _ _ = A.failure A.NotInteger "The number of significant figures to round to must be a nonnegative integer"

    _toList :: (Num a, Eq a) => BigNumber a -> [a]
    _toList (BigReal realVal) = [realVal]
    _toList (BigComplex realVal imag0Val) = [realVal, imag0Val]
    _toList (BigQuaternion realVal imag0Val imag1Val imag2Val) = [realVal, imag0Val, imag1Val, imag2Val]

    _fromList :: (Num a, Eq a) => [a] -> BigNumber a
    _fromList [realVal] = real realVal
    _fromList [realVal, imag0Val] = complex realVal imag0Val
    _fromList [realVal, imag0Val, imag1Val, imag2Val] = quaternion realVal imag0Val imag1Val imag2Val
    _fromList _ = error "Invalid list length"

    _roundedHelper :: (Integral a, RealFrac b) => a -> b -> b
    _roundedHelper places val = numerator / powerOfTen
        where powerOfTen = fromIntegral $ 10 ^ places
              numerator = (fromIntegral . round) $ val * powerOfTen

    _sigfigHelper :: (Integral a, RealFrac b, Floating b) => a -> b -> b
    _sigfigHelper amount val
        | intLength >= amount = leastSigfigsToRemove * ((fromIntegral . round) $ val / leastSigfigsToRemove)
        | otherwise = _roundedHelper (amount - intLength) val
        where intLength = floor $ (logBase 10 (abs val)) + 1
              leastSigfigsToRemove = 10 ^ (intLength - amount)

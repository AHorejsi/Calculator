{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
    makeReal,
    makeComplex,
    makeQuaternion,
    isExactReal,
    isExactComplex,
    isExactQuaternion,
    isInteger,
    isReal,
    isComplex,
    isQuaternion,
    zero,
    one,
    two,
    ten,
    negOne,
    half,
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
    perm,
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
    less,
    greater,
    lessEqual,
    greaterEqual,
    roundUp,
    roundDown,
    roundOff,
    rounded,
    sigfig,
    isEven,
    isOdd
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
        } deriving (G.Generic)

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

    instance (Eq a) => Eq (BigNumber a) where
        (==) (BigReal leftReal) (BigReal rightReal) = leftReal == rightReal
        (==) (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = leftReal == rightReal && leftImag0 == rightImag0
        (==) (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = leftReal == rightReal && leftImag0 == rightImag0 && leftImag1 == rightImag1 && leftImag2 == rightImag2
        (==) _ _ = False

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
        show val@(BigReal realVal) = TP.printf "Real(%s)" (show realVal)
        show (BigComplex realVal imag0Val) = TP.printf "Complex(%s,%s)" (show realVal) (show imag0Val)
        show (BigQuaternion realVal imag0Val imag1Val imag2Val) = TP.printf "Quaternion(%s,%s,%s,%s)" (show realVal) (show imag0Val) (show imag1Val) (show imag2Val)

    instance (Show a) => P.Stringifier (BigNumber a) where
        stringify val = realStr ++ signedImagStr
            where valList = _list val
                  strList = fmap show valList
                  imagAxes = ["i", "j", "k"]
                  realStr = head strList
                  imagStr = tail strList
                  imagStrWithAxes = zipWith (++) imagStr imagAxes
                  signedImagStr = concatMap _signedStr imagStrWithAxes

    _list :: BigNumber a -> [a]
    _list (BigReal realVal) = [realVal]
    _list (BigComplex realVal imag0Val) = [realVal, imag0Val]
    _list (BigQuaternion realVal imag0Val imag1Val imag2Val) = [realVal, imag0Val, imag1Val, imag2Val]

    _signedStr :: String -> String
    _signedStr val
        | '-' == (head val) = val
        | otherwise = '+' : val

    asNumber :: (Integral a, Num b) => a -> BigNumber b
    asNumber = makeReal . fromIntegral

    asIntegral :: (Integral a, RealFrac b) => BigNumber b -> a
    asIntegral (BigReal realVal)
        | intPart == realVal = floorVal
        | otherwise = error "Input is not in the integer set"
        where floorVal = floor realVal
              intPart = fromIntegral floorVal
    asIntegral _ = error "Input is not in the real set"

    makeReal :: a -> BigNumber a
    makeReal = BigReal

    makeComplex :: (Num a, Eq a) => a -> a -> BigNumber a
    makeComplex realVal 0 = makeReal realVal
    makeComplex realVal imag0Val = BigComplex realVal imag0Val

    makeQuaternion :: (Num a, Eq a) => a -> a -> a -> a -> BigNumber a
    makeQuaternion realVal 0 0 0 = makeReal realVal
    makeQuaternion realVal imag0Val 0 0 = makeComplex realVal imag0Val
    makeQuaternion realVal imag0Val imag1Val imag2Val = BigQuaternion realVal imag0Val imag1Val imag2Val

    isExactReal :: BigNumber a -> Bool
    isExactReal BigReal{} = True
    isExactReal _ = False

    isExactComplex :: BigNumber a -> Bool
    isExactComplex BigComplex{} = True
    isExactComplex _ = False

    isExactQuaternion :: BigNumber a -> Bool
    isExactQuaternion BigQuaternion{} = True
    isExactQuaternion _ = False

    isInteger :: (RealFrac a) => BigNumber a -> Bool
    isInteger (BigReal realVal) = realVal == floorVal
        where floorVal = fromIntegral $ floor realVal
    isInteger _ = False

    isReal :: (RealFrac a) => BigNumber a -> Bool
    isReal val = (isInteger val) || (isExactReal val)

    isComplex :: (RealFrac a) => BigNumber a -> Bool
    isComplex val = (isReal val) || (isExactComplex val)

    isQuaternion :: (RealFrac a) => BigNumber a -> Bool
    isQuaternion val = (isComplex val) || (isExactQuaternion val)

    zero :: (Num a) => BigNumber a
    zero = BigReal 0

    one :: (Num a) => BigNumber a
    one = BigReal 1

    two :: (Num a) => BigNumber a
    two = BigReal 2

    ten :: (Num a) => BigNumber a
    ten = BigReal 10

    negOne :: (Num a) => BigNumber a
    negOne = BigReal $ -1

    half :: (Fractional a) => BigNumber a
    half = BigReal 0.5

    piValue :: (Floating a) => BigNumber a
    piValue = BigReal pi

    eValue :: (Floating a) => BigNumber a
    eValue = BigReal $ exp 1

    imagI :: (Num a) => BigNumber a
    imagI = BigComplex 0 1

    imagJ :: (Num a) => BigNumber a
    imagJ = BigQuaternion 0 0 1 0

    imagK :: (Num a) => BigNumber a
    imagK = BigQuaternion 0 0 0 1

    _realPart :: BigNumber a -> a
    _realPart (BigReal realVal) = realVal
    _realPart (BigComplex realVal _) = realVal
    _realPart (BigQuaternion realVal _ _ _) = realVal

    _imag0Part ::(Num a) => BigNumber a -> a
    _imag0Part BigReal{} = 0
    _imag0Part (BigComplex _ imag0Val) = imag0Val
    _imag0Part (BigQuaternion _ imag0Val _ _) = imag0Val

    realCoef :: BigNumber a -> BigNumber a
    realCoef (BigReal realVal) = makeReal realVal
    realCoef (BigComplex realVal _) = makeReal realVal
    realCoef (BigQuaternion realVal _ _ _) = makeReal realVal

    imag0Coef :: (Num a) => BigNumber a -> BigNumber a
    imag0Coef BigReal{} = zero
    imag0Coef (BigComplex _ imag0Val) = makeReal imag0Val
    imag0Coef (BigQuaternion _ imag0Val _ _) = makeReal imag0Val

    imag1Coef :: (Num a) => BigNumber a -> BigNumber a
    imag1Coef BigReal{} = zero
    imag1Coef BigComplex{} = zero
    imag1Coef (BigQuaternion _ _ imag1Val _) = makeReal imag1Val

    imag2Coef :: (Num a) => BigNumber a -> BigNumber a
    imag2Coef BigReal{} = zero
    imag2Coef BigComplex{} = zero
    imag2Coef (BigQuaternion _ _ _ imag2Val) = makeReal imag2Val

    _forceComplex :: (Num a) => BigNumber a -> BigNumber a
    _forceComplex (BigReal realVal) = BigComplex realVal 0
    _forceComplex _ = error "Not a real number"

    plus :: (Num a, Eq a) => BigNumber a -> BigNumber a -> BigNumber a
    plus (BigReal leftReal) (BigReal rightReal) = makeReal $ leftReal + rightReal
    plus (BigReal leftReal) (BigComplex rightReal rightImag0) = makeComplex (leftReal + rightReal) rightImag0
    plus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
    plus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = makeComplex (leftReal + rightReal) (rightReal + rightImag0)
    plus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
    plus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
    plus left right = plus right left

    minus :: (Num a, Eq a) => BigNumber a -> BigNumber a -> BigNumber a
    minus (BigReal leftReal) (BigReal rightReal) = makeReal $ leftReal - rightReal
    minus (BigReal leftReal) (BigComplex rightReal rightImag0) = makeComplex (leftReal - rightReal) (-rightImag0)
    minus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion (leftReal - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)
    minus (BigComplex leftReal leftImag0) (BigReal rightReal) = makeComplex (leftReal - rightReal) leftImag0
    minus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = makeComplex (leftReal - rightReal) (leftImag0 - rightImag0)
    minus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)
    minus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigReal rightReal) = makeQuaternion (leftReal - rightReal) leftImag0 leftImag1 leftImag2
    minus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = makeQuaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2
    minus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)

    multiply :: (Num a, Eq a) => BigNumber a -> BigNumber a -> BigNumber a
    multiply (BigReal leftReal) (BigReal rightReal) = makeReal $ leftReal * rightReal
    multiply (BigReal leftReal) (BigComplex rightReal rightImag0) = makeComplex (leftReal * rightReal) (leftReal * rightImag0)
    multiply (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)
    multiply (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = makeComplex realResult imag0Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
    multiply (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftReal * rightImag1 - leftImag0 * rightImag2
              imag2Result = leftReal * rightImag2 + leftImag0 * rightImag1
    multiply (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = makeQuaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftImag1 * rightReal + leftImag2 * rightImag0
              imag2Result = -leftImag1 * rightImag0 + leftImag2 * rightReal
    multiply (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = makeQuaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1
              imag1Result = leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0
              imag2Result = leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal
    multiply left right = multiply right left

    negate :: (Num a, Eq a) => BigNumber a -> BigNumber a
    negate = multiply negOne

    divide :: (Fractional a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    divide _ (BigReal 0) = A.failure A.DivideByZero "Cannot divide by zero"
    divide (BigReal leftReal) (BigReal rightReal) = (A.success . makeReal) $ leftReal / rightReal
    divide left right@BigComplex{} = divide numerator denominator
        where rightConj = conjugate right
              numerator = multiply left rightConj
              denominator = multiply right rightConj
    divide (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = A.success $ makeQuaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = leftReal * rightReal / denominator
              imag0Result = -leftReal * rightImag0 / denominator
              imag1Result = -leftReal * rightImag1 / denominator
              imag2Result = -leftReal * rightImag2 / denominator
    divide (BigComplex leftReal leftImag0) (BigReal rightReal) = A.success $ makeComplex (leftReal / rightReal) (leftImag0 / rightReal)
    divide (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = A.success $ makeQuaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
              imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
              imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
              imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator
    divide (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigReal rightReal) = A.success $ makeQuaternion (leftReal / rightReal) (leftImag0 / rightReal) (leftImag1 / rightReal) (leftImag2 / rightReal)
    divide (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = A.success $ makeQuaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
              imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
              imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
              imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator

    inverse :: (Fractional a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    inverse (BigReal 0) = A.failure A.DivideByZero "Zero does not have an inverse"
    inverse (BigReal realVal) = A.success $ makeReal $ 1 / realVal
    inverse (BigComplex realVal imag0Val) = A.success $ makeComplex (realVal / denominator) (-imag0Val / denominator)
        where denominator = (realVal ^^ 2) + (imag0Val ^^ 2)
    inverse quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = divide conjVal denominator
        where conjVal = conjugate quat
              denominator = makeReal $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val

    absolute :: (Floating a) => BigNumber a -> BigNumber a
    absolute (BigReal realVal) = makeReal $ abs realVal
    absolute (BigComplex realVal imag0Val) = (makeReal . sqrt) $ realVal * realVal + imag0Val * imag0Val
    absolute (BigQuaternion realVal imag0Val imag1Val imag2Val) = (makeReal . sqrt) $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val

    normalize :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    normalize val = divide val (absolute val)

    _binaryIntAction :: (Integral a, RealFrac b) => A.BinaryAction a a a -> BigNumber b -> BigNumber b -> A.Computation (BigNumber b)
    _binaryIntAction action left@BigReal{} right@BigReal{}
        | not $ (isInteger left) && (isInteger right) = A.failure A.NotInteger "Given action is only possible on integers"
        | otherwise = (A.success . asNumber) $ action leftInt rightInt
        where leftInt = asIntegral left
              rightInt = asIntegral right
    _binaryIntAction _ _ _ = A.failure A.NotInteger "Given action is only possible on integers"
    
    modulo :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    modulo _ (BigReal 0) = A.failure A.DivideByZero "Modulo by 0 is uncomputable"
    modulo left right
        | A.isFailure result = A.failure A.NotInteger "Operator mod is only possible on integers"
        | otherwise = result
        where result = _binaryIntAction mod left right

    remainder :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    remainder _ (BigReal 0) = A.failure A.DivideByZero "Division by 0 is uncomputable, therefore there is no remainder for any division by 0"
    remainder left right
        | A.isFailure result = A.failure A.NotInteger "Function rem is only possible on integers"
        | otherwise = result
        where result = _binaryIntAction rem left right

    integerDivide :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    integerDivide _ (BigReal 0) = A.failure A.DivideByZero "Integer division by zero is uncomputable"
    integerDivide left right
        | A.isFailure result = A.failure A.NotInteger "Integer division is only possible on integers"
        | otherwise = result
        where result = _binaryIntAction div left right

    gcd :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    gcd (BigReal 0) (BigReal 0) = A.failure A.InvalidInput "(0, 0) has no greatest common divisor because all integers are valid divisors"
    gcd first@BigReal{} second@BigReal{}
        | not $ (isInteger first) && (isInteger second) = A.failure A.NotInteger "Function gcd only works on integers"
        | otherwise = (A.success . asNumber) $ _gcdHelper firstInt secondInt
        where firstInt = asIntegral first
              secondInt = asIntegral second
    gcd _ _ = A.failure A.NotInteger "Function gcd only applies to integers"

    _gcdHelper :: (Integral a, Eq a) => a -> a -> a
    _gcdHelper first second
        | 0 == second = first
        | 0 == first = second
        | otherwise = _gcdHelper second (mod first second)

    lcm :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    lcm left right = A.resolveErrableBinary prod gcdVal integerDivide
        where prod = A.success $ multiply left right
              gcdVal = gcd left right

    factorial :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    factorial real@BigReal{}
        | not $ isInteger real = A.failure A.NotInteger "Only nonnegative integers have factorials"
        | int < 0 = A.failure A.NegativeInput "Only nonnegative integers have factorials"
        | otherwise = (A.success . asNumber) $ _factorialHelper int
        where int = asIntegral real
    factorial _ = A.failure A.NotInteger "Only integers have factorials"
    
    _factorialHelper :: (Integral a) => a -> a
    _factorialHelper 0 = 1
    _factorialHelper intVal = intVal * (_factorialHelper $ intVal - 1)

    choose :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    choose choices picks = A.resolveErrableBinary a d integerDivide
        where a = factorial choices
              b = factorial picks
              c = factorial $ minus choices picks
              d = A.resolveBinary b c multiply

    perm :: (RealFrac a, Eq a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    perm choices picks = A.resolveErrableBinary a b integerDivide
        where a = factorial choices
              b = factorial $ minus choices picks

    power :: (RealFloat a) => BigNumber a -> BigNumber a -> BigNumber a
    power (BigReal 0) (BigReal 0) = one
    power (BigReal 0) _ = zero
    power left right = exponential $ multiply (A.value $ naturalLogarithm left) right

    squareRoot :: (RealFloat a) => BigNumber a -> BigNumber a
    squareRoot = (`power` half)

    naturalLogarithm :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    naturalLogarithm (BigReal 0) = A.failure A.LogOfZero "Logarithm of zero is uncomputable"
    naturalLogarithm (BigReal realVal)
        | realVal < 0 = A.success $ makeComplex (log $ -realVal) pi
        | otherwise = (A.success . makeReal) $ log realVal
    naturalLogarithm com@BigComplex{} = A.success $ makeComplex realPart imag0Part
        where realPart = (log . _rreal) $ absolute com
              imag0Part = (_rreal . A.value) $ arg com
    naturalLogarithm quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = A.resolveBinary f e plus
        where a = vectorPart quat
              b = absolute quat
              c = A.value $ divide (makeReal realVal) b
              d = A.value $ arccosine c
              e = A.success $ multiply (A.value $ normalize a) d
              f = naturalLogarithm b

    logarithm :: (RealFloat a, Ord a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    logarithm base val = A.resolveErrableBinary valArg baseArg divide
        where baseArg = naturalLogarithm base
              valArg = naturalLogarithm val

    logarithm10 :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    logarithm10 = logarithm ten

    logarithm2 :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    logarithm2 = logarithm two
    
    exponential :: (Floating a, Eq a) => BigNumber a -> BigNumber a
    exponential (BigReal 0) = one
    exponential (BigReal realVal) = makeReal $ exp realVal
    exponential (BigComplex realVal imag0Val) = multiply (exponential $ makeReal realVal) (makeComplex (cos imag0Val) (sin imag0Val))
    exponential quat@BigQuaternion{} = multiply e d
        where a = vectorPart quat
              b = absolute quat
              c = multiply (A.value $ normalize a) (A.value $ sine b)
              d = plus (A.value $ cosine b) c
              e = exponential $ realCoef quat

    conjugate :: (Num a, Eq a) => BigNumber a -> BigNumber a
    conjugate val@BigReal{} = val
    conjugate (BigComplex realVal imag0Val) = makeComplex realVal (-imag0Val)
    conjugate (BigQuaternion realVal imag0Val imag1Val imag2Val) = makeQuaternion realVal (-imag0Val) (-imag1Val) (-imag2Val)

    arg :: (RealFloat a) => BigNumber a -> A.Computation (BigNumber a)
    arg BigReal{} = A.success zero
    arg (BigComplex 0 imag0Val)
        | imag0Val < 0 = A.success $ multiply (negate piValue) half
        | imag0Val > 0 = A.success $ multiply piValue half
    arg (BigComplex realVal imag0Val) = (A.success . makeReal) $ atan2 imag0Val realVal
    arg _ = A.failure A.NotComplex "Complex argument can only be applied to scalars that are in the set of all complex numbers"

    vectorPart :: (Num a, Eq a) => BigNumber a -> BigNumber a
    vectorPart BigReal{} = zero
    vectorPart (BigComplex _ imag0Val) = makeComplex 0 imag0Val
    vectorPart (BigQuaternion _ imag0Val imag1Val imag2Val) = makeQuaternion 0 imag0Val imag1Val imag2Val

    sine :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    sine (BigReal realVal) = (A.success . makeReal) $ sin realVal
    sine (BigComplex realVal imag0Val) = A.success $ makeComplex ((sin realVal) * (cosh imag0Val)) ((cos realVal) * (sinh imag0Val))
    sine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    cosine :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    cosine (BigReal realVal) = (A.success . makeReal) $ cos realVal
    cosine (BigComplex realVal imag0Val) = A.success $ makeComplex ((cos realVal) * (cosh imag0Val)) (-(sin realVal) * (sinh imag0Val))
    cosine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    tangent :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    tangent val = A.resolveErrableBinary sinValue cosValue divide
        where sinValue = sine val
              cosValue = cosine val

    sineHyperbolic :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    sineHyperbolic (BigReal realVal) = (A.success . makeReal) $ sinh realVal
    sineHyperbolic (BigComplex realVal imag0Val) = A.success $ makeComplex ((sinh realVal) * (cos imag0Val)) ((cosh realVal) * (sin imag0Val))
    sineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    cosineHyperbolic :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    cosineHyperbolic (BigReal realVal) = (A.success . makeReal) $ cosh realVal
    cosineHyperbolic (BigComplex realVal imag0Val) = A.success $ makeComplex ((cosh realVal) * (cos imag0Val)) ((sinh realVal) * (sin imag0Val))
    cosineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    tangentHyperbolic :: (Floating a, Eq a) => BigNumber a -> A.Computation (BigNumber a)
    tangentHyperbolic val = A.resolveErrableBinary sinhValue coshValue divide
        where sinhValue = sineHyperbolic val
              coshValue = cosineHyperbolic val

    arcsine :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arcsine val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . makeReal) $ asin realVal
        | otherwise = arcsine $ _forceComplex val
    arcsine com@BigComplex{} = A.success c
        where a = minus one (multiply com com)
              b = plus (multiply imagI com) (squareRoot a)
              c = multiply (negate imagI) (A.value $ naturalLogarithm b)
    arcsine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arccosine :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arccosine val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . makeReal) $ acos realVal
        | otherwise = arccosine $ _forceComplex val
    arccosine com@BigComplex{} = A.resolveBinary leftArg rightArg multiply
        where a = minus (multiply com com) one
              b = plus com (squareRoot a)
              leftArg = A.success $ negate imagI
              rightArg = naturalLogarithm b
    arccosine _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arctangent :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arctangent val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . makeReal) $ atan realVal
        | otherwise = arctangent $ _forceComplex val
    arctangent com@BigComplex{} = A.success $ multiply e d
        where a = multiply imagI com
              b = plus one a
              c = minus one a
              d = minus (A.value $ naturalLogarithm c) (A.value $ naturalLogarithm b)
              e = multiply imagI half
    arctangent _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arctangent2 :: (RealFloat a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    arctangent2 (BigReal leftReal) (BigReal rightReal) = (A.success . makeReal) $ atan2 leftReal rightReal
    arctangent2 _ _ = A.failure A.NotComplex "Function arctan2 can only be applied to real numbers"

    arcsineHyperbolic :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arcsineHyperbolic val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . makeReal) $ asinh realVal
        | otherwise = arcsineHyperbolic $ _forceComplex val
    arcsineHyperbolic com@BigComplex{} = A.resolveErrableBinary asinValue rightArg divide
        where asinValue = arcsine $ multiply imagI com
              rightArg = A.success imagI
    arcsineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arccosineHyperbolic :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arccosineHyperbolic val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . makeReal) $ acosh realVal
        | otherwise = arccosineHyperbolic $ _forceComplex val
    arccosineHyperbolic com@BigComplex{} = A.resolveBinary leftArg acosValue multiply
        where acosValue = arccosine com
              leftArg = A.success imagI
    arccosineHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    arctangentHyperbolic :: (RealFloat a, Ord a) => BigNumber a -> A.Computation (BigNumber a)
    arctangentHyperbolic val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (A.success . makeReal) $ atanh realVal
        | otherwise = arctangentHyperbolic $ _forceComplex val
    arctangentHyperbolic com@BigComplex{} = A.resolveErrableBinary atanValue rightArg divide
        where atanValue = arctangent $ multiply imagI com
              rightArg = A.success imagI
    arctangentHyperbolic _ = A.failure A.NotComplex "Trig Functions can only be applied to complex numbers"

    minVal :: (Ord a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    minVal (BigReal leftReal) (BigReal rightReal) = (A.success . makeReal) $ min leftReal rightReal
    minVal _ _ = A.failure A.NotComparable "Real numbers are the only scalars that have a total ordering and therefore have a minimum value"

    maxVal :: (Ord a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    maxVal (BigReal leftReal) (BigReal rightReal) = (A.success . makeReal) $ max leftReal rightReal
    maxVal _ _ = A.failure A.NotComparable "Real numbers are the only scalars that have a total ordering and therefore have a maximum value"

    less :: (Ord a) => BigNumber a -> BigNumber a -> A.Computation Bool
    less left right = A.resolveUnary result (LT==)
            where result = _compare left right

    greater :: (Ord a) => BigNumber a -> BigNumber a -> A.Computation Bool
    greater = flip less

    lessEqual :: (Ord a) => BigNumber a -> BigNumber a -> A.Computation Bool
    lessEqual left right = A.resolveUnary (greater left right) not

    greaterEqual :: (Ord a) => BigNumber a -> BigNumber a -> A.Computation Bool
    greaterEqual left right = A.resolveUnary (less left right) not

    _compare :: (Ord a) => BigNumber a -> BigNumber a -> A.Computation Ordering
    _compare (BigReal leftReal) (BigReal rightReal) = A.success $ compare leftReal rightReal
    _compare _ _ = A.failure A.NotComparable "Real numbers are the only scalars that have a total ordering"

    roundDown :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    roundDown (BigReal realVal) = (A.success . asNumber) $ floor realVal
    roundDown _ = A.failure A.NotReal "Only real numbers can be rounded up/down/off to the nearest integer"

    roundUp :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    roundUp (BigReal realVal) = (A.success . asNumber) $ ceiling realVal
    roundUp _ = A.failure A.NotReal "Only real numbers can be rounded up/down/off to the nearest integer"

    roundOff :: (RealFrac a) => BigNumber a -> A.Computation (BigNumber a)
    roundOff = roundDown . (plus half)

    rounded :: (RealFrac a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    rounded val places@BigReal{}
        | not $ isInteger places = A.failure A.NotInteger "Number of decimal places must be an integer"
        | intPlaces < 0 = A.failure A.NegativeInput "Cannot round to a negative number of decimal places"
        | isInteger val = A.success val
        | otherwise = A.success $ _fromList roundedList
        where intPlaces = asIntegral places
              list = _list val
              roundedList = fmap (_roundedHelper intPlaces) list
    rounded _ _ = A.failure A.NotInteger "The number of decimal places to round to must be a nonnegative integer"

    sigfig :: (RealFrac a, Show a) => BigNumber a -> BigNumber a -> A.Computation (BigNumber a)
    sigfig val places@BigReal{}
        | not $ isInteger places = A.failure A.NotInteger "Number of significant figures must be an integer"
        | otherwise = A.success $ _fromList sigfigList
        where intPlaces = asIntegral places
              list = _list val
              sigfigList = fmap (_sigfigHelper intPlaces) list
    sigfig _ _ = A.failure A.NotInteger "The number of significant figures to round to must be a nonnegative integer"

    _fromList :: (Num a, Eq a) => [a] -> BigNumber a
    _fromList [realVal] = makeReal realVal
    _fromList [realVal, imag0Val] = makeComplex realVal imag0Val
    _fromList [realVal, imag0Val, imag1Val, imag2Val] = makeQuaternion realVal imag0Val imag1Val imag2Val
    _fromList _ = error "Invalid list length"

    _roundedHelper :: (Integral a, RealFrac b) => a -> b -> b
    _roundedHelper places val = numerator / powerOfTen
        where powerOfTen = fromIntegral $ 10 ^ places
              numerator = (fromIntegral . round) $ val * powerOfTen

    _sigfigHelper :: (Integral a, RealFrac b, Show b) => a -> b -> b
    _sigfigHelper places val = _roundedHelper decimalPlaces val
        where strVal = show $ abs val
              indexOfDecimal = (fromIntegral . M.fromJust) $ L.elemIndex '.' strVal
              decimalPlaces = places - indexOfDecimal

    isEven :: (RealFrac a) => BigNumber a -> Bool
    isEven val@BigReal{}
        | A.isFailure result = False
        | otherwise = zero == (A.value result)
        where result = modulo val two
    isEven _ = False

    isOdd :: (RealFrac a) => BigNumber a -> Bool
    isOdd val@BigReal{}
        | A.isFailure result = False
        | otherwise = one == (A.value result)
        where result = modulo val two
    isOdd _ = False
  
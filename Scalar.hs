module Scalar (
    BigScalar,
    BinaryScalarOperation,
    real,
    complex,
    quaternion,
    isReal,
    isComplex,
    isQuaternion,
    zero,
    one,
    two,
    ten,
    negOne,
    spi,
    e,
    imagI,
    imagJ,
    imagK,
    realCoef,
    imag0Coef,
    imag1Coef,
    imag2Coef,
    splus,
    sminus,
    smult,
    sdiv,
    sneg,
    sabs,
    spow,
    ssqrt,
    sinv,
    sconj,
    snorm,
    sarg,
    sexp,
    slog,
    slog2,
    slog10,
    slogBase,
    ssin,
    scos,
    stan,
    ssinh,
    scosh,
    stanh,
    sasin,
    sacos,
    satan,
    sasinh,
    sacosh,
    satanh,
    (==),
    (/=),
    show
) where
    import MathInfo
    import Data.Number.BigFloat

    type BigNum = BigFloat Prec50

    data BigScalar = BigReal {
        _rreal :: BigNum
    } | BigComplex {
        _creal :: BigNum,
        _cimag0 :: BigNum
    } | BigQuaternion {
        _qreal :: BigNum,
        _qimag0 :: BigNum,
        _qimag1 :: BigNum,
        _qimag2 :: BigNum
    } deriving (Eq, Show)

    type BinaryScalarOperation = (BigScalar -> BigScalar -> BigScalar)

    real :: BigNum -> BigScalar
    real real = BigReal real

    complex :: BigNum -> BigNum -> BigScalar
    complex real 0 = BigReal real
    complex real imag0 = BigComplex real imag0

    quaternion :: BigNum -> BigNum -> BigNum -> BigNum -> BigScalar
    quaternion real 0 0 0 = BigReal real
    quaternion real imag0 0 0 = BigComplex real imag0
    quaternion real imag0 imag1 imag2 = BigQuaternion real imag0 imag1 imag2

    isReal :: BigScalar -> Bool
    isReal BigReal{} = True
    isReal _ = False

    isComplex :: BigScalar -> Bool
    isComplex BigComplex{} = True
    isComplex _ = False

    isQuaternion :: BigScalar -> Bool
    isQuaternion BigQuaternion{} = True
    isQuaternion _ = False

    zero :: BigScalar
    zero = BigReal 0

    one :: BigScalar
    one = BigReal 1

    two :: BigScalar
    two = BigReal 2

    ten :: BigScalar
    ten = BigReal 10

    negOne :: BigScalar
    negOne = BigReal $ -1

    half :: BigScalar
    half = BigReal 0.5

    spi :: BigScalar
    spi = BigReal pi

    e :: BigScalar
    e = BigReal $ exp 1

    imagI :: BigScalar
    imagI = BigComplex 0 1

    imagJ :: BigScalar
    imagJ = BigQuaternion 0 0 1 0

    imagK :: BigScalar
    imagK = BigQuaternion 0 0 0 1

    realCoef :: BigScalar -> BigScalar
    realCoef val@BigReal{} = val
    realCoef (BigComplex real _) = BigReal real
    realCoef (BigQuaternion real _ _ _) = BigReal real

    imag0Coef :: BigScalar -> BigScalar
    imag0Coef BigReal{} = zero
    imag0Coef (BigComplex _ imag0) = BigReal imag0
    imag0Coef (BigQuaternion _ imag0 _ _) = BigReal imag0

    imag1Coef :: BigScalar -> BigScalar
    imag1Coef BigReal{} = zero
    imag1Coef BigComplex{} = zero
    imag1Coef (BigQuaternion _ _ imag1 _) = BigReal imag1

    imag2Coef :: BigScalar -> BigScalar
    imag2Coef BigReal{} = zero
    imag2Coef BigComplex{} = zero
    imag2Coef (BigQuaternion _ _ _ imag2) = BigReal imag2

    splus :: BigScalar -> BigScalar -> BigScalar
    splus (BigReal leftReal) (BigReal rightReal) = real $ leftReal + rightReal
    splus (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) rightImag0
    splus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
    splus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) (leftImag0 + rightImag0)
    splus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
    splus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
    splus left right = splus right left

    sminus :: BigScalar -> BigScalar -> BigScalar
    sminus (BigReal leftReal) (BigReal rightReal) = real $ leftReal - rightReal
    sminus (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal - rightReal) (-rightImag0)
    sminus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)
    sminus (BigComplex leftReal leftImag0) (BigReal rightReal) = complex (leftReal - rightReal) leftImag0
    sminus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex (leftReal - rightReal) (leftImag0 - rightImag0)
    sminus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)
    sminus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigReal rightReal) = quaternion (leftReal - rightReal) leftImag0 leftImag1 leftImag2
    sminus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2
    sminus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)

    smult :: BigScalar -> BigScalar -> BigScalar
    smult (BigReal leftReal) (BigReal rightReal) = real $ leftReal * rightReal
    smult (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal * rightReal) (leftReal * rightImag0)
    smult (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)
    smult (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex realResult imag0Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
    smult (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftReal * rightImag1 - leftImag0 * rightImag2
              imag2Result = leftReal * rightImag2 + leftImag0 * rightImag1
    smult (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftImag1 * rightReal + leftImag2 * rightImag0
              imag2Result = -leftImag1 * rightImag0 + leftImag2 * rightReal
    smult (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1
              imag1Result = leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0
              imag2Result = leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal
    smult left right = smult right left

    sneg :: BigScalar -> BigScalar
    sneg = smult negOne

    sabs :: BigScalar -> BigScalar
    sabs (BigReal realVal) = real $ abs realVal
    sabs (BigComplex realVal imag0Val) = real $ sqrt $ realVal * realVal + imag0Val * imag0Val
    sabs (BigQuaternion realVal imag0Val imag1Val imag2Val) = real $ sqrt $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val

    sdiv :: BigScalar -> BigScalar -> MathResult BigScalar
    sdiv _ (BigReal 0) = withError DivideByZero
    sdiv (BigReal leftReal) (BigReal rightReal) = withValue $ real $ leftReal / rightReal
    sdiv left right@BigReal{} = withValue $ smult left (value $ sinv right)
    sdiv left right@(BigComplex rightReal rightImag0) = sdiv numerator denominator
        where rightConj =  sconj right
              numerator = smult left rightConj
              denominator = realCoef $ smult right rightConj
    sdiv (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = leftReal * rightReal / denominator
              imag0Result = -leftReal * rightImag0 / denominator
              imag1Result = -leftReal * rightImag1 / denominator
              imag2Result = -leftReal * rightImag2 / denominator
    sdiv (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
              imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
              imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
              imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator
    sdiv (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
              imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
              imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
              imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator

    spow :: BigScalar -> BigScalar -> MathResult BigScalar
    spow (BigReal 0) (BigReal 0) = withError ZeroToPowerOfZero
    spow _ (BigReal 0) = withValue one
    spow (BigReal 0) _ = withValue zero
    spow (BigReal leftReal) (BigReal rightReal) = withValue $ BigReal $ leftReal ** rightReal
    spow left@(BigReal leftReal) (BigComplex rightReal rightImag0) = withValue $ smult (value $ spow left (real rightReal)) (complex (cos a) (sin a))
        where a = (log leftReal) * rightImag0
    spow left@(BigComplex leftReal leftImag0) right@(BigReal rightReal) = withValue $ smult (value $ spow a right) (complex (cos c) (sin c))
        where a = sabs left
              b = value $ sarg left
              c = _rreal $ smult right b
    spow left@(BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = withValue $ complex (d * (cos c)) (d * (sin c))
        where a = leftReal * leftReal + leftImag0 * leftImag0
              b = _rreal $ value $ sarg left
              c = rightReal * b + 0.5 * rightImag0 * (log a)
              d = (a ** (rightReal / 2)) * (exp $ -rightImag0 * b)
    spow left right = withValue $ sexp $ smult (value $ slog left) right

    ssqrt :: BigScalar -> BigScalar
    ssqrt (BigReal realVal) = if realVal < 0 then complex 0 sqrtVal else real sqrtVal
        where sqrtVal = sqrt $ abs realVal
    ssqrt val = value $ spow val half

    sinv:: BigScalar -> MathResult BigScalar
    sinv quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = sdiv conj denominator
        where conj = sconj quat
              denominator = real $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val
    sinv val = sdiv one val

    sconj :: BigScalar -> BigScalar
    sconj (BigReal realVal) = real realVal
    sconj (BigComplex realVal imag0Val) = complex realVal (-imag0Val)
    sconj (BigQuaternion realVal imag0Val imag1Val imag2Val) = quaternion realVal (-imag0Val) (-imag1Val) (-imag2Val)

    snorm :: BigScalar -> MathResult BigScalar
    snorm value = sdiv value (sabs value)

    sarg :: BigScalar -> MathResult BigScalar
    sarg BigReal{} = withValue $ zero
    sarg (BigComplex 0 imag0Val) = withValue $ if imag0Val < 0 then value $ sdiv (sneg spi) two else if imag0Val > 0 then value $ sdiv spi two else zero
    sarg (BigComplex realVal imag0Val) = withValue $ real $ atan2 imag0Val realVal
    sarg _ = withError InvalidType

    sexp :: BigScalar -> BigScalar
    sexp (BigReal realVal) = real $ exp realVal
    sexp com@BigComplex{} = value $ spow (real $ exp 1) com
    sexp quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = smult (sexp $ real realVal) (splus (value $ scos b) (smult (value $ snorm a) (value $ ssin b)))
        where a = quaternion 0 imag0Val imag1Val imag2Val
              b = sabs quat

    slog :: BigScalar -> MathResult BigScalar
    slog (BigReal 0) = withError LogarithmOfZero
    slog (BigReal realVal)
        | (realVal < 0) = withValue $ complex (log $ abs realVal) pi
        | otherwise = withValue $ real $ log realVal
    slog com@BigComplex{} = withValue $ complex (log $ _rreal $ sabs com) (_rreal $ value $ sarg com)
    slog quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = withValue $ splus (value $ slog b) (smult (value $ snorm a) (value $ sacos $ value $ sdiv (real realVal) b))
        where a = quaternion 0 imag0Val imag1Val imag2Val
              b = sabs quat

    slog2 :: BigScalar -> MathResult BigScalar
    slog2 value = slogBase two value

    slog10 :: BigScalar -> MathResult BigScalar
    slog10 value = slogBase ten value

    slogBase :: BigScalar -> BigScalar -> MathResult BigScalar
    slogBase (BigReal 0) (BigReal 0) = withErrorList [LogarithmOfZero, LogarithmBaseOfZero]
    slogBase (BigReal 0) _ = withError LogarithmBaseOfZero
    slogBase _ (BigReal 0) = withError LogarithmOfZero
    slogBase base value = combine (slog value) (slog base) sdiv

    ssin :: BigScalar -> MathResult BigScalar
    ssin (BigReal realVal) = withValue $ real $ sin realVal
    ssin (BigComplex realVal imag0Val) = withValue $ complex ((sin realVal) * (cosh imag0Val)) ((cos realVal) * (sinh imag0Val))
    ssin _ = withError InvalidType

    scos :: BigScalar -> MathResult BigScalar
    scos (BigReal realVal) = withValue $ real $ cos realVal
    scos (BigComplex realVal imag0Val) = withValue $ complex ((cos realVal) * (cosh imag0Val)) (-(sin realVal) * (sinh imag0Val))
    scos _ = withError InvalidType

    stan :: BigScalar -> MathResult BigScalar
    stan value = combine sinValue cosValue sdiv
        where sinValue = ssin value
              cosValue = scos value

    ssinh :: BigScalar -> MathResult BigScalar
    ssinh (BigReal realVal) = withValue $ real $ sinh realVal
    ssinh (BigComplex realVal imag0Val) = withValue $ complex ((sinh realVal) * (cos imag0Val)) ((cosh realVal) * (sin imag0Val))
    ssinh _ = withError InvalidType

    scosh :: BigScalar -> MathResult BigScalar
    scosh (BigReal realVal) = withValue $ real $ cosh realVal
    scosh (BigComplex realVal imag0Val) = withValue $ complex ((cosh realVal) * (cos imag0Val)) ((sinh realVal) * (sin imag0Val))
    scosh _ = withError InvalidType

    stanh :: BigScalar -> MathResult BigScalar
    stanh value = combine sinhValue coshValue sdiv
        where sinhValue = ssinh value
              coshValue = scosh value

    sasin :: BigScalar -> MathResult BigScalar
    sasin (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ asin realVal
        | otherwise = sasin $ BigComplex realVal 0
    sasin com@BigComplex{} = withValue $ smult (sneg imagI) (value $ slog $ splus (smult imagI com) (ssqrt $ sminus one (smult com com)))
    sasin _ = withError InvalidType

    sacos :: BigScalar -> MathResult BigScalar
    sacos (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ acos realVal
        | otherwise = sacos $ BigComplex realVal 0
    sacos com@BigComplex{} = withValue $ sconj $ splus (value $ sdiv spi two) (smult imagI ((value $ slog $ splus (smult imagI com) (ssqrt $ sminus one (smult com com)))))
    sacos _ = withError InvalidType

    satan :: BigScalar -> MathResult BigScalar
    satan (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ atan realVal
        | otherwise = satan $ BigComplex realVal 0
    satan com@BigComplex{} = withValue $ smult (value $ sdiv imagI two) (sminus (value $ slog $ sminus one (smult imagI com)) (value $ slog $ splus one (smult imagI com)))
    satan _ = withError InvalidType

    sasinh :: BigScalar -> MathResult BigScalar
    sasinh (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ asinh realVal
        | otherwise = sasinh $ BigComplex realVal 0
    sasinh com@BigComplex{} = resolveLeft asinValue imagI sdiv
        where asinValue = sasin $ smult imagI com
    asainh _ = withError InvalidType

    sacosh :: BigScalar -> MathResult BigScalar
    sacosh (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ acosh realVal
        | otherwise = sacosh $ BigComplex realVal 0
    sacosh com@BigComplex{} = computeRight imagI acosValue smult
        where acosValue = sacos com
    sacosh _ = withError InvalidType

    satanh :: BigScalar -> MathResult BigScalar
    satanh (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ atanh realVal
        | otherwise = satanh $ BigComplex realVal 0
    satanh com@BigComplex{} = resolveLeft atanValue imagI sdiv
        where atanValue = satan $ smult imagI com
    satanh _ = withError InvalidType

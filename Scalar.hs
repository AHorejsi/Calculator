module Scalar (
    Scalar,
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
    import Data.HashSet

    data Scalar a = Real {
        _rreal :: a
    } | Complex {
        _creal :: a,
        _cimag0 :: a
    } | Quaternion {
        _qreal :: a,
        _qimag0 :: a,
        _qimag1 :: a,
        _qimag2 :: a
    } deriving (Eq, Show)

    real :: (RealFloat a) => a -> Scalar a
    real real = Real real

    complex ::(RealFloat a) => a -> a -> Scalar a
    complex real 0 = Real real
    complex real imag0 = Complex real imag0

    quaternion :: (RealFloat a) => a -> a -> a -> a -> Scalar a
    quaternion real 0 0 0 = Real real
    quaternion real imag0 0 0 = Complex real imag0
    quaternion real imag0 imag1 imag2 = Quaternion real imag0 imag1 imag2

    isReal :: Scalar a -> Bool
    isReal Real{} = True
    isReal _ = False

    isComplex :: Scalar a -> Bool
    isComplex Complex{} = True
    isComplex _ = False

    isQuaternion :: Scalar a -> Bool
    isQuaternion Quaternion{} = True
    isQuaternion _ = False

    zero :: (RealFloat a) => Scalar a
    zero = Real 0

    one :: (RealFloat a) => Scalar a
    one = Real 1

    two :: (RealFloat a) => Scalar a
    two = Real 2

    ten :: (RealFloat a) => Scalar a
    ten = Real 10

    negOne :: (RealFloat a) => Scalar a
    negOne = Real $ -1

    spi :: (RealFloat a) => Scalar a
    spi = Real pi

    imagI :: (RealFloat a) => Scalar a
    imagI = Complex 0 1

    imagJ :: (RealFloat a) => Scalar a
    imagJ = Quaternion 0 0 1 0

    imagK :: (RealFloat a) => Scalar a
    imagK = Quaternion 0 0 0 1

    realCoef :: (RealFloat a) => Scalar a -> Scalar a
    realCoef val@(Real real) = val
    realCoef (Complex real _) = Real real
    realCoef (Quaternion real _ _ _) = Real real

    imag0Coef :: (RealFloat a) => Scalar a -> Scalar a
    imag0Coef Real{} = zero
    imag0Coef (Complex _ imag0) = Real imag0
    imag0Coef (Quaternion _ imag0 _ _) = Real imag0

    imag1Coef :: (RealFloat a) => Scalar a -> Scalar a
    imag1Coef Real{} = zero
    imag1Coef Complex{} = zero
    imag1Coef (Quaternion _ _ imag1 _) = Real imag1

    imag2Coef :: (RealFloat a) => Scalar a -> Scalar a
    imag2Coef Real{} = zero
    imag2Coef Complex{} = zero
    imag2Coef (Quaternion _ _ _ imag2) = Real imag2

    splus :: (RealFloat a) => Scalar a -> Scalar a -> Scalar a
    splus (Real leftReal) (Real rightReal) = real $ leftReal + rightReal
    splus (Real leftReal) (Complex rightReal rightImag0) = complex (leftReal + rightReal) rightImag0
    splus (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
    splus (Complex leftReal leftImag0) (Complex rightReal rightImag0) = complex (leftReal + rightReal) (leftImag0 + rightImag0)
    splus (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
    splus (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
    splus left right = splus right left

    sminus :: (RealFloat a) => Scalar a -> Scalar a -> Scalar a
    sminus (Real leftReal) (Real rightReal) = real $ leftReal - rightReal
    sminus (Real leftReal) (Complex rightReal rightImag0) = complex (leftReal - rightReal) (-rightImag0)
    sminus (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)
    sminus (Complex leftReal leftImag0) (Real rightReal) = complex (leftReal - rightReal) leftImag0
    sminus (Complex leftReal leftImag0) (Complex rightReal rightImag0) = complex (leftReal - rightReal) (leftImag0 - rightImag0)
    sminus (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)
    sminus (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Real rightReal) = quaternion (leftReal - rightReal) leftImag0 leftImag1 leftImag2
    sminus (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2
    sminus (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)

    smult :: (RealFloat a) => Scalar a -> Scalar a -> Scalar a
    smult (Real leftReal) (Real rightReal) = real $ leftReal * rightReal
    smult (Real leftReal) (Complex rightReal rightImag0) = complex (leftReal * rightReal) (leftReal * rightImag0)
    smult (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)
    smult (Complex leftReal leftImag0) (Complex rightReal rightImag0) = complex realResult imag0Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
    smult (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftReal * rightImag1 - leftImag0 * rightImag2
              imag2Result = leftReal * rightImag2 + leftImag0 * rightImag1
    smult (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftImag1 * rightReal + leftImag2 * rightImag0
              imag2Result = -leftImag1 * rightImag0 + leftImag2 * rightReal
    smult (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1
              imag1Result = leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0
              imag2Result = leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal
    smult left right = smult right left

    sneg :: (RealFloat a) => Scalar a -> Scalar a
    sneg = smult negOne

    sabs :: (RealFloat a) => Scalar a -> Scalar a
    sabs (Real realVal) = real $ abs realVal
    sabs (Complex realVal imag0Val) = real $ sqrt $ realVal * realVal + imag0Val * imag0Val
    sabs (Quaternion realVal imag0Val imag1Val imag2Val) = real $ sqrt $ realVal * realVal + imag0Val * imag0Val * imag1Val * imag1Val + imag2Val * imag2Val

    sdiv :: (RealFloat a) => Scalar a -> Scalar a -> MathResult (Scalar a)
    sdiv _ (Real 0) = withError DivideByZero
    sdiv (Real leftReal) (Real rightReal) = withValue $ real $ leftReal / rightReal
    sdiv left right@Real{} = withValue $ smult left (value $ sinv right)
    sdiv left right@(Complex rightReal rightImag0) = sdiv numerator denominator
        where rightConj =  sconj right
              numerator = smult left rightConj
              denominator = realCoef $ smult right rightConj
    sdiv (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = leftReal * rightReal / denominator
              imag0Result = -leftReal * rightImag0 / denominator
              imag1Result = -leftReal * rightImag1 / denominator
              imag2Result = -leftReal * rightImag2 / denominator
    sdiv (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
              imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
              imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
              imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator
    sdiv (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
              imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
              imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
              imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator

    spow :: (RealFloat a) => Scalar a -> Scalar a -> MathResult (Scalar a)
    spow (Real 0) (Real 0) = withError ZeroToPowerOfZero
    spow _ (Real 0) = withValue one
    spow (Real 0) _ = withValue zero
    spow (Real leftReal) (Real rightReal) = withValue $ Real $ leftReal ** rightReal
    spow (Real leftReal) (Complex rightReal rightImag0) = withValue $ smult (Real $ leftReal ** rightReal) (Complex (cos a) (sin a))
        where a = (log leftReal) * rightImag0
    spow left@(Complex leftReal leftImag0) right@(Real rightReal) = withValue $ smult (value $ spow a right) (Complex (cos c) (sin c))
        where a = sabs left
              b = value $ sarg left
              c = _rreal $ smult right b
    spow left@(Complex leftReal leftImag0) (Complex rightReal rightImag0) = withValue $ complex (d * (cos c)) (d * (sin c))
        where a = leftReal * leftReal + leftImag0 * leftImag0
              b = _rreal $ value $ sarg left
              c = rightReal * b + 0.5 * rightImag0 * (log a)
              d = (a ** (rightReal / 2)) * (exp $ -rightImag0 * b)
    spow left right = withValue $ sexp $ smult (value $ slog left) right

    ssqrt :: (RealFloat a) => Scalar a -> Scalar a
    ssqrt (Real real)
        | real < 0 = smult imagI (Real $ sqrt $ -real)
        | otherwise = Real $ sqrt real
    ssqrt val = value $ spow val (value $ sdiv one two)

    sinv:: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    sinv quat@(Quaternion real imag0 imag1 imag2) = sdiv conj denominator
        where conj = sconj quat
              denominator = Real $ real * real + imag0 * imag0 + imag1 * imag1 + imag2 * imag2
    sinv val = sdiv one val

    sconj :: (Num a) => Scalar a -> Scalar a
    sconj (Real real) = Real real
    sconj (Complex real imag0) = Complex real (-imag0)
    sconj (Quaternion real imag0 imag1 imag2) = Quaternion real (-imag0) (-imag1) (-imag2)

    snorm :: (RealFloat a, Eq a) => Scalar a -> MathResult (Scalar a)
    snorm value = sdiv value (sabs value)

    sarg :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    sarg Real{} = withValue $ zero
    sarg (Complex 0 imag0) = withValue $ if imag0 < 0 then value $ sdiv (sneg spi) two else if imag0 > 0 then value $ sdiv spi two else zero
    sarg (Complex real imag0) = withValue $ Real $ atan2 imag0 real
    sarg _ = withError InvalidType

    sexp :: (RealFloat a) => Scalar a -> Scalar a
    sexp (Real real) = Real $ exp real
    sexp com@Complex{} = value $ spow (real $ exp 1) com
    sexp quat@(Quaternion real imag0 imag1 imag2) = smult (sexp $ Real real) (splus (value $ scos b) (smult (value $ snorm a) (value $ ssin b)))
        where a = Quaternion 0 imag0 imag1 imag2
              b = sabs quat

    slog :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    slog (Real 0) = withError LogarithmOfZero
    slog (Real realVal)
        | (realVal < 0) = withValue $ complex (log $ abs realVal) pi
        | otherwise = withValue $ real $ log realVal
    slog com@Complex{} = withValue $ complex (log $ _rreal $ sabs com) (_rreal $ value $ sarg com)
    slog quat@(Quaternion realVal imag0Val imag1Val imag2Val) = withValue $ splus (value $ slog b) (smult (value $ snorm a) (value $ sacos $ value $ sdiv (real realVal) b))
        where a = Quaternion 0 imag0Val imag1Val imag2Val
              b = sabs quat

    slog2 :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    slog2 value = slogBase two value

    slog10 :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    slog10 value = slogBase ten value

    slogBase :: (RealFloat a) => Scalar a -> Scalar a -> MathResult (Scalar a)
    slogBase (Real 0) (Real 0) = withErrorSet $ fromList [LogarithmOfZero, LogarithmBaseOfZero]
    slogBase (Real 0) _ = withError LogarithmBaseOfZero
    slogBase base value = combine (slog value) (slog base) sdiv

    ssin :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    ssin (Real realVal) = withValue $ real $ sin realVal
    ssin (Complex realVal imag0Val) = withValue $ complex ((sin realVal) * (cosh imag0Val)) ((cos realVal) * (sinh imag0Val))
    ssin _ = withError InvalidType

    scos :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    scos (Real realVal) = withValue $ real $ cos realVal
    scos (Complex realVal imag0Val) = withValue $ Complex ((cos realVal) * (cosh imag0Val)) (-(sin realVal) * (sinh imag0Val))
    scos _ = withError InvalidType

    stan :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    stan value = combine sinValue cosValue sdiv
        where sinValue = ssin value
              cosValue = scos value

    ssinh :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    ssinh (Real realVal) = withValue $ real $ sinh realVal
    ssinh (Complex realVal imag0Val) = withValue $ Complex ((sinh realVal) * (cos imag0Val)) ((cosh realVal) * (sin imag0Val))
    ssinh _ = withError InvalidType

    scosh :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    scosh (Real realVal) = withValue $ real $ cosh realVal
    scosh (Complex realVal imag0Val) = withValue $ Complex ((cosh realVal) * (cos imag0Val)) ((sinh realVal) * (sin imag0Val))
    scosh _ = withError InvalidType

    stanh :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    stanh value = combine sinhValue coshValue sdiv
        where sinhValue = ssinh value
              coshValue = scosh value

    sasin :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    sasin (Real realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ asin realVal
        | otherwise = sasin $ Complex realVal 0
    sasin com@Complex{} = withValue $ smult (sneg imagI) (value $ slog $ splus (smult imagI com) (ssqrt $ sminus one (smult com com)))
    sasin _ = withError InvalidType

    sacos :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    sacos (Real realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ acos realVal
        | otherwise = sacos $ Complex realVal 0
    sacos com@Complex{} = withValue $ sconj $ splus (value $ sdiv spi two) (smult imagI ((value $ slog $ splus (smult imagI com) (ssqrt $ sminus one (smult com com)))))
    sacos _ = withError InvalidType

    satan :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    satan (Real realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ atan realVal
        | otherwise = satan $ Complex realVal 0
    satan com@Complex{} = withValue $ smult (value $ sdiv imagI two) (sminus (value $ slog $ sminus one (smult imagI com)) (value $ slog $ splus one (smult imagI com)))
    satan _ = withError InvalidType

    sasinh :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    sasinh (Real realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ asinh realVal
        | otherwise = sasinh $ Complex realVal 0
    sasinh com@Complex{} = resolveLeft asinValue imagI sdiv
        where asinValue = sasin $ smult imagI com
    asainh _ = withError InvalidType

    sacosh :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    sacosh (Real realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ acosh realVal
        | otherwise = sacosh $ Complex realVal 0
    sacosh com@Complex{} = computeRight imagI acosValue smult
        where acosValue = sacos com
    sacosh _ = withError InvalidType

    satanh :: (RealFloat a) => Scalar a -> MathResult (Scalar a)
    satanh (Real realVal)
        | (-1) <= realVal && realVal <= 1 = withValue $ real $ atanh realVal
        | otherwise = satanh $ Complex realVal 0
    satanh com@Complex{} = resolveLeft atanValue imagI sdiv
        where atanValue = satan $ smult imagI com
    satanh _ = withError InvalidType

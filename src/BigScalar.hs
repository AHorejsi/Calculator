{-# LANGUAGE DeriveGeneric #-}

module BigScalar (
    BigScalar,
    UnaryScalarOperation,
    ErrableUnaryScalarOperation,
    BinaryScalarOperation,
    ErrableBinaryScalarOperation,
    ComparisonOperation,
    ErrableComparisonOperation,
    integral,
    integer,
    real,
    complex,
    quaternion,
    isExactInteger,
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
    eight,
    ten,
    sixteen,
    negOne,
    half,
    spi,
    se,
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
    sIntDiv,
    smod,
    srem,
    sgcd,
    slcm,
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
    smin,
    smax,
    scompare,
    sceil,
    sfloor,
    sLess,
    sGreater,
    sLessEqual,
    sGreaterEqual,
    toBinary,
    toHex,
    toOctal,
    (==),
    (/=),
    H.hash,
    H.hashWithSalt,
    show
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Char as C
    import qualified Data.Bits as B
    import qualified Data.Hashable as H
    import qualified Data.HashSet as HS
    import qualified Data.Number.Fixed as F
    import qualified MathInfo as MI
    
    type BigInt_ = Integer
    type BigFloat_ = F.Fixed F.Prec50

    data BigScalar = BigInteger {
        _int :: BigInt_
    } | BigReal {
        _rreal :: BigFloat_
    } | BigComplex {
        _creal :: BigFloat_,
        _cimag0 :: BigFloat_
    } | BigQuaternion {
        _qreal :: BigFloat_,
        _qimag0 :: BigFloat_,
        _qimag1 :: BigFloat_,
        _qimag2 :: BigFloat_
    } deriving (G.Generic, Eq)

    instance H.Hashable BigScalar where
        hashWithSalt salt (BigInteger intVal) = H.hashWithSalt salt (abs intVal)
        hashWithSalt salt (BigReal realVal) = H.hashWithSalt salt (fromEnum realVal)
        hashWithSalt salt (BigComplex realVal imag0Val) = H.hashWithSalt salt ((fromEnum realVal) + (fromEnum imag0Val))
        hashWithSalt salt (BigQuaternion realVal imag0Val imag1Val imag2Val) = H.hashWithSalt salt ((fromEnum realVal) + (fromEnum imag0Val) + (fromEnum imag0Val) + (fromEnum imag2Val))

    instance Show BigScalar where
        show (BigInteger intVal) = TP.printf "Integer(%s)" (show intVal)
        show (BigReal realVal) = TP.printf "Real(%s)" (show realVal)
        show (BigComplex realVal imag0Val) = TP.printf "Complex(%s,%s)" (show realVal) (show imag0Val)
        show (BigQuaternion realVal imag0Val imag1Val imag2Val) = TP.printf "Quaternion(%s,%s,%s,%s)" (show realVal) (show imag0Val) (show imag1Val) (show imag2Val)

    type UnaryScalarOperation = MI.UnaryOperation BigScalar BigScalar
    type ErrableUnaryScalarOperation = MI.ErrableUnaryOperation BigScalar BigScalar
    type BinaryScalarOperation = MI.BinaryOperation BigScalar BigScalar BigScalar
    type ErrableBinaryScalarOperation = MI.ErrableBinaryOperation BigScalar BigScalar BigScalar
    type ComparisonOperation = MI.BinaryOperation BigScalar BigScalar Bool
    type ErrableComparisonOperation = MI.ErrableBinaryOperation BigScalar BigScalar Bool

    integral :: (Integral a) => a -> BigScalar
    integral val = integer $ toInteger val

    integer :: BigInt_ -> BigScalar
    integer = BigInteger

    real :: BigFloat_ -> BigScalar
    real realVal
        | intPart == realVal = integer floorVal
        | otherwise = BigReal realVal
        where floorVal = floor realVal
              intPart = fromIntegral floorVal

    complex :: BigFloat_ -> BigFloat_ -> BigScalar
    complex realVal 0 = real realVal
    complex realVal imag0Val = BigComplex realVal imag0Val

    quaternion :: BigFloat_ -> BigFloat_ -> BigFloat_ -> BigFloat_ -> BigScalar
    quaternion realVal 0 0 0 = real realVal
    quaternion realVal imag0Val 0 0 = complex realVal imag0Val
    quaternion realVal imag0Val imag1Val imag2Val = BigQuaternion realVal imag0Val imag1Val imag2Val

    isExactInteger :: BigScalar -> Bool
    isExactInteger BigInteger{} = True
    isExactInteger _ = False

    isExactReal :: BigScalar -> Bool
    isExactReal BigReal{} = True
    isExactReal _ = False

    isExactComplex :: BigScalar -> Bool
    isExactComplex BigComplex{} = True
    isExactComplex _ = False

    isExactQuaternion :: BigScalar -> Bool
    isExactQuaternion BigQuaternion{} = True
    isExactQuaternion _ = False

    isInteger :: BigScalar -> Bool
    isInteger = isExactInteger

    isReal :: BigScalar -> Bool
    isReal val = isExactInteger val || isExactReal val

    isComplex :: BigScalar -> Bool
    isComplex val = isExactInteger val || isExactReal val || isExactComplex val

    isQuaternion :: BigScalar -> Bool
    isQuaternion _ = True

    zero :: BigScalar
    zero = BigInteger 0

    one :: BigScalar
    one = BigInteger 1

    two :: BigScalar
    two = BigInteger 2

    eight :: BigScalar
    eight = BigInteger 8

    ten :: BigScalar
    ten = BigInteger 10

    sixteen :: BigScalar
    sixteen = BigInteger 16

    negOne :: BigScalar
    negOne = BigInteger $ -1

    half :: BigScalar
    half = BigReal 0.5

    spi :: BigScalar
    spi = BigReal pi

    se :: BigScalar
    se = BigReal $ exp 1

    imagI :: BigScalar
    imagI = BigComplex 0 1

    imagJ :: BigScalar
    imagJ = BigQuaternion 0 0 1 0

    imagK :: BigScalar
    imagK = BigQuaternion 0 0 0 1

    realCoef :: BigScalar -> BigScalar
    realCoef val@BigInteger{} = val
    realCoef val@BigReal{} = val
    realCoef (BigComplex realVal _) = real realVal
    realCoef (BigQuaternion realVal _ _ _) = real realVal

    imag0Coef :: BigScalar -> BigScalar
    imag0Coef BigInteger{} = zero
    imag0Coef BigReal{} = zero
    imag0Coef (BigComplex _ imag0Val) = real imag0Val
    imag0Coef (BigQuaternion _ imag0Val _ _) = real imag0Val

    imag1Coef :: BigScalar -> BigScalar
    imag1Coef val@BigInteger{} = zero
    imag1Coef BigReal{} = zero
    imag1Coef BigComplex{} = zero
    imag1Coef (BigQuaternion _ _ imag1Val _) = real imag1Val

    imag2Coef :: BigScalar -> BigScalar
    imag2Coef val@BigInteger{} = zero
    imag2Coef BigReal{} = zero
    imag2Coef BigComplex{} = zero
    imag2Coef (BigQuaternion _ _ _ imag2Val) = real imag2Val

    _forceReal :: BigScalar -> BigScalar
    _forceReal (BigInteger intVal) = BigReal $ fromIntegral intVal
    _forceReal _ = error "Not a BigInteger"

    _forceComplex :: BigScalar -> BigScalar
    _forceComplex val@BigInteger{} = _forceComplex $ _forceReal val
    _forceComplex (BigReal realVal) = BigComplex realVal 0
    _forceComplex _ = error "Not a BigInteger or a BigReal"

    splus :: BigScalar -> BigScalar -> BigScalar
    splus (BigInteger leftInt) (BigInteger rightInt) = integer $ leftInt + rightInt
    splus left@BigInteger{} right = splus (_forceReal left) right
    splus (BigReal leftReal) (BigReal rightReal) = real $ leftReal + rightReal
    splus (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) rightImag0
    splus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
    splus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) (leftImag0 + rightImag0)
    splus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
    splus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
    splus left right = splus right left

    sminus :: BigScalar -> BigScalar -> BigScalar
    sminus (BigInteger leftInt) (BigInteger rightInt) = integer $ leftInt - rightInt
    sminus left@BigInteger{} right = sminus (_forceReal left) right
    sminus left right@BigInteger{} = sminus left (_forceReal right)
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
    smult (BigInteger leftInt) (BigInteger rightInt) = integer $ leftInt * rightInt
    smult left@BigInteger{} right = smult (_forceReal left) right
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
    sabs (BigInteger intVal) = integer $ abs intVal
    sabs (BigReal realVal) = real $ abs realVal
    sabs (BigComplex realVal imag0Val) = real $ sqrt $ realVal * realVal + imag0Val * imag0Val
    sabs (BigQuaternion realVal imag0Val imag1Val imag2Val) = real $ sqrt $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val

    sdiv :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    sdiv _ (BigInteger 0) = MI.withError MI.DivideByZero
    sdiv left@BigInteger{} right@BigInteger{} = sdiv (_forceReal left) (_forceReal right)
    sdiv left@BigInteger{} right = sdiv (_forceReal left) right
    sdiv left right@BigInteger{} = sdiv left (_forceReal right)
    sdiv (BigReal leftReal) (BigReal rightReal) = MI.withValue $ real $ leftReal / rightReal
    sdiv left right@BigReal{} = MI.withValue $ smult left (MI.value $ sinv right)
    sdiv left right@BigComplex{} = sdiv numerator denominator
        where rightConj = sconj right
              numerator = smult left rightConj
              denominator = smult right rightConj
    sdiv (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = MI.withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = leftReal * rightReal / denominator
              imag0Result = -leftReal * rightImag0 / denominator
              imag1Result = -leftReal * rightImag1 / denominator
              imag2Result = -leftReal * rightImag2 / denominator
    sdiv (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = MI.withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
              imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
              imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
              imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator
    sdiv (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = MI.withValue $ quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
              imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
              imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
              imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator

    _binaryIntOperation :: BigScalar -> BigScalar -> (BigInt_ -> BigInt_ -> BigInt_) -> MI.MathResult BigScalar
    _binaryIntOperation (BigInteger leftInt) (BigInteger rightInt) operation = MI.withValue $ integer $ operation leftInt rightInt
    _binaryIntOperation _ _ _ = MI.withError MI.InvalidType

    sIntDiv :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    sIntDiv _ (BigInteger 0) = MI.withError MI.DivideByZero
    sIntDiv left right = _binaryIntOperation left right div

    smod :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    smod _ (BigInteger 0) = MI.withError MI.DivideByZero
    smod left right = _binaryIntOperation left right mod

    srem :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    srem _ (BigInteger 0) = MI.withError MI.DivideByZero
    srem left right = _binaryIntOperation left right rem

    sgcd :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    sgcd left right = _binaryIntOperation left right gcd

    slcm :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    slcm left right = _binaryIntOperation left right lcm

    spow :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    spow (BigInteger 0) (BigInteger 0) = MI.withError MI.ZeroToPowerOfZero
    spow _ (BigInteger 0) = MI.withValue one
    spow (BigInteger 0) _ = MI.withValue zero
    spow left@(BigInteger leftInt) right@(BigInteger rightInt) = if rightInt < 0 then spow (_forceReal left) (_forceReal right) else MI.withValue $ integer $ leftInt ^ rightInt
    spow left@BigInteger{} right = spow (_forceReal left) right
    spow left right@BigInteger{} = spow left (_forceReal right)
    spow (BigReal leftReal) (BigReal rightReal) = MI.withValue $ real $ leftReal ** rightReal
    spow left right = MI.withValue $ sexp $ smult (MI.value $ slog left) right

    ssqrt :: BigScalar -> BigScalar
    ssqrt val@BigInteger{} = ssqrt $ _forceReal val
    ssqrt (BigReal realVal) = if realVal < 0 then complex 0 sqrtVal else real sqrtVal
        where sqrtVal = sqrt $ abs realVal
    ssqrt val = MI.value $ spow val half

    sinv:: BigScalar -> MI.MathResult BigScalar
    sinv quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = sdiv conj denominator
        where conj = sconj quat
              denominator = real $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val
    sinv val = sdiv one val

    sconj :: BigScalar -> BigScalar
    sconj (BigInteger intVal) = integer intVal
    sconj (BigReal realVal) = real realVal
    sconj (BigComplex realVal imag0Val) = complex realVal (-imag0Val)
    sconj (BigQuaternion realVal imag0Val imag1Val imag2Val) = quaternion realVal (-imag0Val) (-imag1Val) (-imag2Val)

    snorm :: BigScalar -> MI.MathResult BigScalar
    snorm val = sdiv val (sabs val)

    sarg :: BigScalar -> MI.MathResult BigScalar
    sarg val@BigInteger{} = sarg $ _forceReal val
    sarg BigReal{} = MI.withValue zero
    sarg (BigComplex 0 imag0Val)
        | imag0Val < 0 = sdiv (sneg spi) two
        | imag0Val > 0 = sdiv spi two
        | otherwise = MI.withValue zero
    sarg (BigComplex realVal imag0Val) = MI.withValue $ real $ atan2 imag0Val realVal
    sarg _ = MI.withError MI.InvalidType

    sexp :: BigScalar -> BigScalar
    sexp val@BigInteger{} = sexp $ _forceReal val
    sexp (BigReal realVal) = real $ exp realVal
    sexp com@(BigComplex realVal imag0Val) = smult (sexp $ real realVal) (complex (cos imag0Val) (sin imag0Val))
    sexp quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = smult (sexp b) (splus (MI.value $ scos c) (smult (MI.value $ snorm a) (MI.value $ ssin c)))
        where a = quaternion 0 imag0Val imag1Val imag2Val
              b = real realVal
              c = sabs quat

    slog :: BigScalar -> MI.MathResult BigScalar
    slog (BigInteger 0) = MI.withError MI.LogarithmOfZero
    slog val@BigInteger{} = slog $ _forceReal val
    slog (BigReal realVal)
        | realVal < 0 = MI.withValue $ complex (log $ -realVal) pi
        | otherwise = MI.withValue $ real $ log realVal
    slog com@BigComplex{} = MI.withValue $ complex (log $ _rreal $ sabs com) (_rreal $ MI.value $ sarg com)
    slog quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = MI.withValue $ splus (MI.value $ slog b) (smult (MI.value $ snorm a) (MI.value $ sacos $ MI.value $ sdiv (real realVal) b))
        where a = quaternion 0 imag0Val imag1Val imag2Val
              b = sabs quat

    slog2 :: BigScalar -> MI.MathResult BigScalar
    slog2 = slogBase two

    slog10 :: BigScalar -> MI.MathResult BigScalar
    slog10 = slogBase ten

    slogBase :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    slogBase (BigInteger 0) (BigInteger 0) = MI.withErrorList [MI.LogarithmOfZero, MI.LogarithmBaseOfZero]
    slogBase (BigInteger 0) _ = MI.withError MI.LogarithmBaseOfZero
    slogBase _ (BigInteger 0) = MI.withError MI.LogarithmOfZero
    slogBase base val = MI.errBinCombine (slog val) (slog base) sdiv

    ssin :: BigScalar -> MI.MathResult BigScalar
    ssin val@BigInteger{} = ssin $ _forceReal val
    ssin (BigReal realVal) = MI.withValue $ real $ sin realVal
    ssin (BigComplex realVal imag0Val) = MI.withValue $ complex ((sin realVal) * (cosh imag0Val)) ((cos realVal) * (sinh imag0Val))
    ssin _ = MI.withError MI.InvalidType

    scos :: BigScalar -> MI.MathResult BigScalar
    scos val@BigInteger{} = scos $ _forceReal val
    scos (BigReal realVal) = MI.withValue $ real $ cos realVal
    scos (BigComplex realVal imag0Val) = MI.withValue $ complex ((cos realVal) * (cosh imag0Val)) (-(sin realVal) * (sinh imag0Val))
    scos _ = MI.withError MI.InvalidType

    stan :: BigScalar -> MI.MathResult BigScalar
    stan val = MI.errBinCombine sinValue cosValue sdiv
        where sinValue = ssin val
              cosValue = scos val

    ssinh :: BigScalar -> MI.MathResult BigScalar
    ssinh val@BigInteger{} = ssinh $ _forceReal val
    ssinh (BigReal realVal) = MI.withValue $ real $ sinh realVal
    ssinh (BigComplex realVal imag0Val) = MI.withValue $ complex ((sinh realVal) * (cos imag0Val)) ((cosh realVal) * (sin imag0Val))
    ssinh _ = MI.withError MI.InvalidType

    scosh :: BigScalar -> MI.MathResult BigScalar
    scosh val@BigInteger{} = scosh $ _forceReal val
    scosh (BigReal realVal) = MI.withValue $ real $ cosh realVal
    scosh (BigComplex realVal imag0Val) = MI.withValue $ complex ((cosh realVal) * (cos imag0Val)) ((sinh realVal) * (sin imag0Val))
    scosh _ = MI.withError MI.InvalidType

    stanh :: BigScalar -> MI.MathResult BigScalar
    stanh val = MI.errBinCombine sinhValue coshValue sdiv
        where sinhValue = ssinh val
              coshValue = scosh val

    sasin :: BigScalar -> MI.MathResult BigScalar
    sasin val@BigInteger{} = sasin $ _forceReal val
    sasin val@(BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = MI.withValue $ real $ asin realVal
        | otherwise = sasin $ _forceComplex val
    sasin com@BigComplex{} = MI.withValue $ smult (sneg imagI) (MI.value $ slog $ splus (smult imagI com) (ssqrt $ sminus one (smult com com)))
    sasin _ = MI.withError MI.InvalidType

    sacos :: BigScalar -> MI.MathResult BigScalar
    sacos val@BigInteger{} = sacos $ _forceReal val
    sacos val@(BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = MI.withValue $ real $ acos realVal
        | otherwise = sacos $ _forceComplex val
    sacos com@BigComplex{} = MI.withValue $ smult (sneg imagI) (MI.value $ slog $ splus com (ssqrt $ sminus (smult com com) one))
    sacos _ = MI.withError MI.InvalidType

    satan :: BigScalar -> MI.MathResult BigScalar
    satan val@BigInteger{} = satan $ _forceReal val
    satan val@(BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = MI.withValue $ real $ atan realVal
        | otherwise = satan $ _forceComplex val
    satan com@BigComplex{} = MI.withValue $ smult (MI.value $ sdiv imagI two) (sminus (MI.value $ slog $ sminus one (smult imagI com)) (MI.value $ slog $ splus one (smult imagI com)))
    satan _ = MI.withError MI.InvalidType

    satan2 :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    satan2 left@BigInteger{} right@BigInteger{} = satan2 (_forceReal left) (_forceReal right)
    satan2 left@BigInteger{} right = satan2 (_forceReal left) right
    satan2 left right@BigInteger{} = satan2 left (_forceReal right)
    satan2 (BigReal leftReal) (BigReal rightReal) = MI.withValue $ real $ atan2 leftReal rightReal
    satan2 _ _ = MI.withError MI.InvalidType

    sasinh :: BigScalar -> MI.MathResult BigScalar
    sasinh val@BigInteger{} = sasinh $ _forceReal val
    sasinh val@(BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = MI.withValue $ real $ asinh realVal
        | otherwise = sasinh $ _forceComplex val
    sasinh com@BigComplex{} = MI.errBinResolveLeft asinValue imagI sdiv
        where asinValue = sasin $ smult imagI com
    sasinh _ = MI.withError MI.InvalidType

    sacosh :: BigScalar -> MI.MathResult BigScalar
    sacosh val@BigInteger{} = sacosh $ _forceReal val
    sacosh (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = MI.withValue $ real $ acosh realVal
        | otherwise = sacosh $ BigComplex realVal 0
    sacosh com@BigComplex{} = MI.binResolveRight imagI acosValue smult
        where acosValue = sacos com
    sacosh _ = MI.withError MI.InvalidType

    satanh :: BigScalar -> MI.MathResult BigScalar
    satanh val@BigInteger{} = satanh $ _forceReal val
    satanh (BigReal realVal)
        | (-1) <= realVal && realVal <= 1 = MI.withValue $ real $ atanh realVal
        | otherwise = satanh $ BigComplex realVal 0
    satanh com@BigComplex{} = MI.errBinResolveLeft atanValue imagI sdiv
        where atanValue = satan $ smult imagI com
    satanh _ = MI.withError MI.InvalidType

    smin :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    smin (BigInteger leftInt) (BigInteger rightInt) = MI.withValue $ integer $ min leftInt rightInt
    smin left@BigInteger{} right = smin (_forceReal left) right
    smin left right@BigInteger{} = smin left (_forceReal right)
    smin (BigReal leftReal) (BigReal rightReal) = MI.withValue $ real $ min leftReal rightReal
    smin _ _ = MI.withError MI.NoncomparableType

    smax :: BigScalar -> BigScalar -> MI.MathResult BigScalar
    smax (BigInteger leftInt) (BigInteger rightInt) = MI.withValue $ integer $ max leftInt rightInt
    smax left@BigInteger{} right = smax (_forceReal left) right
    smax left right@BigInteger{} = smax left (_forceReal right)
    smax (BigReal leftReal) (BigReal rightReal) = MI.withValue $ real $ max leftReal rightReal
    smax _ _ = MI.withError MI.NoncomparableType

    scompare :: BigScalar -> BigScalar -> MI.MathResult Ordering
    scompare left@BigInteger{} right = scompare (_forceReal left) right
    scompare left right@BigInteger{} = scompare left (_forceReal right)
    scompare (BigReal leftReal) (BigReal rightReal) = MI.withValue $ compare leftReal rightReal
    scompare _ _ = MI.withError MI.NoncomparableType

    sLess :: BigScalar -> BigScalar -> MI.MathResult Bool
    sLess = _checkCompare $ HS.singleton LT

    sGreater :: BigScalar -> BigScalar -> MI.MathResult Bool
    sGreater = _checkCompare $ HS.singleton GT

    sLessEqual :: BigScalar -> BigScalar -> MI.MathResult Bool
    sLessEqual = _checkCompare set
        where set = HS.fromList [EQ, LT]

    sGreaterEqual :: BigScalar -> BigScalar -> MI.MathResult Bool
    sGreaterEqual = _checkCompare set
        where set = HS.fromList [EQ, GT]

    _checkCompare :: HS.HashSet Ordering -> BigScalar -> BigScalar -> MI.MathResult Bool
    _checkCompare set left right
        | MI.isSuccess comparison = MI.withValue $ HS.member (MI.value comparison) set
        | otherwise = MI.convert comparison
        where comparison = scompare left right

    sceil :: BigScalar -> MI.MathResult BigScalar
    sceil val@BigInteger{} = MI.withValue val
    sceil (BigReal realVal) = MI.withValue $ integer $ ceiling realVal
    sceil _ = MI.withError MI.InvalidType

    sfloor :: BigScalar -> MI.MathResult BigScalar
    sfloor val@BigInteger{} = MI.withValue val
    sfloor (BigReal realVal) = MI.withValue $ integer $ floor realVal
    sfloor _ = MI.withError MI.InvalidType

    sEven :: BigScalar -> Bool
    sEven val@BigInteger{} = zero == (MI.value $ smod val two)
    sEven _ = False

    sOdd :: BigScalar -> Bool
    sOdd val@BigInteger{} = one == (MI.value $ smod val two)
    sOdd _ = False

    toBinary :: BigScalar -> String
    toBinary (BigInteger 0) = "0"
    toBinary val@BigInteger{} = _convertBase two val

    toHex :: BigScalar -> String
    toHex (BigInteger 0) = "0"
    toHex val@BigInteger{} = _convertBase sixteen val

    toOctal :: BigScalar -> String
    toOctal (BigInteger 0) = "0"
    toOctal val@BigInteger{} = _convertBase eight val

    _convertBase :: BigScalar -> BigScalar -> String
    _convertBase base (BigInteger 0) = ""
    _convertBase base (BigInteger 1) = "1"
    _convertBase base val = (_convertBase base divValue) ++ [char]
        where divValue = MI.value $ sIntDiv val base
              modValue = MI.value $ smod val base
              char = C.toUpper $ C.intToDigit $ fromIntegral $ _int modValue


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module BigScalar (
    BigScalar,
    UnaryScalarAction,
    ErrableUnaryScalarAction,
    BinaryScalarAction,
    ErrableBinaryScalarAction,
    TernaryScalarAction,
    ErrableTernaryScalarAction,
    integral,
    integer,
    asInt,
    real,
    complex,
    quaternion,
    isExactInteger,
    isExactReal,
    isExactComplex,
    isExactQuaternion,
    sIsInteger,
    sIsReal,
    sIsComplex,
    sIsQuaternion,
    zero,
    one,
    two,
    ten,
    negOne,
    half,
    spi,
    se,
    imagI,
    imagJ,
    imagK,
    sRealCoef,
    sImag0Coef,
    sImag1Coef,
    sImag2Coef,
    sminus,
    smult,
    sneg,
    sdiv,
    sIntDiv,
    smod,
    srem,
    sgcd,
    slcm,
    sfactorial,
    sabs,
    spow,
    ssqrt,
    sinv,
    sconj,
    snorm,
    sarg,
    sVectorPart,
    sexp,
    slog,
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
    satan2,
    sasinh,
    sacosh,
    satanh,
    smin,
    smax,
    sceil,
    sfloor,
    sround,
    sLess,
    sGreater,
    sLessEqual,
    sGreaterEqual,
    sEven,
    sOdd,
    sToBinary,
    sToHexadecimal,
    sToOctal
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Char as C
    import qualified Data.List as L
    import qualified Data.Maybe as M
    import qualified Data.Hashable as H
    import qualified Data.HashSet as HS
    import qualified Data.Number.Fixed as DNF
    import qualified CalcSettings as CS
    import qualified Actions as A
    import qualified Stringify as Str
    import qualified MathInfo as MI

    type BigInt_ = Integer
    type BigDecimal_ = DNF.Fixed DNF.Prec50

    class (Num a) => NumberStringifier_ a where
        simple :: a -> String
        rounded :: Int -> a -> String
        sigfig :: Int -> a -> String

    instance NumberStringifier_ BigInt_ where
        simple val = show val
        rounded count val = (simple val) ++ "." ++ (replicate count '0')
        sigfig count val = sigDigits ++ zeroes
            where str = simple val
                  valLength = (floor $ logBase 10 (fromIntegral $ abs val)) + 1
                  sigDigits = take count str
                  zeroCount = max 0 (valLength - count)
                  zeroes = replicate zeroCount '0'

    instance NumberStringifier_ BigDecimal_ where
        simple 0.0 = "0"
        simple val = reverse $ _dropTrailingZeroDecimals reversed
            where string = show val
                  reversed = reverse string
        rounded count val = simple $ numerator / powerOfTen
            where powerOfTen = 10 ^ count
                  numerator = (fromIntegral . round) $ val * powerOfTen
        sigfig count val = rounded decimalPlaces val
            where strVal = show $ abs val
                  indexOfDecimal = M.fromJust $ L.elemIndex '.' strVal
                  decimalPlaces = count - indexOfDecimal

    _dropTrailingZeroDecimals :: String -> String
    _dropTrailingZeroDecimals str
        | '0' == headVal = _dropTrailingZeroDecimals rest
        | '.' == headVal = rest
        | otherwise = str
        where headVal = head str
              rest = tail str

    data BigScalar = BigInteger {
        _int :: BigInt_
    } | BigReal {
        _rreal :: BigDecimal_
    } | BigComplex {
        _creal :: BigDecimal_,
        _cimag0 :: BigDecimal_
    } | BigQuaternion {
        _qreal :: BigDecimal_,
        _qimag0 :: BigDecimal_,
        _qimag1 :: BigDecimal_,
        _qimag2 :: BigDecimal_
    } deriving (G.Generic, Eq)

    type UnaryScalarAction = MI.UnaryAction BigScalar BigScalar
    type ErrableUnaryScalarAction = MI.ErrableUnaryAction BigScalar BigScalar
    type BinaryScalarAction = MI.BinaryAction BigScalar BigScalar BigScalar
    type ErrableBinaryScalarAction = MI.ErrableBinaryAction BigScalar BigScalar BigScalar
    type TernaryScalarAction = MI.TernaryAction BigScalar BigScalar BigScalar BigScalar
    type ErrableTernaryScalarAction = MI.ErrableTernaryAction BigScalar BigScalar BigScalar BigScalar

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

    instance Str.Stringifier BigScalar where
        stringify sets (BigInteger intVal) = stringFunc intVal
            where stringFunc = _makeStringFunc (CS.string sets)
        stringify sets val = _str val stringFunc
            where stringFunc = _makeStringFunc (CS.string sets)


    _makeStringFunc :: (NumberStringifier_ a) => CS.StringMode -> (a -> String)
    _makeStringFunc CS.Simple = simple
    _makeStringFunc (CS.Round roundCount) = rounded roundCount
    _makeStringFunc (CS.Sigfig sigfigCount) = sigfig sigfigCount

    _str :: BigScalar -> (BigDecimal_ -> String) -> String
    _str val converter = realStr ++ signedImagStr
        where valList = _list val
              strList = fmap converter valList
              imagAxes = ["i", "j", "k"]
              realStr = head strList
              imagStr = tail strList
              imagStrWithAxes = zipWith (++) imagStr imagAxes
              signedImagStr = concatMap _signedStr imagStrWithAxes

    _list :: BigScalar -> [BigDecimal_]
    _list (BigReal realVal) = [realVal]
    _list (BigComplex realVal imag0Val) = [realVal, imag0Val]
    _list (BigQuaternion realVal imag0Val imag1Val imag2Val) = [realVal, imag0Val, imag1Val, imag2Val]
    _list _ = error "Invalid Scalar"

    _signedStr :: String -> String
    _signedStr val
        | '-' == (head val) = val
        | otherwise = '+' : val

    integral :: (Integral a) => a -> BigScalar
    integral val = integer $ toInteger val

    asInt :: BigScalar -> Int
    asInt (BigInteger intVal) = fromIntegral intVal
    asInt _ = error "Input is not a BigInteger"

    integer :: BigInt_ -> BigScalar
    integer = BigInteger

    real :: BigDecimal_ -> BigScalar
    real realVal
        | intPart == realVal = integer floorVal
        | otherwise = BigReal realVal
        where floorVal = floor realVal
              intPart = fromIntegral floorVal

    complex :: BigDecimal_ -> BigDecimal_ -> BigScalar
    complex realVal 0 = real realVal
    complex realVal imag0Val = BigComplex realVal imag0Val

    quaternion :: BigDecimal_ -> BigDecimal_ -> BigDecimal_ -> BigDecimal_ -> BigScalar
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

    sIsInteger :: BigScalar -> Bool
    sIsInteger = isExactInteger

    sIsReal :: BigScalar -> Bool
    sIsReal val = isExactInteger val || isExactReal val

    sIsComplex :: BigScalar -> Bool
    sIsComplex val = isExactInteger val || isExactReal val || isExactComplex val

    sIsQuaternion :: BigScalar -> Bool
    sIsQuaternion _ = True

    zero :: BigScalar
    zero = BigInteger 0

    one :: BigScalar
    one = BigInteger 1

    two :: BigScalar
    two = BigInteger 2

    ten :: BigScalar
    ten = BigInteger 10

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

    _realPart :: BigScalar -> BigDecimal_
    _realPart (BigInteger intVal) = fromIntegral intVal
    _realPart (BigReal realVal) = realVal
    _realPart (BigComplex realVal _) = realVal
    _realPart (BigQuaternion realVal _ _ _) = realVal

    _imag0Part :: BigScalar -> BigDecimal_
    _imag0Part BigInteger{} = 0
    _imag0Part BigReal{} = 0
    _imag0Part (BigComplex _ imag0Val) = imag0Val
    _imag0Part (BigQuaternion _ imag0Val _ _) = imag0Val

    sRealCoef :: BigScalar -> BigScalar
    sRealCoef (BigInteger intVal) = integer intVal
    sRealCoef (BigReal realVal) = real realVal
    sRealCoef (BigComplex realVal _) = real realVal
    sRealCoef (BigQuaternion realVal _ _ _) = real realVal

    sImag0Coef :: BigScalar -> BigScalar
    sImag0Coef BigInteger{} = zero
    sImag0Coef BigReal{} = zero
    sImag0Coef (BigComplex _ imag0Val) = real imag0Val
    sImag0Coef (BigQuaternion _ imag0Val _ _) = real imag0Val

    sImag1Coef :: BigScalar -> BigScalar
    sImag1Coef BigInteger{} = zero
    sImag1Coef BigReal{} = zero
    sImag1Coef BigComplex{} = zero
    sImag1Coef (BigQuaternion _ _ imag1Val _) = real imag1Val

    sImag2Coef :: BigScalar -> BigScalar
    sImag2Coef BigInteger{} = zero
    sImag2Coef BigReal{} = zero
    sImag2Coef BigComplex{} = zero
    sImag2Coef (BigQuaternion _ _ _ imag2Val) = real imag2Val

    _forceReal :: BigScalar -> BigScalar
    _forceReal (BigInteger intVal) = BigReal $ fromIntegral intVal
    _forceReal _ = error "Not a BigInteger"

    _forceComplex :: BigScalar -> BigScalar
    _forceComplex val@BigInteger{} = _forceComplex $ _forceReal val
    _forceComplex (BigReal realVal) = BigComplex realVal 0
    _forceComplex _ = error "Not a BigInteger or a BigReal"

    instance A.Addable BigScalar where
        plus left right = MI.withValue $ A.unsafePlus left right
        unsafePlus (BigInteger leftInt) (BigInteger rightInt) = integer $ leftInt + rightInt
        unsafePlus left@BigInteger{} right = A.unsafePlus (_forceReal left) right
        unsafePlus (BigReal leftReal) (BigReal rightReal) = real $ leftReal + rightReal
        unsafePlus (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) rightImag0
        unsafePlus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
        unsafePlus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex (leftReal + rightReal) (rightReal + rightImag0)
        unsafePlus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
        unsafePlus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
        unsafePlus left right = A.unsafePlus right left 

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
    sabs (BigComplex realVal imag0Val) = (real . sqrt) $ realVal * realVal + imag0Val * imag0Val
    sabs (BigQuaternion realVal imag0Val imag1Val imag2Val) = (real . sqrt) $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val

    sdiv :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    sdiv _ (BigInteger 0) = MI.withError MI.InvalidValue
    sdiv left@BigInteger{} right@BigInteger{} = sdiv (_forceReal left) (_forceReal right)
    sdiv left@BigInteger{} right = sdiv (_forceReal left) right
    sdiv left right@BigInteger{} = sdiv left (_forceReal right)
    sdiv (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ leftReal / rightReal
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

    _binaryIntOperation :: BigScalar -> BigScalar -> MI.BinaryAction BigInt_ BigInt_ BigInt_ -> MI.ComputationResult BigScalar
    _binaryIntOperation (BigInteger leftInt) (BigInteger rightInt) action = (MI.withValue . integer) $ action leftInt rightInt
    _binaryIntOperation _ _ _ = MI.withError MI.InvalidValue

    sIntDiv :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    sIntDiv _ (BigInteger 0) = MI.withError MI.InvalidValue
    sIntDiv left right = _binaryIntOperation left right div

    smod :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    smod _ (BigInteger 0) = MI.withError MI.InvalidValue
    smod left right = _binaryIntOperation left right mod

    srem :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    srem _ (BigInteger 0) = MI.withError MI.InvalidValue
    srem left right = _binaryIntOperation left right rem

    sgcd :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    sgcd left right = _binaryIntOperation left right gcd

    slcm :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    slcm left right = _binaryIntOperation left right lcm

    sfactorial :: BigScalar -> MI.ComputationResult BigScalar
    sfactorial (BigInteger intVal) = (MI.withValue . integer) $ _sfactorialHelper intVal
    sfactorial _ = MI.withError MI.InvalidValue
    
    _sfactorialHelper :: BigInt_ -> BigInt_
    _sfactorialHelper 0 = 1
    _sfactorialHelper 1 = 1
    _sfactorialHelper intVal = intVal * (_sfactorialHelper $ intVal - 1)

    spow :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    spow (BigInteger 0) (BigInteger 0) = MI.withValue one
    spow (BigInteger 0) _ = MI.withValue zero
    spow left@(BigInteger leftInt) right@(BigInteger rightInt) = if rightInt < 0 then spow (_forceReal left) (_forceReal right) else (MI.withValue . integer) $ leftInt ^ rightInt
    spow left@BigInteger{} right = spow (_forceReal left) right
    spow left right@BigInteger{} = spow left (_forceReal right)
    spow (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ leftReal ** rightReal
    spow left right = (MI.withValue . sexp) $ smult (MI.value $ slog left) right

    ssqrt :: BigScalar -> BigScalar
    ssqrt val@BigInteger{} = ssqrt $ _forceReal val
    ssqrt (BigReal realVal) = if realVal < 0 then complex 0 sqrtVal else real sqrtVal
        where sqrtVal = sqrt $ abs realVal
    ssqrt val = MI.value $ spow val half

    sinv:: BigScalar -> MI.ComputationResult BigScalar
    sinv quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = sdiv conj denominator
        where conj = sconj quat
              denominator = real $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val
    sinv val = sdiv one val

    sconj :: BigScalar -> BigScalar
    sconj (BigInteger intVal) = integer intVal
    sconj (BigReal realVal) = real realVal
    sconj (BigComplex realVal imag0Val) = complex realVal (-imag0Val)
    sconj (BigQuaternion realVal imag0Val imag1Val imag2Val) = quaternion realVal (-imag0Val) (-imag1Val) (-imag2Val)

    snorm :: BigScalar -> MI.ComputationResult BigScalar
    snorm val = sdiv val (sabs val)

    sarg :: BigScalar -> MI.ComputationResult BigScalar
    sarg val@BigInteger{} = sarg $ _forceReal val
    sarg BigReal{} = MI.withValue zero
    sarg (BigComplex 0 imag0Val)
        | imag0Val < 0 = sdiv (sneg spi) two
        | imag0Val > 0 = sdiv spi two
        | otherwise = MI.withValue zero
    sarg (BigComplex realVal imag0Val) = MI.withValue $ real $ atan2 imag0Val realVal
    sarg _ = MI.withError MI.InvalidValue

    sVectorPart :: BigScalar -> BigScalar
    sVectorPart val@BigInteger{} = zero
    sVectorPart (BigReal realVal) = zero
    sVectorPart (BigComplex _ imag0Val) = complex 0 imag0Val
    sVectorPart (BigQuaternion _ imag0Val imag1Val imag2Val) = quaternion 0 imag0Val imag1Val imag2Val

    sexp :: BigScalar -> BigScalar
    sexp val@BigInteger{} = sexp $ _forceReal val
    sexp (BigReal realVal) = real $ exp realVal
    sexp (BigComplex realVal imag0Val) = smult (sexp $ real realVal) (complex (cos imag0Val) (sin imag0Val))
    sexp quat@BigQuaternion{} = smult (sexp b) (A.unsafePlus (MI.value $ scos c) (smult (MI.value $ snorm a) (MI.value $ ssin c)))
        where a = sVectorPart quat
              b = sRealCoef quat
              c = sabs quat

    slog :: BigScalar -> MI.ComputationResult BigScalar
    slog (BigInteger 0) = MI.withError MI.InvalidValue
    slog val@BigInteger{} = slog $ _forceReal val
    slog (BigReal realVal)
        | realVal < 0 = MI.withValue $ complex (log $ -realVal) pi
        | otherwise = (MI.withValue . real) $ log realVal
    slog com@BigComplex{} = MI.withValue $ complex realPart imag0Part
        where realPart = (log . _realPart) $ sabs com
              imag0Part = (_realPart . MI.value) $ sarg com
    slog quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = MI.withValue $ A.unsafePlus (MI.value $ slog b) (smult (MI.value $ snorm a) ((MI.value . sacos . MI.value) $ sdiv (real realVal) b))
        where a = sVectorPart quat
              b = sabs quat

    slogBase :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    slogBase base val = MI.errBinCombine (slog val) (slog base) sdiv

    ssin :: BigScalar -> MI.ComputationResult BigScalar
    ssin val@BigInteger{} = ssin $ _forceReal val
    ssin (BigReal realVal) = (MI.withValue . real) $ sin realVal
    ssin (BigComplex realVal imag0Val) = MI.withValue $ complex ((sin realVal) * (cosh imag0Val)) ((cos realVal) * (sinh imag0Val))
    ssin _ = MI.withError MI.InvalidValue

    scos :: BigScalar -> MI.ComputationResult BigScalar
    scos val@BigInteger{} = scos $ _forceReal val
    scos (BigReal realVal) = (MI.withValue . real) $ cos realVal
    scos (BigComplex realVal imag0Val) = MI.withValue $ complex ((cos realVal) * (cosh imag0Val)) (-(sin realVal) * (sinh imag0Val))
    scos _ = MI.withError MI.InvalidValue

    stan :: BigScalar -> MI.ComputationResult BigScalar
    stan val = MI.errBinCombine sinValue cosValue sdiv
        where sinValue = ssin val
              cosValue = scos val

    ssinh :: BigScalar -> MI.ComputationResult BigScalar
    ssinh val@BigInteger{} = ssinh $ _forceReal val
    ssinh (BigReal realVal) = (MI.withValue . real) $ sinh realVal
    ssinh (BigComplex realVal imag0Val) = MI.withValue $ complex ((sinh realVal) * (cos imag0Val)) ((cosh realVal) * (sin imag0Val))
    ssinh _ = MI.withError MI.InvalidValue

    scosh :: BigScalar -> MI.ComputationResult BigScalar
    scosh val@BigInteger{} = scosh $ _forceReal val
    scosh (BigReal realVal) = (MI.withValue . real) $ cosh realVal
    scosh (BigComplex realVal imag0Val) = MI.withValue $ complex ((cosh realVal) * (cos imag0Val)) ((sinh realVal) * (sin imag0Val))
    scosh _ = MI.withError MI.InvalidValue

    stanh :: BigScalar -> MI.ComputationResult BigScalar
    stanh val = MI.errBinCombine sinhValue coshValue sdiv
        where sinhValue = ssinh val
              coshValue = scosh val

    sasin :: BigScalar -> MI.ComputationResult BigScalar
    sasin val@BigInteger{} = sasin $ _forceReal val
    sasin val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ asin realVal
        | otherwise = sasin $ _forceComplex val
    sasin com@BigComplex{} = MI.withValue $ smult (sneg imagI) ((MI.value . slog) $ A.unsafePlus (smult imagI com) (ssqrt $ sminus one (smult com com)))
    sasin _ = MI.withError MI.InvalidValue

    sacos :: BigScalar -> MI.ComputationResult BigScalar
    sacos val@BigInteger{} = sacos $ _forceReal val
    sacos val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ acos realVal
        | otherwise = sacos $ _forceComplex val
    sacos com@BigComplex{} = MI.withValue $ smult (sneg imagI) ((MI.value . slog) $ A.unsafePlus com (ssqrt $ sminus (smult com com) one))
    sacos _ = MI.withError MI.InvalidValue

    satan :: BigScalar -> MI.ComputationResult BigScalar
    satan val@BigInteger{} = satan $ _forceReal val
    satan val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ atan realVal
        | otherwise = satan $ _forceComplex val
    satan com@BigComplex{} = MI.withValue $ smult (smult imagI half) (sminus ((MI.value . slog) $ sminus one (smult imagI com)) ((MI.value . slog) $ A.unsafePlus one (smult imagI com)))
    satan _ = MI.withError MI.InvalidValue

    satan2 :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    satan2 left@BigInteger{} right@BigInteger{} = satan2 (_forceReal left) (_forceReal right)
    satan2 left@BigInteger{} right = satan2 (_forceReal left) right
    satan2 left right@BigInteger{} = satan2 left (_forceReal right)
    satan2 (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ atan2 leftReal rightReal
    satan2 _ _ = MI.withError MI.InvalidValue

    sasinh :: BigScalar -> MI.ComputationResult BigScalar
    sasinh val@BigInteger{} = sasinh $ _forceReal val
    sasinh val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ asinh realVal
        | otherwise = sasinh $ _forceComplex val
    sasinh com@BigComplex{} = MI.errBinResolveLeft asinValue imagI sdiv
        where asinValue = sasin $ smult imagI com
    sasinh _ = MI.withError MI.InvalidValue

    sacosh :: BigScalar -> MI.ComputationResult BigScalar
    sacosh val@BigInteger{} = sacosh $ _forceReal val
    sacosh val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ acosh realVal
        | otherwise = sacosh $ _forceComplex val
    sacosh com@BigComplex{} = MI.binResolveRight imagI acosValue smult
        where acosValue = sacos com
    sacosh _ = MI.withError MI.InvalidValue

    satanh :: BigScalar -> MI.ComputationResult BigScalar
    satanh val@BigInteger{} = satanh $ _forceReal val
    satanh val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ atanh realVal
        | otherwise = satanh $ _forceComplex val
    satanh com@BigComplex{} = MI.errBinResolveLeft atanValue imagI sdiv
        where atanValue = satan $ smult imagI com
    satanh _ = MI.withError MI.InvalidValue

    smin :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    smin (BigInteger leftInt) (BigInteger rightInt) = (MI.withValue . integer) $ min leftInt rightInt
    smin left@BigInteger{} right = smin (_forceReal left) right
    smin left right@BigInteger{} = smin left (_forceReal right)
    smin (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ min leftReal rightReal
    smin _ _ = MI.withError MI.InvalidValue

    smax :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    smax (BigInteger leftInt) (BigInteger rightInt) = (MI.withValue . integer) $ max leftInt rightInt
    smax left@BigInteger{} right = smax (_forceReal left) right
    smax left right@BigInteger{} = smax left (_forceReal right)
    smax (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ max leftReal rightReal
    smax _ _ = MI.withError MI.InvalidValue

    _scompare :: BigScalar -> BigScalar -> MI.ComputationResult Ordering
    _scompare left@BigInteger{} right = _scompare (_forceReal left) right
    _scompare left right@BigInteger{} = _scompare left (_forceReal right)
    _scompare (BigReal leftReal) (BigReal rightReal) = MI.withValue $ compare leftReal rightReal
    _scompare _ _ = MI.withError MI.InvalidValue

    sLess :: BigScalar -> BigScalar -> MI.ComputationResult Bool
    sLess left right = MI.unResolve result (LT==)
        where result = _scompare left right

    sGreater :: BigScalar -> BigScalar -> MI.ComputationResult Bool
    sGreater left right = MI.unResolve result (GT==)
        where result = _scompare left right

    sLessEqual :: BigScalar -> BigScalar -> MI.ComputationResult Bool
    sLessEqual left right = MI.unResolve result (\order -> (LT == order) || (EQ == order))
        where result = _scompare left right

    sGreaterEqual :: BigScalar -> BigScalar -> MI.ComputationResult Bool
    sGreaterEqual left right = MI.unResolve result (\order -> (GT == order) || (EQ == order))
        where result = _scompare left right

    sceil :: BigScalar -> MI.ComputationResult BigScalar
    sceil val@BigInteger{} = MI.withValue val
    sceil (BigReal realVal) = (MI.withValue . integer) $ ceiling realVal
    sceil _ = MI.withError MI.InvalidValue

    sfloor :: BigScalar -> MI.ComputationResult BigScalar
    sfloor val@BigInteger{} = MI.withValue val
    sfloor (BigReal realVal) = (MI.withValue . integer) $ floor realVal
    sfloor _ = MI.withError MI.InvalidValue

    sround :: BigScalar -> MI.ComputationResult BigScalar
    sround = sfloor . (A.unsafePlus half)

    sEven :: BigScalar -> Bool
    sEven val@BigInteger{} = zero == (MI.value $ smod val two)
    sEven _ = False

    sOdd :: BigScalar -> Bool
    sOdd val@BigInteger{} = one == (MI.value $ smod val two)
    sOdd _ = False

    sToBinary :: BigScalar -> MI.ComputationResult String
    sToBinary = _baseConvert 2

    sToHexadecimal :: BigScalar -> MI.ComputationResult String
    sToHexadecimal = _baseConvert 16

    sToOctal :: BigScalar -> MI.ComputationResult String
    sToOctal = _baseConvert 8

    _baseConvert :: BigInt_ -> BigScalar -> MI.ComputationResult String
    _baseConvert radix value = MI.unResolve result reverse
        where result = _baseConvertHelper radix value

    _baseConvertHelper :: BigInt_ -> BigScalar -> MI.ComputationResult String
    _baseConvertHelper _ (BigInteger 0) = MI.withValue "0"
    _baseConvertHelper radix (BigInteger intVal) = MI.withValue $ _intBaseConvert radix intVal
    _baseConvertHelper _ _ = MI.withError MI.InvalidValue

    _intBaseConvert :: BigInt_ -> BigInt_ -> String
    _intBaseConvert _ 0 = ""
    _intBaseConvert radix val = digitChar : next
        where digit = mod val radix
              digitChar = (C.toUpper . C.intToDigit) $ fromIntegral digit
              quotient = div val radix
              next = _intBaseConvert radix quotient
              
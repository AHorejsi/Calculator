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
    smod,
    srem,
    sIntDiv,
    sgcd,
    slcm,
    sfactorial,
    sconj,
    sarg,
    sVectorPart,
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
    sceil,
    sfloor,
    sround,
    sEven,
    sOdd,
    sToBinary,
    sToHexadecimal,
    sToOctal,
    ScalarMultipliable,
    leftScalarMult,
    rightScalarMult,
    unsafeLeftScalarMult,
    unsafeRightScalarMult,
    Graphable,
    absol,
    norm,
    unsafeAbsol,
    unsafeNorm
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Char as C
    import qualified Data.List as L
    import qualified Data.Maybe as M
    import qualified Data.Hashable as H
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

    class ScalarMultipliable a where
        leftScalarMult :: a -> BigScalar -> MI.ComputationResult a
        rightScalarMult :: BigScalar -> a -> MI.ComputationResult a
        unsafeLeftScalarMult :: a -> BigScalar -> a
        unsafeLeftScalarMult = MI.binaryNonerror leftScalarMult
        unsafeRightScalarMult :: BigScalar -> a -> a
        unsafeRightScalarMult = MI.binaryNonerror rightScalarMult

    class Graphable a where
        absol :: a -> MI.ComputationResult BigScalar
        norm :: a -> MI.ComputationResult a
        unsafeAbsol :: a -> BigScalar
        unsafeAbsol = MI.unaryNonerror absol
        unsafeNorm :: a -> a
        unsafeNorm = MI.unaryNonerror norm

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

    instance A.Subtractable BigScalar where
        minus left right = MI.withValue $ A.unsafeMinus left right
        unsafeMinus (BigInteger leftInt) (BigInteger rightInt) = integer $ leftInt - rightInt
        unsafeMinus left@BigInteger{} right = A.unsafeMinus (_forceReal left) right
        unsafeMinus left right@BigInteger{} = A.unsafeMinus left (_forceReal right)
        unsafeMinus (BigReal leftReal) (BigReal rightReal) = real $ leftReal - rightReal
        unsafeMinus (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal - rightReal) (-rightImag0)
        unsafeMinus (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)
        unsafeMinus (BigComplex leftReal leftImag0) (BigReal rightReal) = complex (leftReal - rightReal) leftImag0
        unsafeMinus (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex (leftReal - rightReal) (leftImag0 - rightImag0)
        unsafeMinus (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)
        unsafeMinus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigReal rightReal) = quaternion (leftReal - rightReal) leftImag0 leftImag1 leftImag2
        unsafeMinus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2
        unsafeMinus (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)

    instance A.Multipliable BigScalar where
        mult left right = MI.withValue $ A.unsafeMult left right
        unsafeMult (BigInteger leftInt) (BigInteger rightInt) = integer $ leftInt * rightInt
        unsafeMult left@BigInteger{} right = A.unsafeMult (_forceReal left) right
        unsafeMult (BigReal leftReal) (BigReal rightReal) = real $ leftReal * rightReal
        unsafeMult (BigReal leftReal) (BigComplex rightReal rightImag0) = complex (leftReal * rightReal) (leftReal * rightImag0)
        unsafeMult (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)
        unsafeMult (BigComplex leftReal leftImag0) (BigComplex rightReal rightImag0) = complex realResult imag0Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
        unsafeMult (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
                  imag1Result = leftReal * rightImag1 - leftImag0 * rightImag2
                  imag2Result = leftReal * rightImag2 + leftImag0 * rightImag1
        unsafeMult (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigComplex rightReal rightImag0) = quaternion realResult imag0Result imag1Result imag2Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
                  imag1Result = leftImag1 * rightReal + leftImag2 * rightImag0
                  imag2Result = -leftImag1 * rightImag0 + leftImag2 * rightReal
        unsafeMult (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = quaternion realResult imag0Result imag1Result imag2Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1
                  imag1Result = leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0
                  imag2Result = leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal
        unsafeMult left right = A.unsafeMult right left

    instance A.Negatable BigScalar where
        unsafeNeg = A.unsafeMult negOne
        neg = MI.withValue . A.unsafeNeg

    instance Graphable BigScalar where
        absol = MI.withValue . unsafeAbsol
        unsafeAbsol (BigInteger intVal) = integer $ abs intVal
        unsafeAbsol (BigReal realVal) = real $ abs realVal
        unsafeAbsol (BigComplex realVal imag0Val) = (real . sqrt) $ realVal * realVal + imag0Val * imag0Val
        unsafeAbsol (BigQuaternion realVal imag0Val imag1Val imag2Val) = (real . sqrt) $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val
        norm val = A.div val (unsafeAbsol val)

    instance A.Divisible BigScalar where
        div _ (BigInteger 0) = MI.withError MI.InvalidValue
        div left@BigInteger{} right@BigInteger{} = A.div (_forceReal left) (_forceReal right)
        div left@BigInteger{} right = A.div (_forceReal left) right
        div left right@BigInteger{} = A.div left (_forceReal right)
        div (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ leftReal / rightReal
        div left right@BigReal{} = MI.withValue $ A.unsafeMult left (A.unsafeInv right)
        div left right@BigComplex{} = A.div numerator denominator
            where rightConj = sconj right
                  numerator = A.unsafeMult left rightConj
                  denominator = A.unsafeMult right rightConj
        div (BigReal leftReal) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = MI.withValue $ quaternion realResult imag0Result imag1Result imag2Result
            where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
                  realResult = leftReal * rightReal / denominator
                  imag0Result = -leftReal * rightImag0 / denominator
                  imag1Result = -leftReal * rightImag1 / denominator
                  imag2Result = -leftReal * rightImag2 / denominator
        div (BigComplex leftReal leftImag0) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = MI.withValue $ quaternion realResult imag0Result imag1Result imag2Result
            where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
                  realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
                  imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
                  imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
                  imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator
        div (BigQuaternion leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion rightReal rightImag0 rightImag1 rightImag2) = MI.withValue $ quaternion realResult imag0Result imag1Result imag2Result
            where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
                  realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
                  imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
                  imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
                  imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator
        inv quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = A.div conj denominator
            where conj = sconj quat
                  denominator = real $ realVal * realVal + imag0Val * imag0Val + imag1Val * imag1Val + imag2Val * imag2Val
        inv val = A.div one val
    
    smod :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    smod _ (BigInteger 0) = MI.withError MI.InvalidValue
    smod left right = _binaryIntOperation left right mod

    srem :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    srem _ (BigInteger 0) = MI.withError MI.InvalidValue
    srem left right = _binaryIntOperation left right rem

    _binaryIntOperation :: BigScalar -> BigScalar -> MI.BinaryAction BigInt_ BigInt_ BigInt_ -> MI.ComputationResult BigScalar
    _binaryIntOperation (BigInteger leftInt) (BigInteger rightInt) action = (MI.withValue . integer) $ action leftInt rightInt
    _binaryIntOperation _ _ _ = MI.withError MI.InvalidValue

    sIntDiv :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    sIntDiv _ (BigInteger 0) = MI.withError MI.InvalidValue
    sIntDiv left right = _binaryIntOperation left right div

    sgcd :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    sgcd left right = _binaryIntOperation left right gcd

    slcm :: BigScalar -> BigScalar -> MI.ComputationResult BigScalar
    slcm left right = _binaryIntOperation left right lcm

    sfactorial :: BigScalar -> MI.ComputationResult BigScalar
    sfactorial (BigInteger intVal) = (MI.withValue . integer) $ _sfactorialHelper intVal
    sfactorial _ = MI.withError MI.InvalidValue
    
    _sfactorialHelper :: BigInt_ -> BigInt_
    _sfactorialHelper 0 = 1
    _sfactorialHelper intVal = intVal * (_sfactorialHelper $ intVal - 1)

    instance A.Power BigScalar where
        pow left right = MI.withValue $ A.unsafePow left right
        unsafePow (BigInteger 0) (BigInteger 0) = one
        unsafePow (BigInteger 0) _ = zero
        unsafePow left@(BigInteger leftInt) right@(BigInteger rightInt) = if rightInt < 0 then A.unsafePow (_forceReal left) (_forceReal right) else integer $ leftInt ^ rightInt
        unsafePow left right = A.unsafeExp $ A.unsafeMult (A.unsafeLog left) right
        sqrt val@BigInteger{} = A.sqrt $ _forceReal val
        sqrt (BigReal realVal) = MI.withValue $ if realVal < 0 then complex 0 sqrtVal else real sqrtVal
            where sqrtVal = sqrt $ abs realVal
        sqrt val = A.pow val half
        log (BigInteger 0) = MI.withError MI.InvalidValue
        log val@BigInteger{} = A.log $ _forceReal val
        log (BigReal realVal)
            | realVal < 0 = MI.withValue $ complex (log $ -realVal) pi
            | otherwise = (MI.withValue . real) $ log realVal
        log com@BigComplex{} = MI.withValue $ complex realPart imag0Part
            where realPart = (log . _realPart) $ unsafeAbsol com
                  imag0Part = (_realPart . MI.value) $ sarg com
        log quat@(BigQuaternion realVal imag0Val imag1Val imag2Val) = MI.withValue $ A.unsafePlus (A.unsafeLog b) (A.unsafeMult (unsafeNorm a) ((MI.value . sacos . MI.value) $ A.div (real realVal) b))
            where a = sVectorPart quat
                  b = unsafeAbsol quat
        logBase base val = MI.errBinCombine (A.log val) (A.log base) A.div
        exp val@BigInteger{} = A.exp $ _forceReal val
        exp (BigReal realVal) = (MI.withValue . real) $ exp realVal
        exp (BigComplex realVal imag0Val) = A.mult (A.unsafeExp $ real realVal) (complex (cos imag0Val) (sin imag0Val))
        exp quat@BigQuaternion{} = A.mult i h
            where a = sVectorPart quat
                  b = sRealCoef quat
                  c = unsafeAbsol quat
                  d = MI.value $ scos c
                  e = unsafeNorm a
                  f = MI.value $ ssin c
                  g = A.unsafeMult e f
                  h = A.unsafePlus d g
                  i = A.unsafeExp b

    sconj :: BigScalar -> BigScalar
    sconj (BigInteger intVal) = integer intVal
    sconj (BigReal realVal) = real realVal
    sconj (BigComplex realVal imag0Val) = complex realVal (-imag0Val)
    sconj (BigQuaternion realVal imag0Val imag1Val imag2Val) = quaternion realVal (-imag0Val) (-imag1Val) (-imag2Val)

    sarg :: BigScalar -> MI.ComputationResult BigScalar
    sarg val@BigInteger{} = sarg $ _forceReal val
    sarg BigReal{} = MI.withValue zero
    sarg (BigComplex 0 imag0Val)
        | imag0Val < 0 = A.div (A.unsafeNeg spi) two
        | imag0Val > 0 = A.div spi two
        | otherwise = MI.withValue zero
    sarg (BigComplex realVal imag0Val) = MI.withValue $ real $ atan2 imag0Val realVal
    sarg _ = MI.withError MI.InvalidValue

    sVectorPart :: BigScalar -> BigScalar
    sVectorPart val@BigInteger{} = zero
    sVectorPart (BigReal realVal) = zero
    sVectorPart (BigComplex _ imag0Val) = complex 0 imag0Val
    sVectorPart (BigQuaternion _ imag0Val imag1Val imag2Val) = quaternion 0 imag0Val imag1Val imag2Val

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
    stan val = MI.errBinCombine sinValue cosValue A.div
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
    stanh val = MI.errBinCombine sinhValue coshValue A.div
        where sinhValue = ssinh val
              coshValue = scosh val

    sasin :: BigScalar -> MI.ComputationResult BigScalar
    sasin val@BigInteger{} = sasin $ _forceReal val
    sasin val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ asin realVal
        | otherwise = sasin $ _forceComplex val
    sasin com@BigComplex{} = MI.withValue $ A.unsafeMult (A.unsafeNeg imagI) (A.unsafeLog $ A.unsafePlus (A.unsafeMult imagI com) (A.unsafeSqrt $ A.unsafeMinus one (A.unsafeMult com com)))
    sasin _ = MI.withError MI.InvalidValue

    sacos :: BigScalar -> MI.ComputationResult BigScalar
    sacos val@BigInteger{} = sacos $ _forceReal val
    sacos val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ acos realVal
        | otherwise = sacos $ _forceComplex val
    sacos com@BigComplex{} = MI.withValue $ A.unsafeMult (A.unsafeNeg imagI) (A.unsafeLog $ A.unsafePlus com (A.unsafeSqrt $ A.unsafeMinus (A.unsafeMult com com) one))
    sacos _ = MI.withError MI.InvalidValue

    satan :: BigScalar -> MI.ComputationResult BigScalar
    satan val@BigInteger{} = satan $ _forceReal val
    satan val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ atan realVal
        | otherwise = satan $ _forceComplex val
    satan com@BigComplex{} = MI.withValue $ A.unsafeMult (A.unsafeMult imagI half) (A.unsafeMinus (A.unsafeLog $ A.unsafeMinus one (A.unsafeMult imagI com)) (A.unsafeLog $ A.unsafePlus one (A.unsafeMult imagI com)))
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
    sasinh com@BigComplex{} = MI.errBinResolveLeft asinValue imagI A.div
        where asinValue = sasin $ A.unsafeMult imagI com
    sasinh _ = MI.withError MI.InvalidValue

    sacosh :: BigScalar -> MI.ComputationResult BigScalar
    sacosh val@BigInteger{} = sacosh $ _forceReal val
    sacosh val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ acosh realVal
        | otherwise = sacosh $ _forceComplex val
    sacosh com@BigComplex{} = MI.binResolveRight imagI acosValue A.unsafeMult
        where acosValue = sacos com
    sacosh _ = MI.withError MI.InvalidValue

    satanh :: BigScalar -> MI.ComputationResult BigScalar
    satanh val@BigInteger{} = satanh $ _forceReal val
    satanh val@(BigReal realVal)
        | ((-1) <= realVal) && (realVal <= 1) = (MI.withValue . real) $ atanh realVal
        | otherwise = satanh $ _forceComplex val
    satanh com@BigComplex{} = MI.errBinResolveLeft atanValue imagI A.div
        where atanValue = satan $ A.unsafeMult imagI com
    satanh _ = MI.withError MI.InvalidValue

    instance A.Comparable BigScalar where
        min (BigInteger leftInt) (BigInteger rightInt) = (MI.withValue . integer) $ min leftInt rightInt
        min left@BigInteger{} right = A.min (_forceReal left) right
        min left right@BigInteger{} = A.min left (_forceReal right)
        min (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ min leftReal rightReal
        min _ _ = MI.withError MI.InvalidValue
        max (BigInteger leftInt) (BigInteger rightInt) = (MI.withValue . integer) $ max leftInt rightInt
        max left@BigInteger{} right = A.max (_forceReal left) right
        max left right@BigInteger{} = A.max left (_forceReal right)
        max (BigReal leftReal) (BigReal rightReal) = (MI.withValue . real) $ max leftReal rightReal
        max _ _ = MI.withError MI.InvalidValue
        less left right = MI.unResolve result (LT==)
            where result = _scompare left right

    _scompare :: BigScalar -> BigScalar -> MI.ComputationResult Ordering
    _scompare left@BigInteger{} right = _scompare (_forceReal left) right
    _scompare left right@BigInteger{} = _scompare left (_forceReal right)
    _scompare (BigReal leftReal) (BigReal rightReal) = MI.withValue $ compare leftReal rightReal
    _scompare _ _ = MI.withError MI.InvalidValue

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
              
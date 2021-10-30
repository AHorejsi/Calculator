module Scalar (
    Scalar(
        Real,
        Complex,
        Quaternion,
        SFail
    ),
    zero,
    one,
    negOne,
    real,
    message,
    divideByZeroError,
    zeroToPowerOfZeroError,
    imag0,
    imag1,
    imag2,
    sconj,
    snorm,
    show
) where

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
    } | SFail {
        _fmsg :: String
    }

    instance (Num a, Show a, Eq a, Ord a) => Show (Scalar a) where
        show (Real real) = show real
        show (Complex 0 0) = "0"
        show (Complex real imag0) = (fst realCoef) ++ (fst imag0Coef)
            where realCoef = _realCoef real
                  imag0Coef = if snd realCoef then _loneImagCoef imag0 "i" else _imagCoef imag0 "i"
        show (Quaternion 0 0 0 0) = "0"
        show (Quaternion real imag0 imag1 imag2) = (fst realCoef) ++ (fst imag0Coef) ++ (fst imag1Coef) ++ (fst imag2Coef)
            where realCoef = _realCoef real
                  imag0Coef = if snd realCoef then _loneImagCoef imag0 "i" else _imagCoef imag0 "i"
                  imag1Coef = if snd imag0Coef then _loneImagCoef imag1 "j" else _imagCoef imag1 "j"
                  imag2Coef = if snd imag1Coef then _loneImagCoef imag2 "k" else _imagCoef imag2 "k"
        show (SFail msg) = show "Error: " ++ msg

    instance (Num a, Eq a) => Eq (Scalar a) where
        (==) (Real leftReal) (Real rightReal) = leftReal == rightReal
        (==) (Real leftReal) (Complex rightReal rightImag0) = leftReal == rightReal && 0 == rightImag0
        (==) (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = leftReal == rightReal && 0 == rightImag0 && 0 == rightImag1 && 0 == rightImag2
        (==) (Complex leftReal leftImag0) (Real rightReal) = leftReal == rightReal && 0 == leftImag0
        (==) (Complex leftReal leftImag0) (Complex rightReal rightImag0) = leftReal == rightReal && leftImag0 == rightImag0
        (==) (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = leftReal == rightReal && leftImag0 == rightImag0 && 0 == rightImag1 && 0 == rightImag2
        (==) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Real rightReal) = leftReal == rightReal && 0 == leftImag0 && 0 == leftImag1 && 0 == leftImag2
        (==) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = leftReal == rightReal && leftImag0 == rightImag0 && 0 == leftImag1 && 0 == leftImag2
        (==) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = leftReal == rightReal && leftImag0 == rightImag0 && leftImag1 == rightImag1 && leftImag2 == rightImag2
        (==) _ _ = False
        (/=) left right = not (left == right)

    _realCoef :: (Num a, Eq a, Show a) => a -> (String, Bool)
    _realCoef value
        | (0 == value) = ("", True)
        | otherwise = (show value, False)

    _imagCoef :: (Num a, Eq a, Show a, Ord a) => a -> String -> (String, Bool)
    _imagCoef value imagUnit
        | (value < 0) = (" - " ++ coef, False)
        | (value > 0) = (" + " ++ coef, False)
        | otherwise = ("", True)
        where coef = (_handleOne value) ++ imagUnit
    
    _loneImagCoef :: (Num a, Eq a, Show a, Ord a) => a -> String -> (String, Bool)
    _loneImagCoef value imagUnit
        | (value < 0) = ("-" ++ coef, False)
        | (value > 0) = (coef, False)
        | otherwise = ("", True)
        where coef = (_handleOne value) ++ imagUnit

    _handleOne :: (Num a, Eq a, Show a, Ord a) => a -> String
    _handleOne value = if 1 == absVal then "" else show absVal
        where absVal = abs value

    zero :: (RealFloat a) => Scalar a
    zero = Real 0

    one :: (RealFloat a) => Scalar a
    one = Real 1

    negOne :: (RealFloat a) => Scalar a
    negOne = negate one

    _invalid :: (Num a) => Scalar a
    _invalid = SFail "Invalid Innput"

    divideByZeroError :: (Num a) => Scalar a
    divideByZeroError = SFail "Cannot divide by zero"

    zeroToPowerOfZeroError :: (RealFloat a) => Scalar a
    zeroToPowerOfZeroError = SFail "Cannot compute zero to the power of zero"

    message :: Scalar a -> Maybe String
    message (SFail msg) = Just msg
    message _ = Nothing

    real :: (RealFloat a) => Scalar a -> Scalar a
    real (Real real) = Real real
    real (Complex real _) = Real real
    real (Quaternion real _ _ _) = Real real
    real (SFail _) = _invalid

    imag0 :: (RealFloat a) => Scalar a -> Scalar a
    imag0 (Real _) = zero
    imag0 (Complex _ imag0) = Real imag0
    imag0 (Quaternion _ imag0 _ _) = Real imag0
    imag0 (SFail _) = _invalid

    imag1 :: (RealFloat a) => Scalar a -> Scalar a
    imag1 (Real _) = zero
    imag1 (Complex _ _) = zero
    imag1 (Quaternion _ _ imag1 _) = Real imag1
    imag1 (SFail _) = _invalid

    imag2 :: (RealFloat a) => Scalar a -> Scalar a
    imag2 (Real _) = zero
    imag2 (Complex _ _) = zero
    imag2 (Quaternion _ _ _ imag2) = Real imag2
    imag2 (SFail _) = _invalid

    instance (RealFloat a) => Num (Scalar a) where
        (+) (Real leftReal) (Real rightReal) = Real $ leftReal + rightReal
        (+) (Real leftReal) (Complex rightReal rightImag0) = Complex (leftReal + rightReal) rightImag0
        (+) (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
        (+) (Complex leftReal leftImag0) (Real rightReal) = Complex (leftReal + rightReal) leftImag0
        (+) (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (leftReal + rightReal) (leftImag0 + rightImag0)
        (+) (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
        (+) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Real rightReal) = Quaternion (leftReal + rightReal) leftImag0 leftImag1 leftImag2
        (+) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = Quaternion (leftReal + rightReal) (leftImag0 + rightImag0) leftImag1 leftImag2
        (+) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
        (+) _ _ = _invalid
        (-) (Real leftReal) (Real rightReal) = Real $ leftReal - rightReal
        (-) (Real leftReal) (Complex rightReal rightImag0) = Complex (leftReal - rightReal) (-rightImag0)
        (-) (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)
        (-) (Complex leftReal leftImag0) (Real rightReal) = Complex (leftReal - rightReal) leftImag0
        (-) (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (leftReal - rightReal) (leftImag0 - rightImag0)
        (-) (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)
        (-) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Real rightReal) = Quaternion (leftReal - rightReal) leftImag0 leftImag1 leftImag2
        (-) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2
        (-) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)
        (-) _ _ = _invalid
        (*) (Real leftReal) (Real rightReal) = Real $ leftReal * rightReal
        (*) (Real leftReal) (Complex rightReal rightImag0) = Complex (leftReal * rightReal) (leftReal * rightImag0)
        (*) (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)
        (*) (Complex leftReal leftImag0) (Real rightReal) = Complex (leftReal * rightReal) (leftImag0 * rightReal)
        (*) (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex realResult imag0Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
        (*) (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
                  imag1Result = leftReal * rightImag1 - leftImag0 * rightImag2
                  imag2Result = leftReal * rightImag2 + leftImag0 * rightImag1
        (*) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Real rightReal) = Quaternion (leftReal * rightReal) (leftImag0 * rightReal) (leftImag1 * rightReal) (leftImag2 * rightReal)
        (*) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = Quaternion realResult imag0Result imag1Result imag2Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
                  imag1Result = leftImag1 * rightReal + leftImag2 * rightImag0
                  imag2Result = -leftImag1 * rightImag0 + leftImag2 * rightReal
        (*) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
            where realResult = leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2
                  imag0Result = leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1
                  imag1Result = leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0
                  imag2Result = leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal
        (*) _ _ = _invalid
        negate = (negOne*)
        abs (Real real) = Real $ abs real
        abs (Complex real imag0) = Real $ sqrt $ real * real + imag0 * imag0
        abs (Quaternion real imag0 imag1 imag2) = Real $ sqrt $ real * real + imag0 * imag0 * imag1 * imag1 + imag2 * imag2
        abs _ = _invalid
        signum (Real real) = if real < 0 then negOne else if real > 0 then one else zero
        signum (Complex real imag0) = if real < 0 then negOne else if real > 0 then one else Real $ signum imag0
        signum _ = _invalid
        fromInteger value = Real $ fromIntegral value

    instance (RealFloat a) => Fractional (Scalar a) where
        (/) _ (Real 0) = divideByZeroError
        (/) _ (Complex 0 0) = divideByZeroError
        (/) _ (Quaternion 0 0 0 0) = divideByZeroError
        (/) (Real leftReal) (Real rightReal) = Real $ leftReal / rightReal
        (/) (Real leftReal) (Complex rightReal rightImag0) = numerator / denominator
            where left = Real leftReal
                  right = Complex rightReal rightImag0
                  rightConj = sconj right
                  numerator = left * rightConj
                  denominator = real $ right * rightConj
        (/) (Real leftReal) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
            where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
                  realResult = leftReal * rightReal / denominator
                  imag0Result = -leftReal * rightImag0 / denominator
                  imag1Result = -leftReal * rightImag1 / denominator
                  imag2Result = -leftReal * rightImag2 / denominator
        (/) (Complex leftReal leftImag0) (Real rightReal) = Complex (leftReal / rightReal) (leftImag0 / rightReal)
        (/) (Complex leftReal leftImag0) (Complex rightReal rightImag0) = numerator / denominator
            where left = Complex leftReal leftImag0
                  right = Complex rightReal rightImag0
                  rightConj = sconj right
                  numerator = left * rightConj
                  denominator = real right * rightConj
        (/) (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
            where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
                  realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
                  imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
                  imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
                  imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator
        (/) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Real rightReal) = Quaternion (leftReal / rightReal) (leftImag0 / rightReal) (leftImag1 / rightReal) (leftImag2 / rightReal)
        (/) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = numerator / denominator
            where left = Quaternion leftReal leftImag0 leftImag1 leftImag2
                  right = Complex rightReal rightImag0
                  rightConj = sconj right
                  numerator = left * rightConj
                  denominator = real $ right * rightConj
        (/) (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
            where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
                  realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
                  imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
                  imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
                  imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator
        (/) _ _ = _invalid
        recip (Real real) = one / value
            where value = Real real
        recip (Complex real imag0) = one / value
            where value = Complex real imag0
        recip (Quaternion real imag0 imag1 imag2) = conj / denominator
            where value = Quaternion real imag0 imag1 imag2
                  conj = sconj value
                  denominator = Real $ real * real + imag0 * imag0 + imag1 * imag1 + imag2 * imag2
        recip _ = _invalid
        fromRational value = Real $ fromRational value

    instance (RealFloat a) => Floating (Scalar a) where
        pi = Real pi
        (**) (Real 0) (Real 0) = zeroToPowerOfZeroError
        (**) (Real 0) (Complex 0 0) = zeroToPowerOfZeroError
        (**) (Real 0) (Quaternion 0 0 0 0) = zeroToPowerOfZeroError
        (**) (Complex 0 0) (Real 0) = zeroToPowerOfZeroError
        (**) (Complex 0 0) (Complex 0 0) = zeroToPowerOfZeroError
        (**) (Complex 0 0) (Quaternion 0 0 0 0) = zeroToPowerOfZeroError
        (**) (Quaternion 0 0 0 0) (Real 0) = zeroToPowerOfZeroError
        (**) (Quaternion 0 0 0 0) (Complex 0 0) = zeroToPowerOfZeroError
        (**) (Quaternion 0 0 0 0) (Quaternion 0 0 0 0) = zeroToPowerOfZeroError
        (**) _ (Real 0) = one
        (**) _ (Complex 0 0) = one
        (**) _ (Quaternion 0 0 0 0) = one
        (**) (Real 0) _ = zero
        (**) (Complex 0 0) _ = zero
        (**) (Quaternion 0 0 0 0) _ = zero
        (**) (Real leftReal) (Real rightReal) = Real $ leftReal ** rightReal
        (**) (Real leftReal) (Complex rightReal rightImag0) = (Real $ leftReal ** rightReal) * (Complex (cos a) (sin a))
            where a = (log leftReal) * rightImag0

    sconj :: (Num a) => Scalar a -> Scalar a
    sconj (Real real) = Real real
    sconj (Complex real imag0) = Complex real (-imag0)
    sconj (Quaternion real imag0 imag1 imag2) = Quaternion real (-imag0) (-imag1) (-imag2)
    sconj _ = _invalid

    snorm :: (RealFloat a, Eq a) => Scalar a -> Scalar a
    snorm value = value / (abs value)
    

    arg :: (RealFloat a) => Scalar a -> Scalar a
    arg (Complex _ 0) = zero
    arg (Complex 0 imag0) = if imag0 < 0 then Real $ (-pi) / 2 else if imag0 > 0 then Real $ pi / 2 else zero
    arg (Complex real imag0) = Real $ atan2 imag0 real
    arg _ = _invalid


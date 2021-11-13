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
    smessage,
    divideByZeroError,
    zeroToPowerOfZeroError,
    real,
    imag0,
    imag1,
    imag2,
    imagI,
    imagJ,
    imagK,
    (+),
    (-),
    (*),
    negate,
    abs,
    signum,
    fromInteger,
    (/),
    recip,
    fromRational,
    pi,
    (**),
    log,
    exp,
    sin,
    cos,
    tan,
    sinh,
    cosh,
    tanh,
    asin,
    acos,
    atan,
    asinh,
    acosh,
    atanh,
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
        _smsg :: String
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
    negOne = Real $ -1

    imagI :: (RealFloat a) => Scalar a
    imagI = Complex 0 1

    imagJ :: (RealFloat a) => Scalar a
    imagJ = Quaternion 0 0 1 0

    imagK :: (RealFloat a) => Scalar a
    imagK = Quaternion 0 0 0 1

    _invalid :: (Num a) => Scalar a
    _invalid = SFail "Invalid Innput"

    divideByZeroError :: (Num a) => Scalar a
    divideByZeroError = SFail "Cannot divide by zero"

    zeroToPowerOfZeroError :: (RealFloat a) => Scalar a
    zeroToPowerOfZeroError = SFail "Cannot compute zero to the power of zero"

    smessage :: Scalar a -> Maybe String
    smessage (SFail msg) = Just msg
    smessage _ = Nothing

    real :: (RealFloat a) => Scalar a -> a
    real (Real real) = real
    real (Complex real _) = real
    real (Quaternion real _ _ _) = real
    real (SFail _) = 0.0 / 0.0

    imag0 :: (RealFloat a) => Scalar a -> a
    imag0 (Real _) = 0
    imag0 (Complex _ imag0) = imag0
    imag0 (Quaternion _ imag0 _ _) = imag0
    imag0 (SFail _) = 0.0 / 0.0

    imag1 :: (RealFloat a) => Scalar a -> a
    imag1 (Real _) = 0
    imag1 (Complex _ _) = 0
    imag1 (Quaternion _ _ imag1 _) = imag1
    imag1 (SFail _) = 0.0 / 0.0

    imag2 :: (RealFloat a) => Scalar a -> a
    imag2 (Real _) = 0
    imag2 (Complex _ _) = 0
    imag2 (Quaternion _ _ _ imag2) = imag2
    imag2 (SFail _) = 0.0 / 0.0

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
                  denominator = Real $ _creal $ right * rightConj
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
                  denominator = Real $ _creal $ right * rightConj
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
                  denominator = Real $ _creal $ right * rightConj
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
        (**) (Complex leftReal leftImag0) (Real rightReal) = (r ** right) * Complex (cos a) (sin a)
            where left = Complex leftReal leftImag0
                  right = Real rightReal
                  r = abs left
                  theta = arg left
                  a = _rreal $ right * theta
        (**) (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (d * (cos c)) (d * (sin c))
            where left = Complex leftReal leftImag0
                  a = leftReal * leftReal + leftImag0 * leftImag0
                  b = _rreal $ arg left
                  c = rightReal * b + 0.5 * rightImag0 * (log a)
                  d = (a ** (rightReal / 2)) * (exp $ -rightImag0 * b)
        (**) left right = exp $ (log left) * right
        exp (Real real) = Real $ exp real
        exp (Complex real imag0) = (exp 1) ** com
            where com = Complex real imag0
        exp (Quaternion real imag0 imag1 imag2) = (exp $ Real real) * ((cos b) + ((snorm a) * (sin b)))
            where quat = Quaternion real imag0 imag1 imag2
                  a = Quaternion 0 imag0 imag1 imag2
                  b = abs quat
        exp _ = _invalid
        log (Real real) = Real $ log real
        log (Complex real imag0) = Complex (log $ _rreal $ abs com) (_rreal $ arg com)
            where com = Complex real imag0
        log (Quaternion real 0 0 0) = log $ Real real
        log (Quaternion real imag0 0 0) = log $ Complex real imag0
        log (Quaternion real imag0 imag1 imag2) = (log b) + (snorm a) * (acos $ (Real real) / b)
            where quat = Quaternion real imag0 imag1 imag2
                  a = Quaternion 0 imag0 imag1 imag2
                  b = abs quat
        log _ = _invalid
        sin (Real real) = Real $ sin real
        sin (Complex real imag0) = Complex ((sin real) * (cosh imag0)) ((cos real) * (sinh imag0))
        sin _ = _invalid
        cos (Real real) = Real $ cos real
        cos (Complex real imag0) = Complex ((cos real) * (cosh imag0)) (-(sin real) * (sinh imag0))
        cos _ = _invalid
        tan value = (sin value) / (cos value)
        sinh (Real real) = Real $ sinh real
        sinh (Complex real imag0) = Complex ((sinh real) * (cos imag0)) ((cosh real) * (sin imag0))
        sinh _ = _invalid
        cosh (Real real) = Real $ cosh real
        cosh (Complex real imag0) = Complex ((cosh real) * (cos imag0)) ((sinh real) * (sin imag0))
        cosh _ = _invalid
        tanh value = (sinh value) / (cosh value)
        asin (Real real) = Real $ asin real
        asin (Complex real imag0) = -imagI * (log $ (imagI * com) + (sqrt $ one - (com * com)))
            where com = Complex real imag0
        asin _ = _invalid
        acos (Real real) = Real $ acos real
        acos (Complex real imag0) = (pi / 2) + (imagI * (log $ (imagI * com) + (sqrt $ one - (com * com))))
            where com = Complex real imag0
        acos _ = _invalid
        atan (Real real) = Real $ atan real
        atan (Complex real imag0) = (imagI / 2) * ((log $ one - (imagI * com)) - (log $ one + (imagI * com)))
            where com = Complex real imag0
        atan _ = _invalid
        asinh (Real real) = Real $ asinh real
        asinh (Complex real imag0) = (asin $ imagI * com) / imagI
            where com = Complex real imag0
        asinh _ = _invalid
        acosh (Real real) = Real $ acosh real
        acosh (Complex real imag0) = imagI * (acos com)
            where com = Complex real imag0
        acosh _ = _invalid
        atanh (Real real) = Real $ atanh real
        atanh (Complex real imag0) = (atan $ imagI * com) / imagI
            where com = Complex real imag0
        atanh _ = _invalid

    sconj :: (Num a) => Scalar a -> Scalar a
    sconj (Real real) = Real real
    sconj (Complex real imag0) = Complex real (-imag0)
    sconj (Quaternion real imag0 imag1 imag2) = Quaternion real (-imag0) (-imag1) (-imag2)
    sconj _ = _invalid

    snorm :: (RealFloat a, Eq a) => Scalar a -> Scalar a
    snorm value = value / (abs value)

    arg :: (RealFloat a) => Scalar a -> Scalar a
    arg (Real _) = zero
    arg (Complex _ 0) = zero
    arg (Complex 0 imag0) = if imag0 < 0 then Real $ (-pi) / 2 else if imag0 > 0 then Real $ pi / 2 else zero
    arg (Complex real imag0) = Real $ atan2 imag0 real
    arg _ = _invalid


module Imaginary (

) where

    data Complex a = Complex {
        creal :: a,
        cimag0 :: a
    } deriving (Eq, Show)

    data Quaternion a = Quaternion {
        qreal :: a,
        qimag0 :: a,
        qimag1 :: a,
        qimag2 :: a
    } deriving (Eq, Show)

    c0 :: (Num a) => Complex a
    c0 = Complex 0 0

    c1 :: (Num a) => Complex a
    c1 = Complex 1 0

    cI :: (Num a) => Complex a
    cI = Complex 0 1

    cNaN :: (Fractional a) => Complex a
    cNaN = Complex (0 / 0) (0 / 0)

    cInf :: (Fractional a) => Complex a
    cInf = Complex (1 / 0) 0

    cNegInf :: (Fractional a) => Complex a
    cNegInf = cneg cInf

    q0 :: (Num a) => Quaternion a
    q0 = Quaternion 0 0 0 0

    q1 :: (Num a) => Quaternion a
    q1 = Quaternion 1 0 0 0

    qI :: (Num a) => Quaternion a
    qI = Quaternion 0 1 0 0

    qJ :: (Num a) => Quaternion a
    qJ = Quaternion 0 0 1 0

    qK :: (Num a) => Quaternion a
    qK = Quaternion 0 0 0 1

    qNaN :: (Fractional a) => Quaternion a
    qNaN = Quaternion (0 / 0) (0 / 0) (0 / 0) (0 / 0)

    qInf :: (Fractional a) => Quaternion a
    qInf = Quaternion (1 / 0) 0 0 0
    
    qNegInf :: (Fractional a) => Quaternion a
    qNegInf = qneg qInf

    rplusc :: (Num a) => a -> Complex a -> Complex a
    rplusc leftReal (Complex rightReal rightImag0) = Complex (leftReal + rightReal) rightImag0

    cplusr :: (Num a) => Complex a -> a -> Complex a
    cplusr left right = rplusc right left

    cplusc :: (Num a) => Complex a -> Complex a -> Complex a
    cplusc (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (leftReal + rightReal) (leftImag0 + rightImag0)

    rplusq :: (Num a) => a -> Quaternion a -> Quaternion a
    rplusq leftReal (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2

    qplusr :: (Num a) => Quaternion a -> a -> Quaternion a
    qplusr left right = rplusq right left

    cplusq :: (Num a) => Complex a -> Quaternion a -> Quaternion a
    cplusq (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2

    qplusc :: (Num a) => Quaternion a -> Complex a -> Quaternion a
    qplusc left right = cplusq right left

    qplusq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
    qplusq (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)

    rminusc :: (Num a) => a -> Complex a -> Complex a
    rminusc leftReal (Complex rightReal rightImag0) = Complex (leftReal - rightReal) (-rightImag0)

    cminusr :: (Num a) => Complex a -> a -> Complex a
    cminusr (Complex leftReal leftImag0) rightReal = Complex (leftReal - rightReal) leftImag0

    cminusc :: (Num a) => Complex a -> Complex a -> Complex a
    cminusc (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (leftReal - rightReal) (leftImag0 - rightImag0)

    rminusq :: (Num a) => a -> Quaternion a -> Quaternion a
    rminusq leftReal (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)

    qminusr :: (Num a) => Quaternion a -> a -> Quaternion a
    qminusr (Quaternion leftReal leftImag0 leftImag1 leftImag2) rightReal = Quaternion (leftReal + rightReal) leftImag0 leftImag1 leftImag2

    cminusq :: (Num a) => Complex a -> Quaternion a -> Quaternion a
    cminusq (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)

    qminusc :: (Num a) => Quaternion a -> Complex a -> Quaternion a
    qminusc (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2

    qminusq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
    qminusq (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)

    rmultc :: (Num a) => a -> Complex a -> Complex a
    rmultc leftReal (Complex rightReal rightImag0) = Complex (leftReal * rightReal) (leftReal * rightImag0)

    cmultr :: (Num a) => Complex a -> a -> Complex a
    cmultr left right = rmultc right left

    cmultc :: (Num a) => Complex a -> Complex a -> Complex a
    cmultc (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex realResult imag0Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal

    rmultq :: (Num a) => a -> Quaternion a -> Quaternion a
    rmultq leftReal (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)

    qmultr :: (Num a) => Quaternion a -> a -> Quaternion a
    qmultr left right = rmultq right left

    cmultq :: (Num a) => Complex a -> Quaternion a -> Quaternion a
    cmultq (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 + rightReal
              imag1Result = leftReal * rightImag1 - leftImag0 * rightImag2
              imag2Result = leftReal * rightImag2 + leftImag0 * rightImag1

    qmultc :: (Num a) => Quaternion a -> Complex a -> Quaternion a
    qmultc (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = Quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal
              imag1Result = leftImag1 * rightReal + leftImag2 * rightImag0
              imag2Result = -leftImag1 * rightImag0 + leftImag2 * rightReal

    qmultq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
    qmultq (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
        where realResult = leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2
              imag0Result = leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1
              imag1Result = leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0
              imag2Result = leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal

    rdivc :: (Fractional a) => a -> Complex a -> Complex a
    rdivc left right = cdivr numerator denominator
        where rightConj = cconj right
              numerator = rmultc left rightConj
              denominator = creal $ cmultc right rightConj

    cdivr :: (Fractional a) => Complex a -> a -> Complex a
    cdivr (Complex leftReal leftImag0) rightReal = Complex (leftReal / rightReal) (leftImag0 / rightReal)

    cdivc :: (Fractional a) => Complex a -> Complex a -> Complex a
    cdivc left right = cdivr numerator denominator
        where rightConj = cconj right
              numerator = cmultc left rightConj
              denominator = creal $ cmultc right rightConj

    rdivq :: (Fractional a) => a -> Quaternion a -> Quaternion a
    rdivq leftReal (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = leftReal * rightReal / denominator
              imag0Result = -leftReal * rightImag0 / denominator
              imag1Result = -leftReal * rightImag1 / denominator
              imag2Result = -leftReal * rightImag2 / denominator

    qdivr :: (Fractional a) => Quaternion a -> a -> Quaternion a
    qdivr (Quaternion leftReal leftImag0 leftImag1 leftImag2) rightReal = Quaternion (leftReal / rightReal) (leftImag0 / rightReal) (leftImag1 / rightReal) (leftImag2 / rightReal)

    cdivq :: (Fractional a) => Complex a -> Quaternion a -> Quaternion a
    cdivq (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0) / denominator
              imag0Result = (leftImag0 * rightReal - leftReal * rightImag0) / denominator
              imag1Result = (-leftReal * rightImag1 - leftImag0 * rightImag2) / denominator
              imag2Result = (leftImag0 * rightImag1 - leftReal * rightImag2) / denominator

    qdivc :: (Fractional a) => Quaternion a -> Complex a -> Quaternion a
    qdivc left right = qdivr numerator denominator
        where rightConj = cconj right
              numerator = qmultc left rightConj
              denominator = creal $ cmultc right rightConj

    qdivq :: (Fractional a) => Quaternion a -> Quaternion a -> Quaternion a
    qdivq (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion realResult imag0Result imag1Result imag2Result
        where denominator = rightReal * rightReal + rightImag0 * rightImag0 + rightImag1 * rightImag1 + rightImag2 * rightImag2
              realResult = (leftReal * rightReal + leftImag0 * rightImag0 + leftImag1 * rightImag1 + leftImag2 * rightImag2) / denominator
              imag0Result = (leftReal * rightImag0 - leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) / denominator
              imag1Result = (leftReal * rightImag1 + leftImag0 * rightImag2 - leftImag1 * rightReal - leftImag2 * rightImag0) / denominator
              imag2Result = (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 - leftImag2 * rightReal) / denominator

    cconj :: (Num a) => Complex a -> Complex a
    cconj (Complex real imag0) = Complex real (-imag0)

    qconj :: (Num a) => Quaternion a -> Quaternion a
    qconj (Quaternion real imag0 imag1 imag2) = Quaternion real (-imag0) (-imag1) (-imag2)

    cneg :: (Num a) => Complex a -> Complex a
    cneg = rmultc (-1)

    qneg :: (Num a) => Quaternion a -> Quaternion a
    qneg = rmultq (-1)

    cabs :: (Floating a) => Complex a -> a
    cabs (Complex real imag0) = sqrt $ real * real + imag0 * imag0

    qabs :: (Floating a) => Quaternion a -> a
    qabs (Quaternion real imag0 imag1 imag2) = sqrt $ real * real + imag0 * imag0 + imag1 * imag1 + imag2 * imag2

    cnorm :: (Floating a) => Complex a -> Complex a
    cnorm com = cdivr com (cabs com)

    qnorm :: (Floating a) => Quaternion a -> Quaternion a
    qnorm quat = qdivr quat (qabs quat)

    cinv :: (Fractional a) => Complex a -> Complex a
    cinv = rdivc 1

    qinv :: (Fractional a) => Quaternion a -> Quaternion a
    qinv (Quaternion real imag0 imag1 imag2) = qdivr conj denominator
        where quat = Quaternion real imag0 imag1 imag2
              denominator = real * real + imag0 * imag0 + imag1 * imag1 + imag2 * imag2
              conj = qconj quat

    rpowc :: (RealFloat a, Eq a) => a -> Complex a -> Complex a
    rpowc leftReal (Complex rightReal rightImag0)
        | (0 == rightReal && 0 == rightImag0) = c1
        | (0 == leftReal) = c0
        | otherwise = rmultc (leftReal ** rightReal) (Complex (cos a) (sin a))
        where a = (log leftReal) * rightImag0

    cpowr :: (RealFloat a, Eq a) => Complex a -> a -> Complex a
    cpowr left right
        | (0 == right) = c1
        | (0 == (creal left) && 0 == (cimag0 left)) = c0
        | otherwise = rmultc ((cabs left) ** right) (Complex (cos a) (sin a))
        where a = right * (carg left)

    cpowc :: (RealFloat a, Eq a) => Complex a -> Complex a -> Complex a
    cpowc left (Complex rightReal rightImag0)
        | (0 == rightReal && 0 == rightImag0) = c1
        | (0 == leftReal && 0 == leftImag0) = c0
        | otherwise = Complex (d * (cos c)) (d * (sin c))
        where leftReal = creal left
              leftImag0 = cimag0 left
              a = leftReal * leftReal + leftImag0 * leftImag0
              b = carg left
              c = rightReal * b + 0.5 * rightImag0 * (log a)
              d = (a ** (rightReal / 2)) * (exp $ (-rightImag0) * b)

    rpowq :: (RealFloat a, Eq a) => a -> Quaternion a -> Quaternion a
    rpowq left right
        | (0 == rightReal && 0 == rightImag0 && 0 == rightImag1 && 0 == rightImag2) = q1
        | (0 == left) = q0
        | otherwise = qexp $ rmultq (log left) right
        where rightReal = qreal right
              rightImag0 = qimag0 right
              rightImag1 = qimag1 right
              rightImag2 = qimag2 right

    qpowr :: (RealFloat a, Eq a) => Quaternion a -> a -> Quaternion a
    qpowr left right
        | (0 == right) = q1
        | (0 == leftReal && 0 == leftImag0 && 0 == leftImag1 && 0 == leftImag2) = q0
        | otherwise = qexp $ qmultr (qlog left) right
        where leftReal = qreal left
              leftImag0 = qimag0 left
              leftImag1 = qimag1 left
              leftImag2 = qimag2 left

    cpowq :: (RealFloat a, Eq a) => Complex a -> Quaternion a -> Quaternion a
    cpowq left right
        | (0 == rightReal && 0 == rightImag0 && 0 == rightImag1 && 0 == rightImag2) = q1
        | (0 == leftReal && 0 == leftImag0) = q0
        | otherwise = qexp $ cmultq (clog left) right
        where leftReal = creal left
              leftImag0 = cimag0 left
              rightReal = qreal right
              rightImag0 = qimag0 right
              rightImag1 = qimag1 right
              rightImag2 = qimag2 right

    qpowc :: (RealFloat a, Eq a) => Quaternion a -> Complex a -> Quaternion a
    qpowc left right
        | (0 == rightReal && 0 == rightImag0) = q1
        | (0 == leftReal && 0 == leftImag0 && 0 == leftImag1 && 0 == leftImag2) = q0
        | otherwise = qexp $ qmultc (qlog left) right
        where leftReal = qreal left
              leftImag0 = qimag0 left
              leftImag1 = qimag1 left
              leftImag2 = qimag2 left
              rightReal = creal right
              rightImag0 = cimag0 right

    qpowq :: (RealFloat a, Eq a) => Quaternion a -> Quaternion a -> Quaternion a
    qpowq left right
        | (0 == rightReal && 0 == rightImag0 && 0 == rightImag1 && 0 == rightImag2) = q1
        | (0 == leftReal && 0 == leftImag0 && 0 == leftImag1 && 0 == leftImag2) = q0
        | otherwise = qexp $ qmultq (qlog left) right
        where leftReal = qreal left
              leftImag0 = qimag0 left
              leftImag1 = qimag1 left
              leftImag2 = qimag2 left
              rightReal = qreal right
              rightImag0 = qimag0 right
              rightImag1 = qimag1 right
              rightImag2 = qimag2 right
    
    carg :: (RealFloat a, Eq a) => Complex a -> a
    carg com
        | not (0 == real && 0 == imag0) = atan2 imag0 real
        | otherwise = if imag0 < 0 then (-pi) / 2 else if imag0 > 0 then pi / 2 else 0.0
        where real = creal com
              imag0 = cimag0 com

    clog :: (RealFloat a, Eq a) => Complex a -> Complex a
    clog com
        | (0 == (creal com) && 0 == (cimag0 com)) = cNegInf
        | otherwise = Complex (log $ cabs com) (carg com)

    qlog :: (RealFloat a, Eq a) => Quaternion a -> Quaternion a
    qlog quat
        | (realIsZero && imag0IsZero && imag1IsZero && imag2IsZero) = qNegInf
        | (imag0IsZero && imag1IsZero && imag2IsZero) = Quaternion (log real) 0 0 0
        | (imag1IsZero && imag2IsZero) = Quaternion (creal asComplex) (cimag0 asComplex) 0 0
        | otherwise = rplusq (log a) (qmultr (qnorm $ Quaternion 0 imag0 imag1 imag2) (acos $ real / a))
        where real = qreal quat
              imag0 = qimag0 quat
              imag1 = qimag1 quat
              imag2 = qimag2 quat
              realIsZero = 0 == real
              imag0IsZero = 0 == imag0
              imag1IsZero = 0 == imag1
              imag2IsZero = 0 == imag2
              asComplex = clog $ Complex real imag0
              a = qabs quat

    clogBase :: (RealFloat a, Eq a) => a -> Complex a -> Complex a
    clogBase base com = cdivr (clog com) (log base)

    qlogBase :: (RealFloat a, Eq a) => a -> Quaternion a -> Quaternion a
    qlogBase base quat = qdivr (qlog quat) (log base)

    cexp :: (RealFloat a, Eq a) => Complex a -> Complex a
    cexp = rpowc $ exp 1

    qexp :: (RealFloat a, Eq a) => Quaternion a -> Quaternion a
    qexp quat
        | (realIsZero && imag0IsZero && imag1IsZero && imag2IsZero) = q1
        | (imag0IsZero && imag1IsZero && imag2IsZero) = Quaternion (exp real) 0 0 0
        | (imag1IsZero && imag2IsZero) = Quaternion (creal asComplex) (cimag0 asComplex) 0 0
        | otherwise = rmultq (exp real) (rplusq (cos b) (qmultr (qnorm a) (sin b)))
        where real = qreal quat
              imag0 = qimag0 quat
              imag1 = qimag1 quat
              imag2 = qimag2 quat
              realIsZero = 0 == real
              imag0IsZero = 0 == imag0
              imag1IsZero = 0 == imag1
              imag2IsZero = 0 == imag2
              asComplex = cexp $ Complex real imag0
              a = Quaternion 0 imag0 imag1 imag2
              b = qabs a

    csqrt :: (RealFloat a, Eq a) => Complex a -> Complex a
    csqrt com = cpowr com 0.5

    qsqrt :: (RealFloat a, Eq a) => Quaternion a -> Quaternion a
    qsqrt quat = qpowr quat 0.5

    csin :: (Floating a) => Complex a -> Complex a
    csin (Complex real imag0) = Complex ((sin real) * (cosh imag0)) ((cos real) * (sinh imag0))

    ccos :: (Floating a) => Complex a -> Complex a
    ccos (Complex real imag0) = Complex ((cos real) * (cosh imag0)) (-(sin real) * (sinh imag0))

    ctan :: (Floating a) => Complex a -> Complex a
    ctan com = cdivc (csin com) (ccos com)

    csinh :: (Floating a) => Complex a -> Complex a
    csinh (Complex real imag0) = Complex ((sinh real) * (cos imag0)) ((cosh real) * (sin imag0))

    ccosh :: (Floating a) => Complex a -> Complex a
    ccosh (Complex real imag0) = Complex ((cosh real) * (cos imag0)) ((sinh real) * (sin imag0))

    ctanh :: (Floating a) => Complex a -> Complex a
    ctanh com = cdivc (csinh com) (ccosh com)

    casin :: (RealFloat a) => Complex a -> Complex a
    casin com = cmultc a g
        where a = cneg cI
              b = cmultc com com
              c = rminusc 1 b
              d = csqrt c
              e = cmultc cI com
              f = cplusc d e
              g = clog f
    
    cacos :: (RealFloat a) => Complex a -> Complex a
    cacos com = rplusc (pi / 2) g
        where a = cmultc com com
              b = rminusc 1 a
              c = csqrt b
              d = cmultc cI com
              e = cplusc c d
              f = clog e
              g = cmultc cI f

    catan :: (RealFloat a) => Complex a -> Complex a
    catan com = cdivr g 2
        where a = cmultc cI com
              b = rminusc 1 a
              c = rplusc 1 a
              d = clog b
              e = clog c
              f = cminusc d e
              g = cmultc cI f

    casinh :: (RealFloat a) => Complex a -> Complex a
    casinh com = clog d
        where a = cmultc com com
              b = rplusc 1 a
              c = csqrt b
              d = cplusc com c

    cacosh :: (RealFloat a) => Complex a -> Complex a
    cacosh com = clog f
        where a = cplusr com 1
              b = cminusr com 1
              c = csqrt a
              d = csqrt b
              e = cmultc c d
              f = cplusc com e

    catanh :: (RealFloat a) => Complex a -> Complex a
    catanh com = cdivr e 2
        where a = rplusc 1 com
              b = rminusc 1 com
              c = clog a
              d = clog b
              e = cminusc c d

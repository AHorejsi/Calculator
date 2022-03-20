{-# LANGUAGE DeriveGeneric #-}

module MathEntity (
    MathEntity,
    UnaryEntityOperation,
    ErrableUnaryEntityOperation,
    BinaryEntityOperation,
    ErrableBinaryEntityOperation,
    scalarEntity,
    vectorEntity,
    listEntity,
    matrixEntity,
    boolEntity,
    isInteger,
    isReal,
    isComplex,
    isQuaternion,
    isScalar,
    isVector,
    isList,
    isMatrix,
    isBool,
    realCoef,
    imag0Coef,
    imag1Coef,
    imag2Coef,
    plus,
    minus,
    mult,
    scale,
    dot,
    cross,
    div,
    intDiv,
    mod,
    rem,
    lcm,
    gcd,
    neg,
    pow,
    sqrt,
    inv,
    conj,
    norm,
    arg,
    exp,
    log,
    log2,
    log10,
    logBase,
    sin,
    cos,
    tan,
    sinh,
    cosh,
    tanh,
    asin,
    acos,
    atan,
    atan2,
    asinh,
    acosh,
    atanh,
    min,
    max,
    less,
    lessEqual,
    greater,
    greaterEqual,
    ceiling,
    floor,
    even,
    odd,
    H.hash,
    H.hashWithSalt,
    (==),
    (/=),
    show
) where
    import Prelude hiding (div, mod, rem, lcm, gcd, sqrt, exp, log, logBase, sin, cos, tan, sinh, cosh, tanh, asin, acos, atan, atan2, asinh, acosh, atanh, min, max, ceiling, floor, even, odd)
    import qualified GHC.Generics as G
    import qualified Data.Hashable as H
    import qualified MathInfo as MI
    import qualified BigScalar as BS
    import qualified BigVector as BV
    import qualified BigList as BL
    import qualified BigMatrix as BM

    data MathEntity = ScalarEntity {
        _scalar :: BS.BigScalar
    } | VectorEntity {
        _vector :: BV.BigVector
    } | ListEntity {
        _list :: BL.BigList
    } | MatrixEntity {
        _matrix :: BM.BigMatrix
    } | BoolEntity {
        _bool :: Bool
    } deriving (Eq, G.Generic)

    instance H.Hashable MathEntity where
        hashWithSalt salt (ScalarEntity scalar) = H.hashWithSalt salt scalar
        hashWithSalt salt (VectorEntity vector) = H.hashWithSalt salt vector
        hashWithSalt salt (ListEntity list) = H.hashWithSalt salt list
        hashWithSalt salt (MatrixEntity matrix) = H.hashWithSalt salt matrix
        hashWithSalt salt (BoolEntity bool) = H.hashWithSalt salt bool

    instance Show MathEntity where
        show entity = "MathEntity:\n" ++ (show entity)

    type UnaryEntityOperation = MI.UnaryOperation MathEntity MathEntity
    type ErrableUnaryEntityOperation = MI.ErrableUnaryOperation MathEntity MathEntity
    type BinaryEntityOperation = MI.BinaryOperation MathEntity MathEntity MathEntity
    type ErrableBinaryEntityOperation = MI.ErrableBinaryOperation MathEntity MathEntity MathEntity

    scalarEntity :: BS.BigScalar -> MathEntity
    scalarEntity = ScalarEntity

    vectorEntity :: BV.BigVector -> MathEntity
    vectorEntity = VectorEntity

    listEntity :: BL.BigList -> MathEntity
    listEntity = ListEntity

    matrixEntity :: BM.BigMatrix -> MathEntity
    matrixEntity = MatrixEntity

    boolEntity :: Bool -> MathEntity
    boolEntity = BoolEntity

    _trueEntity :: MathEntity
    _trueEntity = boolEntity True

    _falseEntity :: MathEntity
    _falseEntity = boolEntity False

    _checkScalarType :: MI.UnaryOperation BS.BigScalar Bool -> MathEntity -> MI.Result MathEntity
    _checkScalarType typeCheck (ScalarEntity scalar) = MI.withValue $ boolEntity $ typeCheck scalar
    _checkScalarType _ _ = MI.withValue _falseEntity

    isInteger :: MathEntity -> MI.Result MathEntity
    isInteger = _checkScalarType BS.sIsInteger

    isReal :: MathEntity -> MI.Result MathEntity
    isReal = _checkScalarType BS.sIsReal

    isComplex :: MathEntity -> MI.Result MathEntity
    isComplex = _checkScalarType BS.sIsComplex

    isQuaternion :: MathEntity -> MI.Result MathEntity
    isQuaternion = _checkScalarType BS.sIsQuaternion

    isScalar :: MathEntity -> MI.Result MathEntity
    isScalar ScalarEntity{} = MI.withValue _trueEntity
    isScalar _ = MI.withValue _falseEntity

    isVector :: MathEntity -> MI.Result MathEntity
    isVector VectorEntity{} = MI.withValue _trueEntity
    isVector _ = MI.withValue _falseEntity

    isList :: MathEntity -> MI.Result MathEntity
    isList ListEntity{} = MI.withValue _trueEntity
    isList _ = MI.withValue _falseEntity

    isMatrix :: MathEntity -> MI.Result MathEntity
    isMatrix MatrixEntity{} = MI.withValue _trueEntity
    isMatrix _ = MI.withValue _falseEntity

    isBool :: MathEntity -> MI.Result MathEntity
    isBool BoolEntity{} = MI.withValue _trueEntity
    isBool _ = MI.withValue _falseEntity

    _coef :: BS.UnaryScalarOperation -> MathEntity -> MI.Result MathEntity
    _coef coefGetter (ScalarEntity scalar) = MI.withValue $ scalarEntity $ coefGetter scalar
    _coef _ _ = MI.withError MI.InvalidType

    realCoef :: MathEntity -> MI.Result MathEntity
    realCoef = _coef BS.sRealCoef

    imag0Coef :: MathEntity -> MI.Result MathEntity
    imag0Coef = _coef BS.sImag0Coef

    imag1Coef :: MathEntity -> MI.Result MathEntity
    imag1Coef = _coef BS.sImag1Coef

    imag2Coef :: MathEntity -> MI.Result MathEntity
    imag2Coef = _coef BS.sImag2Coef

    plus :: MathEntity -> MathEntity -> MI.Result MathEntity
    plus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ scalarEntity $ BS.splus leftScalar rightScalar
    plus (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ listEntity $ BL.splusl leftScalar rightList
    plus (VectorEntity leftVec) (VectorEntity rightVec) = MI.unResolve result vectorEntity
        where result = BV.vplus leftVec rightVec
    plus (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ listEntity $ BL.lpluss leftList rightScalar
    plus (ListEntity leftList) (ListEntity rightList) = MI.unResolve result listEntity
        where result = BL.lplus leftList rightList
    plus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result matrixEntity
        where result = BM.mplus leftMatrix rightMatrix
    plus _ _ = MI.withError MI.IncompatibleTypes

    minus :: MathEntity -> MathEntity -> MI.Result MathEntity
    minus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ scalarEntity $ BS.sminus leftScalar rightScalar
    minus (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ listEntity $ BL.sminusl leftScalar rightList
    minus (VectorEntity leftVec) (VectorEntity rightVec) = MI.unResolve result vectorEntity
        where result = BV.vminus leftVec rightVec
    minus (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ listEntity $ BL.lminuss leftList rightScalar
    minus (ListEntity leftList) (ListEntity rightList) = MI.unResolve result listEntity
        where result = BL.lminus leftList rightList
    minus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result matrixEntity
        where result = BM.mminus leftMatrix rightMatrix
    minus _ _ = MI.withError MI.IncompatibleTypes

    mult :: MathEntity -> MathEntity -> MI.Result MathEntity
    mult (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ scalarEntity $ BS.splus leftScalar rightScalar
    mult (ScalarEntity leftScalar) (VectorEntity rightVector) = MI.unResolve result vectorEntity
        where result = BV.smultv leftScalar rightVector
    mult (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ listEntity $ BL.smultl leftScalar rightList
    mult (ScalarEntity leftScalar) (MatrixEntity rightMatrix) = MI.withValue $ matrixEntity $ BM.smultm leftScalar rightMatrix
    mult (VectorEntity leftVector) (ScalarEntity rightScalar) = MI.unResolve result vectorEntity
        where result = BV.vmults leftVector rightScalar
    mult (VectorEntity leftVector) (MatrixEntity rightMatrix) = MI.unResolve result matrixEntity
        where result = BM.vmultm leftVector rightMatrix
    mult (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ listEntity $ BL.lmults leftList rightScalar
    mult (ListEntity leftList) (ListEntity rightList) = MI.unResolve result listEntity
        where result = BL.lmult leftList rightList
    mult (MatrixEntity leftMatrix) (ScalarEntity rightScalar) = MI.withValue $ matrixEntity $ BM.mmults leftMatrix rightScalar
    mult (MatrixEntity leftMatrix) (VectorEntity rightVector) = MI.unResolve result matrixEntity
        where result = BM.mmultv leftMatrix rightVector
    mult (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result matrixEntity
        where result = BM.mmult leftMatrix rightMatrix
    mult _ _ = MI.withError MI.IncompatibleTypes

    scale :: MathEntity -> MathEntity -> MI.Result MathEntity
    scale (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result vectorEntity
        where result = BV.vscale leftVector rightVector
    scale (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result matrixEntity
        where result = BM.mscale leftMatrix rightMatrix
    scale _ _ = MI.withError MI.InvalidType

    dot :: MathEntity -> MathEntity -> MI.Result MathEntity
    dot (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result scalarEntity
        where result = BV.vdot leftVector rightVector
    dot _ _ = MI.withError MI.InvalidType

    cross :: MathEntity -> MathEntity -> MI.Result MathEntity
    cross (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result vectorEntity
        where result = BV.vcross leftVector rightVector
    cross _ _ = MI.withError MI.InvalidType

    div :: MathEntity -> MathEntity -> MI.Result MathEntity
    div (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.sdiv leftScalar rightScalar
    div (ScalarEntity leftScalar) (ListEntity rightList) = MI.unResolve result listEntity
        where result = BL.sdivl leftScalar rightList
    div (VectorEntity leftVector) (ScalarEntity rightScalar) = MI.unResolve result vectorEntity
        where result = BV.vdivs leftVector rightScalar
    div (ListEntity leftList) (ScalarEntity rightScalar) = MI.unResolve result listEntity
        where result = BL.ldivs leftList rightScalar
    div (ListEntity leftList) (ListEntity rightList) = MI.unResolve result listEntity
        where result = BL.ldiv leftList rightList
    div (MatrixEntity leftMatrix) (ScalarEntity rightScalar) = MI.unResolve result matrixEntity
        where result = BM.mdivs leftMatrix rightScalar
    div (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result matrixEntity
        where result = BM.mdiv leftMatrix rightMatrix
    div _ _ = MI.withError MI.IncompatibleTypes

    intDiv :: MathEntity -> MathEntity -> MI.Result MathEntity
    intDiv (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.sIntDiv leftScalar rightScalar
    intDiv _ _ = MI.withError MI.InvalidType

    mod :: MathEntity -> MathEntity -> MI.Result MathEntity
    mod (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.smod leftScalar rightScalar
    mod _ _ = MI.withError MI.InvalidType

    rem :: MathEntity -> MathEntity -> MI.Result MathEntity
    rem (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.srem leftScalar rightScalar
    rem _ _ = MI.withError MI.InvalidType

    lcm :: MathEntity -> MathEntity -> MI.Result MathEntity
    lcm (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.slcm leftScalar rightScalar
    lcm _ _ = MI.withError MI.InvalidType

    gcd :: MathEntity -> MathEntity -> MI.Result MathEntity
    gcd (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.sgcd leftScalar rightScalar
    gcd _ _ = MI.withError MI.InvalidType

    neg :: MathEntity -> MI.Result MathEntity
    neg (ScalarEntity scalar) = MI.withValue $ scalarEntity $ BS.sneg scalar
    neg (VectorEntity vector) = MI.withValue $ vectorEntity $ BV.vneg vector
    neg (ListEntity list) = MI.withValue $ listEntity $ BL.lneg list
    neg (MatrixEntity matrix) = MI.withValue $ matrixEntity $ BM.mneg matrix
    neg _ = MI.withError MI.InvalidType

    pow :: MathEntity -> MathEntity -> MI.Result MathEntity
    pow (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.spow leftScalar rightScalar
    pow (ScalarEntity leftScalar) (ListEntity rightList) = MI.unResolve result listEntity
        where result = BL.spowl leftScalar rightList
    pow (ListEntity leftList) (ScalarEntity rightScalar) = MI.unResolve result listEntity
        where result = BL.lpows leftList rightScalar
    pow (ListEntity leftList) (ListEntity rightList) = MI.unResolve result listEntity
        where result = BL.lpow leftList rightList
    pow _ _ = MI.withError MI.InvalidType

    sqrt :: MathEntity -> MI.Result MathEntity
    sqrt (ScalarEntity scalar) = MI.withValue $ scalarEntity $ BS.ssqrt scalar
    sqrt _ = MI.withError MI.InvalidType

    inv :: MathEntity -> MI.Result MathEntity
    inv (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sinv scalar
    inv (MatrixEntity matrix) = MI.unResolve result matrixEntity
        where result = BM.minv matrix
    inv _ = MI.withError MI.InvalidType

    conj :: MathEntity -> MI.Result MathEntity
    conj (ScalarEntity scalar) = MI.withValue $ scalarEntity $ BS.sconj scalar
    conj _ = MI.withError MI.InvalidType

    norm :: MathEntity -> MI.Result MathEntity
    norm (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.snorm scalar
    norm (VectorEntity vector) = MI.unResolve result vectorEntity
        where result = BV.vnorm vector
    norm _ = MI.withError MI.InvalidType

    arg :: MathEntity -> MI.Result MathEntity
    arg (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sarg scalar
    arg _ = MI.withError MI.InvalidType

    exp :: MathEntity -> MI.Result MathEntity
    exp (ScalarEntity scalar) = MI.withValue $ scalarEntity $ BS.sexp scalar
    exp _ = MI.withError MI.InvalidType

    log :: MathEntity -> MI.Result MathEntity
    log (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.slog scalar
    log _ = MI.withError MI.InvalidType

    log2 :: MathEntity -> MI.Result MathEntity
    log2 (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.slog2 scalar
    log2 _ = MI.withError MI.InvalidType

    log10 :: MathEntity -> MI.Result MathEntity
    log10 (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.slog2 scalar
    log10 _ = MI.withError MI.InvalidType

    logBase :: MathEntity -> MathEntity -> MI.Result MathEntity
    logBase (ScalarEntity baseScalar) (ScalarEntity argScalar) = MI.unResolve result scalarEntity
        where result = BS.slogBase baseScalar argScalar
    logBase _ _ = MI.withError MI.InvalidType

    sin :: MathEntity -> MI.Result MathEntity
    sin (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.ssin scalar
    sin _ = MI.withError MI.InvalidType

    cos :: MathEntity -> MI.Result MathEntity
    cos (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.scos scalar
    cos _ = MI.withError MI.InvalidType

    tan :: MathEntity -> MI.Result MathEntity
    tan (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.stan scalar
    tan _ = MI.withError MI.InvalidType

    sinh :: MathEntity -> MI.Result MathEntity
    sinh (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.ssinh scalar
    sinh _ = MI.withError MI.InvalidType

    cosh :: MathEntity -> MI.Result MathEntity
    cosh (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.scosh scalar
    cosh _ = MI.withError MI.InvalidType

    tanh :: MathEntity -> MI.Result MathEntity
    tanh (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.stanh scalar
    tanh _ = MI.withError MI.InvalidType

    asin :: MathEntity -> MI.Result MathEntity
    asin (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sasin scalar
    asin _ = MI.withError MI.InvalidType

    acos :: MathEntity -> MI.Result MathEntity
    acos (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sacos scalar
    acos _ = MI.withError MI.InvalidType

    atan :: MathEntity -> MI.Result MathEntity
    atan (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.satan scalar
    atan _ = MI.withError MI.InvalidType

    atan2 :: MathEntity -> MathEntity -> MI.Result MathEntity
    atan2 (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.satan2 leftScalar rightScalar
    atan2 _ _ = MI.withError MI.InvalidType

    asinh :: MathEntity -> MI.Result MathEntity
    asinh (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sasinh scalar
    asinh _ = MI.withError MI.InvalidType

    acosh :: MathEntity -> MI.Result MathEntity
    acosh (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sacosh scalar
    acosh _ = MI.withError MI.InvalidType

    atanh :: MathEntity -> MI.Result MathEntity
    atanh (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.satanh scalar
    atanh _ = MI.withError MI.InvalidType

    min :: MathEntity -> MathEntity -> MI.Result MathEntity
    min (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.smin leftScalar rightScalar
    min _ _ = MI.withError MI.InvalidType

    max :: MathEntity -> MathEntity -> MI.Result MathEntity
    max (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result scalarEntity
        where result = BS.smax leftScalar rightScalar
    max _ _ = MI.withError MI.InvalidType

    less :: MathEntity -> MathEntity -> MI.Result MathEntity
    less (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result boolEntity
        where result = BS.sLess leftScalar rightScalar
    less _ _ = MI.withError MI.InvalidType

    lessEqual :: MathEntity -> MathEntity -> MI.Result MathEntity
    lessEqual (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result boolEntity
        where result = BS.sLessEqual leftScalar rightScalar
    lessEqual _ _ = MI.withError MI.InvalidType

    greater :: MathEntity -> MathEntity -> MI.Result MathEntity
    greater (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result boolEntity
        where result = BS.sGreater leftScalar rightScalar
    greater _ _ = MI.withError MI.InvalidType

    greaterEqual :: MathEntity -> MathEntity -> MI.Result MathEntity
    greaterEqual (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result boolEntity
        where result = BS.sGreaterEqual leftScalar rightScalar
    greaterEqual _ _ = MI.withError MI.InvalidType

    ceiling :: MathEntity -> MI.Result MathEntity
    ceiling (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sceil scalar
    ceiling _ = MI.withError MI.InvalidType

    floor :: MathEntity -> MI.Result MathEntity
    floor (ScalarEntity scalar) = MI.unResolve result scalarEntity
        where result = BS.sfloor scalar
    floor _ = MI.withError MI.InvalidType

    even :: MathEntity -> MI.Result MathEntity
    even (ScalarEntity scalar) = MI.withValue $ boolEntity $ BS.sEven scalar
    even _ = MI.withError MI.InvalidType

    odd :: MathEntity -> MI.Result MathEntity
    odd (ScalarEntity scalar) = MI.withValue $ boolEntity $ BS.sOdd scalar
    odd _ = MI.withError MI.InvalidType

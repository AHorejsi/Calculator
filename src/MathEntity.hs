{-# LANGUAGE DeriveGeneric #-}

module MathEntity (
    MathEntity,
    UnaryEntityAction,
    ErrableUnaryEntityAction,
    BinaryEntityAction,
    ErrableBinaryEntityAction,
    TernaryEntityAction,
    ErrableTernaryEntityAction,
    makeScalar,
    makeVector,
    makeList,
    makeMatrix,
    makeBool,
    scalarResult,
    vectorResult,
    listResult,
    matrixResult,
    boolResult,
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
    vectorPart,
    plus,
    plusPad,
    minus,
    minusPad,
    mult,
    multPad,
    scale,
    dot,
    cross,
    div,
    divPad,
    intDiv,
    mod,
    rem,
    abs,
    lcm,
    gcd,
    neg,
    pow,
    powPad,
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
    size,
    equalSize,
    rows,
    cols,
    isSquare,
    isNull,
    dist,
    angle,
    sum,
    cumsum,
    prod,
    cumprod,
    mean,
    gmean,
    hmean,
    range,
    midrange,
    mode,
    sortAsc,
    sortDesc,
    isSortedAsc,
    isSortedDesc,
    isSorted,
    sublist,
    concat,
    merge,
    rowVector,
    colVector,
    det,
    transpose,
    submatrix,
    addRow,
    multRow,
    swapRows,
    addCol,
    multCol,
    swapCols,
    not,
    and,
    or,
    xor,
    get1,
    get2,
    listRepeat,
    listIncrement,
    matrixRepeat,
    identity
) where
    import Prelude hiding (abs, div, mod, rem, lcm, gcd, sqrt, exp, log, logBase, sin, cos, tan, sinh, cosh, tanh, asin, acos, atan, atan2, asinh, acosh, atanh, min, max, ceiling, floor, even, odd, sum, concat, not, and, or)
    import qualified GHC.Generics as G
    import qualified Data.Char as C
    import qualified Data.Hashable as H
    import qualified Text.Printf as TP
    import qualified Stringify as Str
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
        show scalar@ScalarEntity{} = TP.printf "ScalarEntity: %s" (show scalar)
        show vector@VectorEntity{} = TP.printf "VectorEntity: %s" (show vector)
        show list@ListEntity{} = TP.printf "ListEntity: %s" (show list)
        show matrix@MatrixEntity{} = TP.printf "MatrixEntity: %s" (show matrix)
        show bool@BoolEntity{} = TP.printf "BoolEntity: %s" (show bool)

    instance Str.Stringifier MathEntity where
        stringify (ScalarEntity scalar) = Str.stringify scalar
        stringify (VectorEntity vector) = Str.stringify vector
        stringify (ListEntity list) = Str.stringify list
        stringify (MatrixEntity matrix) = Str.stringify matrix
        stringify (BoolEntity bool) = (C.toLower first) : rest
            where str = show bool
                  first = head str
                  rest = tail str

    type UnaryEntityAction = MI.UnaryAction MathEntity MathEntity
    type ErrableUnaryEntityAction = MI.ErrableUnaryAction MathEntity MathEntity
    type BinaryEntityAction = MI.BinaryAction MathEntity MathEntity MathEntity
    type ErrableBinaryEntityAction = MI.ErrableBinaryAction MathEntity MathEntity MathEntity
    type TernaryEntityAction = MI.TernaryAction MathEntity MathEntity MathEntity MathEntity
    type ErrableTernaryEntityAction = MI.ErrableTernaryAction MathEntity MathEntity MathEntity MathEntity

    makeScalar :: BS.BigScalar -> MathEntity
    makeScalar = ScalarEntity

    makeVector :: BV.BigVector -> MathEntity
    makeVector = VectorEntity

    makeList :: BL.BigList -> MathEntity
    makeList = ListEntity

    makeMatrix :: BM.BigMatrix -> MathEntity
    makeMatrix = MatrixEntity

    makeBool :: Bool -> MathEntity
    makeBool = BoolEntity

    scalarResult :: BS.BigScalar -> MI.ComputationResult MathEntity
    scalarResult = MI.withValue . makeScalar

    vectorResult :: BV.BigVector -> MI.ComputationResult MathEntity
    vectorResult = MI.withValue . makeVector

    listResult :: BL.BigList -> MI.ComputationResult MathEntity
    listResult = MI.withValue . makeList

    matrixResult :: BM.BigMatrix -> MI.ComputationResult MathEntity
    matrixResult = MI.withValue . makeMatrix

    boolResult :: Bool -> MI.ComputationResult MathEntity
    boolResult = MI.withValue . makeBool

    _trueEntity :: MathEntity
    _trueEntity = makeBool True

    _falseEntity :: MathEntity
    _falseEntity = makeBool False

    _checkScalarType :: MI.UnaryAction BS.BigScalar Bool -> MathEntity -> MI.ComputationResult MathEntity
    _checkScalarType typeCheck (ScalarEntity scalar) = boolResult $ typeCheck scalar
    _checkScalarType _ _ = MI.withValue _falseEntity

    isInteger :: MathEntity -> MI.ComputationResult MathEntity
    isInteger = _checkScalarType BS.sIsInteger

    isReal :: MathEntity -> MI.ComputationResult MathEntity
    isReal = _checkScalarType BS.sIsReal

    isComplex :: MathEntity -> MI.ComputationResult MathEntity
    isComplex = _checkScalarType BS.sIsComplex

    isQuaternion :: MathEntity -> MI.ComputationResult MathEntity
    isQuaternion = _checkScalarType BS.sIsQuaternion

    isScalar :: MathEntity -> MI.ComputationResult MathEntity
    isScalar ScalarEntity{} = MI.withValue _trueEntity
    isScalar _ = MI.withValue _falseEntity

    isVector :: MathEntity -> MI.ComputationResult MathEntity
    isVector VectorEntity{} = MI.withValue _trueEntity
    isVector _ = MI.withValue _falseEntity

    isList :: MathEntity -> MI.ComputationResult MathEntity
    isList ListEntity{} = MI.withValue _trueEntity
    isList _ = MI.withValue _falseEntity

    isMatrix :: MathEntity -> MI.ComputationResult MathEntity
    isMatrix MatrixEntity{} = MI.withValue _trueEntity
    isMatrix _ = MI.withValue _falseEntity

    isBool :: MathEntity -> MI.ComputationResult MathEntity
    isBool BoolEntity{} = MI.withValue _trueEntity
    isBool _ = MI.withValue _falseEntity

    _coef :: BS.UnaryScalarAction -> MathEntity -> MI.ComputationResult MathEntity
    _coef coefGetter (ScalarEntity scalar) = scalarResult $ coefGetter scalar
    _coef _ _ = MI.withError MI.InvalidType

    realCoef :: MathEntity -> MI.ComputationResult MathEntity
    realCoef = _coef BS.sRealCoef

    imag0Coef :: MathEntity -> MI.ComputationResult MathEntity
    imag0Coef = _coef BS.sImag0Coef

    imag1Coef :: MathEntity -> MI.ComputationResult MathEntity
    imag1Coef = _coef BS.sImag1Coef

    imag2Coef :: MathEntity -> MI.ComputationResult MathEntity
    imag2Coef = _coef BS.sImag2Coef

    vectorPart :: MathEntity -> MI.ComputationResult MathEntity
    vectorPart (ScalarEntity scalar) = scalarResult $ BS.sVectorPart scalar
    vectorPart _ = MI.withError MI.InvalidType

    plus :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    plus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = scalarResult $ BS.splus leftScalar rightScalar
    plus (ScalarEntity leftScalar) (ListEntity rightList) = listResult $ BL.splusl leftScalar rightList
    plus (VectorEntity leftVec) (VectorEntity rightVec) = MI.unResolve result makeVector
        where result = BV.vplus leftVec rightVec
    plus (ListEntity leftList) (ScalarEntity rightScalar) = listResult $ BL.lpluss leftList rightScalar
    plus (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.lplus leftList rightList
    plus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
        where result = BM.mplus leftMatrix rightMatrix
    plus _ _ = MI.withError MI.InvalidType

    plusPad :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    plusPad (ScalarEntity padVal) (ListEntity leftList) (ListEntity rightList) = listResult $ BL.lplusPad padVal leftList rightList
    plusPad _ _ _ = MI.withError MI.InvalidType

    minus :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    minus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = scalarResult $ BS.sminus leftScalar rightScalar
    minus (ScalarEntity leftScalar) (ListEntity rightList) = listResult $ BL.sminusl leftScalar rightList
    minus (VectorEntity leftVec) (VectorEntity rightVec) = MI.unResolve result makeVector
        where result = BV.vminus leftVec rightVec
    minus (ListEntity leftList) (ScalarEntity rightScalar) = listResult $ BL.lminuss leftList rightScalar
    minus (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.lminus leftList rightList
    minus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
        where result = BM.mminus leftMatrix rightMatrix
    minus _ _ = MI.withError MI.InvalidType

    minusPad :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    minusPad (ScalarEntity padVal) (ListEntity leftList) (ListEntity rightList) = listResult $ BL.lminusPad padVal leftList rightList
    minusPad _ _ _ = MI.withError MI.InvalidType

    mult :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    mult (ScalarEntity leftScalar) (ScalarEntity rightScalar) = scalarResult $ BS.splus leftScalar rightScalar
    mult (ScalarEntity leftScalar) (VectorEntity rightVector) = MI.unResolve result makeVector
        where result = BV.smultv leftScalar rightVector
    mult (ScalarEntity leftScalar) (ListEntity rightList) = listResult $ BL.smultl leftScalar rightList
    mult (ScalarEntity leftScalar) (MatrixEntity rightMatrix) = matrixResult $ BM.smultm leftScalar rightMatrix
    mult (VectorEntity leftVector) (ScalarEntity rightScalar) = MI.unResolve result makeVector
        where result = BV.vmults leftVector rightScalar
    mult (VectorEntity leftVector) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
        where result = BM.vmultm leftVector rightMatrix
    mult (ListEntity leftList) (ScalarEntity rightScalar) = listResult $ BL.lmults leftList rightScalar
    mult (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.lmult leftList rightList
    mult (MatrixEntity leftMatrix) (ScalarEntity rightScalar) = matrixResult $ BM.mmults leftMatrix rightScalar
    mult (MatrixEntity leftMatrix) (VectorEntity rightVector) = MI.unResolve result makeMatrix
        where result = BM.mmultv leftMatrix rightVector
    mult (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
        where result = BM.mmult leftMatrix rightMatrix
    mult _ _ = MI.withError MI.InvalidType

    multPad :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    multPad (ScalarEntity padVal) (ListEntity leftList) (ListEntity rightList) = listResult $ BL.lmultPad padVal leftList rightList
    multPad _ _ _ = MI.withError MI.InvalidType

    scale :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    scale (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeVector
        where result = BV.vscale leftVector rightVector
    scale (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
        where result = BM.mscale leftMatrix rightMatrix
    scale _ _ = MI.withError MI.InvalidType

    dot :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    dot (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeScalar
        where result = BV.vdot leftVector rightVector
    dot _ _ = MI.withError MI.InvalidType

    cross :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    cross (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeVector
        where result = BV.vcross leftVector rightVector
    cross _ _ = MI.withError MI.InvalidType

    div :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    div (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.sdiv leftScalar rightScalar
    div (ScalarEntity leftScalar) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.sdivl leftScalar rightList
    div (VectorEntity leftVector) (ScalarEntity rightScalar) = MI.unResolve result makeVector
        where result = BV.vdivs leftVector rightScalar
    div (ListEntity leftList) (ScalarEntity rightScalar) = MI.unResolve result makeList
        where result = BL.ldivs leftList rightScalar
    div (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.ldiv leftList rightList
    div (MatrixEntity leftMatrix) (ScalarEntity rightScalar) = MI.unResolve result makeMatrix
        where result = BM.mdivs leftMatrix rightScalar
    div (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
        where result = BM.mdiv leftMatrix rightMatrix
    div _ _ = MI.withError MI.InvalidType

    divPad :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    divPad (ScalarEntity padVal) (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.ldivPad padVal leftList rightList
    divPad _ _ _ = MI.withError MI.InvalidType

    intDiv :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    intDiv (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.sIntDiv leftScalar rightScalar
    intDiv _ _ = MI.withError MI.InvalidType

    mod :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    mod (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.smod leftScalar rightScalar
    mod _ _ = MI.withError MI.InvalidType

    rem :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    rem (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.srem leftScalar rightScalar
    rem _ _ = MI.withError MI.InvalidType

    abs :: MathEntity -> MI.ComputationResult MathEntity
    abs (ScalarEntity scalar) = scalarResult $ BS.sabs scalar
    abs (VectorEntity vector) = scalarResult $ BV.vabs vector
    abs _ = MI.withError MI.InvalidType

    lcm :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    lcm (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.slcm leftScalar rightScalar
    lcm _ _ = MI.withError MI.InvalidType

    gcd :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    gcd (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.sgcd leftScalar rightScalar
    gcd _ _ = MI.withError MI.InvalidType

    neg :: MathEntity -> MI.ComputationResult MathEntity
    neg (ScalarEntity scalar) = scalarResult $ BS.sneg scalar
    neg (VectorEntity vector) = vectorResult $ BV.vneg vector
    neg (ListEntity list) = listResult $ BL.lneg list
    neg (MatrixEntity matrix) = matrixResult $ BM.mneg matrix
    neg _ = MI.withError MI.InvalidType

    pow :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    pow (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.spow leftScalar rightScalar
    pow (ScalarEntity leftScalar) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.spowl leftScalar rightList
    pow (ListEntity leftList) (ScalarEntity rightScalar) = MI.unResolve result makeList
        where result = BL.lpows leftList rightScalar
    pow (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.lpow leftList rightList
    pow _ _ = MI.withError MI.InvalidType

    powPad :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    powPad (ScalarEntity padVal) (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.lpowPad padVal leftList rightList
    powPad _ _ _ = MI.withError MI.InvalidType

    sqrt :: MathEntity -> MI.ComputationResult MathEntity
    sqrt (ScalarEntity scalar) = scalarResult $ BS.ssqrt scalar
    sqrt _ = MI.withError MI.InvalidType

    inv :: MathEntity -> MI.ComputationResult MathEntity
    inv (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sinv scalar
    inv (MatrixEntity matrix) = MI.unResolve result makeMatrix
        where result = BM.minv matrix
    inv _ = MI.withError MI.InvalidType

    conj :: MathEntity -> MI.ComputationResult MathEntity
    conj (ScalarEntity scalar) = scalarResult $ BS.sconj scalar
    conj _ = MI.withError MI.InvalidType

    norm :: MathEntity -> MI.ComputationResult MathEntity
    norm (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.snorm scalar
    norm (VectorEntity vector) = MI.unResolve result makeVector
        where result = BV.vnorm vector
    norm _ = MI.withError MI.InvalidType

    arg :: MathEntity -> MI.ComputationResult MathEntity
    arg (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sarg scalar
    arg _ = MI.withError MI.InvalidType

    exp :: MathEntity -> MI.ComputationResult MathEntity
    exp (ScalarEntity scalar) = scalarResult $ BS.sexp scalar
    exp _ = MI.withError MI.InvalidType

    log :: MathEntity -> MI.ComputationResult MathEntity
    log (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.slog scalar
    log _ = MI.withError MI.InvalidType

    log2 :: MathEntity -> MI.ComputationResult MathEntity
    log2 (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.slog2 scalar
    log2 _ = MI.withError MI.InvalidType

    log10 :: MathEntity -> MI.ComputationResult MathEntity
    log10 (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.slog10 scalar
    log10 _ = MI.withError MI.InvalidType

    logBase :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    logBase (ScalarEntity baseScalar) (ScalarEntity argScalar) = MI.unResolve result makeScalar
        where result = BS.slogBase baseScalar argScalar
    logBase _ _ = MI.withError MI.InvalidType

    sin :: MathEntity -> MI.ComputationResult MathEntity
    sin (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.ssin scalar
    sin _ = MI.withError MI.InvalidType

    cos :: MathEntity -> MI.ComputationResult MathEntity
    cos (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.scos scalar
    cos _ = MI.withError MI.InvalidType

    tan :: MathEntity -> MI.ComputationResult MathEntity
    tan (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.stan scalar
    tan _ = MI.withError MI.InvalidType

    sinh :: MathEntity -> MI.ComputationResult MathEntity
    sinh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.ssinh scalar
    sinh _ = MI.withError MI.InvalidType

    cosh :: MathEntity -> MI.ComputationResult MathEntity
    cosh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.scosh scalar
    cosh _ = MI.withError MI.InvalidType

    tanh :: MathEntity -> MI.ComputationResult MathEntity
    tanh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.stanh scalar
    tanh _ = MI.withError MI.InvalidType

    asin :: MathEntity -> MI.ComputationResult MathEntity
    asin (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sasin scalar
    asin _ = MI.withError MI.InvalidType

    acos :: MathEntity -> MI.ComputationResult MathEntity
    acos (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sacos scalar
    acos _ = MI.withError MI.InvalidType

    atan :: MathEntity -> MI.ComputationResult MathEntity
    atan (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.satan scalar
    atan _ = MI.withError MI.InvalidType

    atan2 :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    atan2 (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.satan2 leftScalar rightScalar
    atan2 _ _ = MI.withError MI.InvalidType

    asinh :: MathEntity -> MI.ComputationResult MathEntity
    asinh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sasinh scalar
    asinh _ = MI.withError MI.InvalidType

    acosh :: MathEntity -> MI.ComputationResult MathEntity
    acosh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sacosh scalar
    acosh _ = MI.withError MI.InvalidType

    atanh :: MathEntity -> MI.ComputationResult MathEntity
    atanh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.satanh scalar
    atanh _ = MI.withError MI.InvalidType

    min :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    min (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.smin leftScalar rightScalar
    min _ _ = MI.withError MI.InvalidType

    listMin :: MathEntity -> MI.ComputationResult MathEntity
    listMin (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lminimum list

    max :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    max (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.smax leftScalar rightScalar
    max _ _ = MI.withError MI.InvalidType

    listMax :: MathEntity -> MI.ComputationResult MathEntity
    listMax (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lmaximum list

    less :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    less (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = BS.sLess leftScalar rightScalar
    less _ _ = MI.withError MI.InvalidType

    lessEqual :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    lessEqual (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = BS.sLessEqual leftScalar rightScalar
    lessEqual _ _ = MI.withError MI.InvalidType

    greater :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    greater (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = BS.sGreater leftScalar rightScalar
    greater _ _ = MI.withError MI.InvalidType

    greaterEqual :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    greaterEqual (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = BS.sGreaterEqual leftScalar rightScalar
    greaterEqual _ _ = MI.withError MI.InvalidType

    ceiling :: MathEntity -> MI.ComputationResult MathEntity
    ceiling (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sceil scalar
    ceiling _ = MI.withError MI.InvalidType

    floor :: MathEntity -> MI.ComputationResult MathEntity
    floor (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sfloor scalar
    floor _ = MI.withError MI.InvalidType

    round :: MathEntity -> MI.ComputationResult MathEntity
    round (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sround scalar
    round _ = MI.withError MI.InvalidType

    even :: MathEntity -> MI.ComputationResult MathEntity
    even (ScalarEntity scalar) = boolResult $ BS.sEven scalar
    even _ = MI.withError MI.InvalidType

    odd :: MathEntity -> MI.ComputationResult MathEntity
    odd (ScalarEntity scalar) = boolResult $ BS.sOdd scalar
    odd _ = MI.withError MI.InvalidType

    size :: MathEntity -> MI.ComputationResult MathEntity
    size (VectorEntity vector) = scalarResult $ BV.vsize vector
    size (ListEntity list) = scalarResult $ BL.lsize list
    size (MatrixEntity matrix) = scalarResult $ BM.msize matrix
    size _ = MI.withError MI.InvalidType

    equalSize :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    equalSize (VectorEntity leftVector) (VectorEntity rightVector) = boolResult $ BV.vEqualSize leftVector rightVector
    equalSize (ListEntity leftList) (ListEntity rightList) = boolResult $ BL.lEqualSize leftList rightList
    equalSize (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = boolResult $ BM.mEqualSize leftMatrix rightMatrix
    equalSize _ _ = MI.withError MI.InvalidType

    rows :: MathEntity -> MI.ComputationResult MathEntity
    rows (MatrixEntity matrix) = scalarResult $ BM.mrows matrix
    rows _ = MI.withError MI.InvalidType

    cols :: MathEntity -> MI.ComputationResult MathEntity
    cols (MatrixEntity matrix) = scalarResult $ BM.mcols matrix
    cols _ = MI.withError MI.InvalidType

    isSquare :: MathEntity -> MI.ComputationResult MathEntity
    isSquare (MatrixEntity matrix) = boolResult $ BM.mIsSquare matrix
    isSquare _ = MI.withError MI.InvalidType

    isNull :: MathEntity -> MI.ComputationResult MathEntity
    isNull (VectorEntity vector) = boolResult $ BV.vIsNull vector
    isNull _ = MI.withError MI.InvalidType

    dist :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    dist (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeScalar
        where result = BV.vdist leftVector rightVector
    dist _ _ = MI.withError MI.InvalidType

    angle :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    angle (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeScalar
        where result = BV.vangle leftVector rightVector

    sum :: MathEntity -> MI.ComputationResult MathEntity
    sum (ListEntity list) = scalarResult $ BL.lsum list
    sum _ = MI.withError MI.InvalidType

    cumsum :: MathEntity -> MI.ComputationResult MathEntity
    cumsum (ListEntity list) = listResult $ BL.lcumsum list
    cumsum _ = MI.withError MI.InvalidType

    prod :: MathEntity -> MI.ComputationResult MathEntity
    prod (ListEntity list) = scalarResult $ BL.lprod list
    prod _ = MI.withError MI.InvalidType

    cumprod :: MathEntity -> MI.ComputationResult MathEntity
    cumprod (ListEntity list) = listResult $ BL.lcumprod list
    cumprod _ = MI.withError MI.InvalidType

    mean :: MathEntity -> MI.ComputationResult MathEntity
    mean (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lmean list
    mean _ = MI.withError MI.InvalidType

    gmean :: MathEntity -> MI.ComputationResult MathEntity
    gmean (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lgmean list
    gmean _ = MI.withError MI.InvalidType

    hmean :: MathEntity -> MI.ComputationResult MathEntity
    hmean (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lhmean list
    hmean _ = MI.withError MI.InvalidType

    median :: MathEntity -> MI.ComputationResult MathEntity
    median (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lmedian list
    median _ = MI.withError MI.InvalidType

    range :: MathEntity -> MI.ComputationResult MathEntity
    range (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lrange list
    range _ = MI.withError MI.InvalidType

    midrange :: MathEntity -> MI.ComputationResult MathEntity
    midrange (ListEntity list) = MI.unResolve result makeScalar
        where result = BL.lmidrange list
    midrange _ = MI.withError MI.InvalidType

    mode :: MathEntity -> MI.ComputationResult MathEntity
    mode (ListEntity list) = listResult $ BL.lmode list
    mode _ = MI.withError MI.InvalidType

    sortAsc :: MathEntity -> MI.ComputationResult MathEntity
    sortAsc (ListEntity list) = MI.unResolve result makeList
        where result = BL.lsortAsc list
    sortAsc _ = MI.withError MI.InvalidType

    sortDesc :: MathEntity -> MI.ComputationResult MathEntity
    sortDesc (ListEntity list) = MI.unResolve result makeList
        where result = BL.lsortDesc list
    sortDesc _ = MI.withError MI.InvalidType

    isSortedAsc :: MathEntity -> MI.ComputationResult MathEntity
    isSortedAsc (ListEntity list) = MI.unResolve result makeBool
        where result = BL.lIsSortedAsc list
    isSortedAsc _ = MI.withError MI.InvalidType

    isSortedDesc :: MathEntity -> MI.ComputationResult MathEntity
    isSortedDesc (ListEntity list) = MI.unResolve result makeBool
        where result = BL.lIsSortedDesc list
    isSortedDesc _ = MI.withError MI.InvalidType
    
    isSorted :: MathEntity -> MI.ComputationResult MathEntity
    isSorted (ListEntity list) = MI.unResolve result makeBool
        where result = BL.lIsSorted list
    isSorted _ = MI.withError MI.InvalidType

    sublist :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    sublist (ListEntity list) (ScalarEntity lowIndex) (ScalarEntity highIndex) = MI.unResolve result makeList
        where result = BL.lsub list lowIndex highIndex

    concat :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    concat (ListEntity leftList) (ListEntity rightList) = listResult $ BL.lconcat leftList rightList
    concat _ _ = MI.withError MI.InvalidType

    merge :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    merge (ListEntity leftList) (ListEntity rightList) = MI.unResolve result makeList
        where result = BL.lmerge leftList rightList
    merge _ _ = MI.withError MI.InvalidType

    rowVector :: MathEntity -> MI.ComputationResult MathEntity
    rowVector (VectorEntity vector) = matrixResult $ BM.mRowVector vector
    rowVector _ = MI.withError MI.InvalidType

    colVector :: MathEntity -> MI.ComputationResult MathEntity
    colVector (VectorEntity vector) = matrixResult $ BM.mColVector vector
    colVector _ = MI.withError MI.InvalidType

    det :: MathEntity -> MI.ComputationResult MathEntity
    det (MatrixEntity matrix) = MI.unResolve result makeScalar
        where result = BM.mdet matrix
    det _ = MI.withError MI.InvalidType

    transpose :: MathEntity -> MI.ComputationResult MathEntity
    transpose (MatrixEntity matrix) = matrixResult $ BM.mtranspose matrix
    transpose _ = MI.withError MI.InvalidType

    submatrix :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    submatrix (MatrixEntity matrix) (ScalarEntity lowIndex) (ScalarEntity highIndex) = MI.unResolve result makeMatrix
        where result = BM.msub matrix lowIndex highIndex
    submatrix _ _ _ = MI.withError MI.InvalidType

    addRow :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    addRow (MatrixEntity matrix) (ScalarEntity fromRowIndex) (ScalarEntity toRowIndex) = MI.unResolve result makeMatrix
        where result = BM.mAddRow matrix fromRowIndex toRowIndex
    addRow _ _ _ = MI.withError MI.InvalidType

    multRow :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    multRow (MatrixEntity matrix) (ScalarEntity multValue) (ScalarEntity rowIndex) = MI.unResolve result makeMatrix
        where result = BM.mMultRow matrix multValue rowIndex
    multRow _ _ _ = MI.withError MI.InvalidType

    swapRows :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    swapRows (MatrixEntity matrix) (ScalarEntity rowIndex1) (ScalarEntity rowIndex2) = MI.unResolve result makeMatrix
        where result = BM.mSwapRows matrix rowIndex1 rowIndex2
    swapRows _ _ _ = MI.withError MI.InvalidType

    addCol :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    addCol (MatrixEntity matrix) (ScalarEntity fromColIndex) (ScalarEntity toColIndex) = MI.unResolve result makeMatrix
        where result = BM.mAddCol matrix fromColIndex toColIndex
    addCol _ _ _ = MI.withError MI.InvalidType

    multCol :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    multCol (MatrixEntity matrix) (ScalarEntity multValue) (ScalarEntity colIndex) = MI.unResolve result makeMatrix
        where result = BM.mMultCol matrix multValue colIndex
    multCol _ _ _ = MI.withError MI.InvalidType

    swapCols :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    swapCols (MatrixEntity matrix) (ScalarEntity colIndex1) (ScalarEntity colIndex2) = MI.unResolve result makeMatrix
        where result = BM.mSwapCols matrix colIndex1 colIndex2
    swapCols _ _ _ = MI.withError MI.InvalidType

    not :: MathEntity -> MI.ComputationResult MathEntity
    not entity@BoolEntity{}
        | entity == _trueEntity = MI.withValue _falseEntity
        | otherwise = MI.withValue _trueEntity
    not _ = MI.withError MI.InvalidType

    and :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    and (BoolEntity leftBool) (BoolEntity rightBool) = boolResult $ leftBool && rightBool
    and _ _ = MI.withError MI.InvalidType

    or :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    or (BoolEntity leftBool) (BoolEntity rightBool) = boolResult $ leftBool || rightBool
    or _ _ = MI.withError MI.InvalidType

    xor :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    xor (BoolEntity leftBool) (BoolEntity rightBool) = boolResult $ leftBool /= rightBool
    xor _ _ = MI.withError MI.InvalidType

    get1 :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    get1 (VectorEntity vec) (ScalarEntity index) = MI.unResolve result makeScalar
        where result = BV.vget vec index
    get1 (ListEntity list) (ScalarEntity index) = MI.unResolve result makeScalar
        where result = BL.lget list index
    get1 _ _ = MI.withError MI.InvalidType

    get2 :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    get2 (MatrixEntity matrix) (ScalarEntity rowIndex) (ScalarEntity colIndex) = MI.unResolve result makeScalar
        where result = BM.mget matrix rowIndex colIndex
    get2 _ _ _ = MI.withError MI.InvalidType

    listRepeat :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    listRepeat (ScalarEntity scalar) (ScalarEntity count) = MI.unResolve result makeList
        where result = BL.lrepeat scalar count

    listIncrement :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    listIncrement (ScalarEntity initial) (ScalarEntity incremental) (ScalarEntity count) = MI.unResolve result makeList
        where result = BL.lincrement initial incremental count

    matrixRepeat :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    matrixRepeat (ScalarEntity value) (ScalarEntity rows) (ScalarEntity cols) = MI.unResolve result makeMatrix
        where result = BM.mrepeat value rows cols
    matrixRepeat _ _ _ = MI.withError MI.InvalidType

    identity :: MathEntity -> MI.ComputationResult MathEntity
    identity (ScalarEntity dimensions) = MI.unResolve result makeMatrix
        where result = BM.midentity dimensions

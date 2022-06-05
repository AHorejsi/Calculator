{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

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
    makeMatrix,
    makeBool,
    scalarResult,
    vectorResult,
    matrixResult,
    boolResult,
    isInteger,
    isReal,
    isComplex,
    isQuaternion,
    isScalar,
    isVector,
    isMatrix,
    isBool,
    realCoef,
    imag0Coef,
    imag1Coef,
    imag2Coef,
    vectorPart,
    dot,
    cross,
    intDiv,
    mod,
    rem,
    abs,
    lcm,
    gcd,
    pow,
    sqrt,
    conj,
    norm,
    arg,
    exp,
    log,
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
    identity,
    toBinary,
    toHexadecimal,
    toOctal,
    factorial,
    choose,
    perm
) where
    import Prelude hiding (abs, div, lcm, gcd, sqrt, exp, log, logBase, sin, cos, tan, sinh, cosh, tanh, asin, acos, atan, atan2, asinh, acosh, atanh, min, max, ceiling, floor, even, odd, sum, concat, not, and, or, mod, rem)
    import qualified GHC.Generics as G
    import qualified Data.Hashable as H
    import qualified Actions as A
    import qualified Stringify as Str
    import qualified MathInfo as MI
    import qualified BigScalar as BS
    import qualified BigVector as BV
    import qualified BigMatrix as BM

    data MathEntity = ScalarEntity {
        _scalar :: BS.BigScalar
    } | VectorEntity {
        _vector :: BV.BigVector
    } | MatrixEntity {
        _matrix :: BM.BigMatrix
    } | BoolEntity {
        _bool :: Bool
    } deriving (Eq, G.Generic)

    instance H.Hashable MathEntity where
        hashWithSalt salt (ScalarEntity scalar) = H.hashWithSalt salt scalar
        hashWithSalt salt (VectorEntity vector) = H.hashWithSalt salt vector
        hashWithSalt salt (MatrixEntity matrix) = H.hashWithSalt salt matrix
        hashWithSalt salt (BoolEntity bool) = H.hashWithSalt salt bool

    instance Show MathEntity where
        show (ScalarEntity scalar) = "ScalarEntity: " ++ (show scalar)
        show (VectorEntity vector) = "VectorEntity: " ++ (show vector)
        show (MatrixEntity matrix) = "MatrixEntity: " ++ (show matrix)
        show (BoolEntity bool) = "BoolEntity: " ++ (show bool)

    instance Str.Stringifier MathEntity where
        stringify sets (ScalarEntity scalar) = Str.stringify sets scalar
        stringify sets (VectorEntity vector) = Str.stringify sets vector
        stringify sets (MatrixEntity matrix) = Str.stringify sets matrix
        stringify sets (BoolEntity bool) = _boolStr bool

    type UnaryEntityAction = MI.UnaryAction MathEntity MathEntity
    type ErrableUnaryEntityAction = MI.ErrableUnaryAction MathEntity MathEntity
    type BinaryEntityAction = MI.BinaryAction MathEntity MathEntity MathEntity
    type ErrableBinaryEntityAction = MI.ErrableBinaryAction MathEntity MathEntity MathEntity
    type TernaryEntityAction = MI.TernaryAction MathEntity MathEntity MathEntity MathEntity
    type ErrableTernaryEntityAction = MI.ErrableTernaryAction MathEntity MathEntity MathEntity MathEntity

    _boolStr :: Bool -> String
    _boolStr True = "true"
    _boolStr False = "false"

    makeScalar :: BS.BigScalar -> MathEntity
    makeScalar = ScalarEntity

    makeVector :: BV.BigVector -> MathEntity
    makeVector = VectorEntity

    makeMatrix :: BM.BigMatrix -> MathEntity
    makeMatrix = MatrixEntity

    makeBool :: Bool -> MathEntity
    makeBool = BoolEntity

    scalarResult :: BS.BigScalar -> MI.ComputationResult MathEntity
    scalarResult = MI.withValue . makeScalar

    vectorResult :: BV.BigVector -> MI.ComputationResult MathEntity
    vectorResult = MI.withValue . makeVector

    matrixResult :: BM.BigMatrix -> MI.ComputationResult MathEntity
    matrixResult = MI.withValue . makeMatrix

    boolResult :: Bool -> MI.ComputationResult MathEntity
    boolResult = MI.withValue . makeBool

    _trueEntity :: MathEntity
    _trueEntity = makeBool True

    _falseEntity :: MathEntity
    _falseEntity = makeBool False

    _checkScalarType :: MI.UnaryPredicate BS.BigScalar -> MathEntity -> MI.ComputationResult MathEntity
    _checkScalarType typeCheck (ScalarEntity scalar) = boolResult $ typeCheck scalar
    _checkScalarType _ _ = MI.withValue _falseEntity

    isInteger :: MathEntity -> MI.ComputationResult MathEntity
    isInteger = _checkScalarType BS.isInteger

    isReal :: MathEntity -> MI.ComputationResult MathEntity
    isReal = _checkScalarType BS.isReal

    isComplex :: MathEntity -> MI.ComputationResult MathEntity
    isComplex = _checkScalarType BS.isComplex

    isQuaternion :: MathEntity -> MI.ComputationResult MathEntity
    isQuaternion = _checkScalarType BS.isQuaternion

    isScalar :: MathEntity -> MI.ComputationResult MathEntity
    isScalar ScalarEntity{} = MI.withValue _trueEntity
    isScalar _ = MI.withValue _falseEntity

    isVector :: MathEntity -> MI.ComputationResult MathEntity
    isVector VectorEntity{} = MI.withValue _trueEntity
    isVector _ = MI.withValue _falseEntity

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
    realCoef = _coef BS.realCoef

    imag0Coef :: MathEntity -> MI.ComputationResult MathEntity
    imag0Coef = _coef BS.imag0Coef

    imag1Coef :: MathEntity -> MI.ComputationResult MathEntity
    imag1Coef = _coef BS.imag1Coef

    imag2Coef :: MathEntity -> MI.ComputationResult MathEntity
    imag2Coef = _coef BS.imag2Coef

    vectorPart :: MathEntity -> MI.ComputationResult MathEntity
    vectorPart (ScalarEntity scalar) = scalarResult $ BS.vectorPart scalar
    vectorPart _ = MI.withError MI.InvalidType

    instance A.Addable MathEntity where
        plus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = scalarResult $ A.unsafePlus leftScalar rightScalar
        plus (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeVector
            where result = A.plus leftVector rightVector
        plus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
            where result = A.plus leftMatrix rightMatrix
        plus _ _ = MI.withError MI.InvalidType

    instance A.Subtractable MathEntity where
        minus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = scalarResult $ A.unsafeMinus leftScalar rightScalar
        minus (VectorEntity leftVec) (VectorEntity rightVec) = MI.unResolve result makeVector
            where result = A.minus leftVec rightVec
        minus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
            where result = A.minus leftMatrix rightMatrix
        minus _ _ = MI.withError MI.InvalidType

    instance A.Multipliable MathEntity where
        mult (ScalarEntity leftScalar) (ScalarEntity rightScalar) = scalarResult $ A.unsafeMult leftScalar rightScalar
        mult (ScalarEntity leftScalar) (VectorEntity rightVector) = vectorResult $ BS.unsafeRightScalarMult leftScalar rightVector
        mult (ScalarEntity leftScalar) (MatrixEntity rightMatrix) = matrixResult $ BS.unsafeRightScalarMult leftScalar rightMatrix
        mult (VectorEntity leftVector) (ScalarEntity rightScalar) = vectorResult $ BS.unsafeLeftScalarMult leftVector rightScalar
        mult (VectorEntity leftVector) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
            where result = BM.rightVectorMult leftVector rightMatrix
        mult (MatrixEntity leftMatrix) (ScalarEntity rightScalar) = matrixResult $ BS.unsafeLeftScalarMult leftMatrix rightScalar
        mult (MatrixEntity leftMatrix) (VectorEntity rightVector) = MI.unResolve result makeMatrix
            where result = BM.leftVectorMult leftMatrix rightVector
        mult (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
            where result = A.mult leftMatrix rightMatrix
        mult _ _ = MI.withError MI.InvalidType

    instance A.Scalable MathEntity where
        scale (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeVector
            where result = A.scale leftVector rightVector
        scale (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
            where result = A.scale leftMatrix rightMatrix
        scale _ _ = MI.withError MI.InvalidType    

    dot :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    dot (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeScalar
        where result = BV.dot leftVector rightVector
    dot _ _ = MI.withError MI.InvalidType

    cross :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    cross (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeVector
        where result = BV.cross leftVector rightVector
    cross _ _ = MI.withError MI.InvalidType

    instance A.Divisible MathEntity where
        div (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
            where result = A.div leftScalar rightScalar
        div (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = MI.unResolve result makeMatrix
            where result = A.div leftMatrix rightMatrix
        div _ _ = MI.withError MI.InvalidType
        inv (ScalarEntity scalar) = MI.unResolve result makeScalar
            where result = A.inv scalar
        inv (MatrixEntity matrix) = MI.unResolve result makeMatrix
            where result = A.inv matrix
        inv _ = MI.withError MI.InvalidType

    intDiv :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    intDiv (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.intDiv leftScalar rightScalar
    intDiv _ _ = MI.withError MI.InvalidType

    mod :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    mod (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.modulo leftScalar rightScalar
    mod _ _ = MI.withError MI.InvalidType

    rem :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    rem (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.remainder leftScalar rightScalar
    rem _ _ = MI.withError MI.InvalidType

    abs :: MathEntity -> MI.ComputationResult MathEntity
    abs (ScalarEntity scalar) = scalarResult $ BS.unsafeAbsol scalar
    abs (VectorEntity vector) = scalarResult $ BS.unsafeAbsol vector
    abs _ = MI.withError MI.InvalidType

    lcm :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    lcm (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.lcm leftScalar rightScalar
    lcm _ _ = MI.withError MI.InvalidType

    gcd :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    gcd (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.gcd leftScalar rightScalar
    gcd _ _ = MI.withError MI.InvalidType

    instance A.Negatable MathEntity where
        neg (ScalarEntity scalar) = scalarResult $ A.unsafeNeg scalar
        neg (VectorEntity vector) = vectorResult $ A.unsafeNeg vector
        neg (MatrixEntity matrix) = matrixResult $ A.unsafeNeg matrix
        neg _ = MI.withError MI.InvalidType

    pow :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    pow (ScalarEntity leftScalar) (ScalarEntity rightScalar) = scalarResult $ A.unsafePow leftScalar rightScalar
    pow _ _ = MI.withError MI.InvalidType

    sqrt :: MathEntity -> MI.ComputationResult MathEntity
    sqrt (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = A.sqrt scalar
    sqrt _ = MI.withError MI.InvalidType

    conj :: MathEntity -> MI.ComputationResult MathEntity
    conj (ScalarEntity scalar) = scalarResult $ BS.conj scalar
    conj _ = MI.withError MI.InvalidType

    norm :: MathEntity -> MI.ComputationResult MathEntity
    norm (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.norm scalar
    norm (VectorEntity vector) = MI.unResolve result makeVector
        where result = BS.norm vector
    norm _ = MI.withError MI.InvalidType

    arg :: MathEntity -> MI.ComputationResult MathEntity
    arg (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.arg scalar
    arg _ = MI.withError MI.InvalidType

    exp :: MathEntity -> MI.ComputationResult MathEntity
    exp (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = A.exp scalar
    exp _ = MI.withError MI.InvalidType

    log :: MathEntity -> MI.ComputationResult MathEntity
    log (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = A.log scalar
    log _ = MI.withError MI.InvalidType

    logBase :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    logBase (ScalarEntity base) (ScalarEntity arg) = MI.unResolve result makeScalar
        where result = A.logBase base arg
    logBase _ _ = MI.withError MI.InvalidType

    sin :: MathEntity -> MI.ComputationResult MathEntity
    sin (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sine scalar
    sin _ = MI.withError MI.InvalidType

    cos :: MathEntity -> MI.ComputationResult MathEntity
    cos (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.cosine scalar
    cos _ = MI.withError MI.InvalidType

    tan :: MathEntity -> MI.ComputationResult MathEntity
    tan (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.tangent scalar
    tan _ = MI.withError MI.InvalidType

    sinh :: MathEntity -> MI.ComputationResult MathEntity
    sinh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.sineHyperbolic scalar
    sinh _ = MI.withError MI.InvalidType

    cosh :: MathEntity -> MI.ComputationResult MathEntity
    cosh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.cosineHyperbolic scalar
    cosh _ = MI.withError MI.InvalidType

    tanh :: MathEntity -> MI.ComputationResult MathEntity
    tanh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.tangentHyperbolic scalar
    tanh _ = MI.withError MI.InvalidType

    asin :: MathEntity -> MI.ComputationResult MathEntity
    asin (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.arcsine scalar
    asin _ = MI.withError MI.InvalidType

    acos :: MathEntity -> MI.ComputationResult MathEntity
    acos (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.arccosine scalar
    acos _ = MI.withError MI.InvalidType

    atan :: MathEntity -> MI.ComputationResult MathEntity
    atan (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.arctangent scalar
    atan _ = MI.withError MI.InvalidType

    atan2 :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    atan2 (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = BS.arctangent2 leftScalar rightScalar
    atan2 _ _ = MI.withError MI.InvalidType

    asinh :: MathEntity -> MI.ComputationResult MathEntity
    asinh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.arcsineHyperbolic scalar
    asinh _ = MI.withError MI.InvalidType

    acosh :: MathEntity -> MI.ComputationResult MathEntity
    acosh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.arccosineHyperbolic scalar
    acosh _ = MI.withError MI.InvalidType

    atanh :: MathEntity -> MI.ComputationResult MathEntity
    atanh (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.arctangentHyperbolic scalar
    atanh _ = MI.withError MI.InvalidType

    min :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    min (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = A.min leftScalar rightScalar
    min _ _ = MI.withError MI.InvalidType

    findMin :: MathEntity -> MI.ComputationResult MathEntity
    findMin (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.findMin vector
    findMin _ = MI.withError MI.InvalidType

    max :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    max (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeScalar
        where result = A.max leftScalar rightScalar
    max _ _ = MI.withError MI.InvalidType

    findMax :: MathEntity -> MI.ComputationResult MathEntity
    findMax (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.findMax vector
    findMax _ = MI.withError MI.InvalidType

    less :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    less (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = A.less leftScalar rightScalar
    less _ _ = MI.withError MI.InvalidType

    lessEqual :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    lessEqual (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = A.lessEqual leftScalar rightScalar
    lessEqual _ _ = MI.withError MI.InvalidType

    greater :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    greater (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = A.greater leftScalar rightScalar
    greater _ _ = MI.withError MI.InvalidType

    greaterEqual :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    greaterEqual (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.unResolve result makeBool
        where result = A.greaterEqual leftScalar rightScalar
    greaterEqual _ _ = MI.withError MI.InvalidType

    ceiling :: MathEntity -> MI.ComputationResult MathEntity
    ceiling (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.roundUp scalar
    ceiling _ = MI.withError MI.InvalidType

    floor :: MathEntity -> MI.ComputationResult MathEntity
    floor (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.roundDown scalar
    floor _ = MI.withError MI.InvalidType

    round :: MathEntity -> MI.ComputationResult MathEntity
    round (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.roundOff scalar
    round _ = MI.withError MI.InvalidType

    even :: MathEntity -> MI.ComputationResult MathEntity
    even (ScalarEntity scalar) = boolResult $ BS.isEven scalar
    even _ = MI.withError MI.InvalidType

    odd :: MathEntity -> MI.ComputationResult MathEntity
    odd (ScalarEntity scalar) = boolResult $ BS.isOdd scalar
    odd _ = MI.withError MI.InvalidType

    size :: MathEntity -> MI.ComputationResult MathEntity
    size (VectorEntity vector) = scalarResult $ BV.size1 vector
    size _ = MI.withError MI.InvalidType

    equalSize :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    equalSize (VectorEntity leftVector) (VectorEntity rightVector) = boolResult $ BV.equalSize leftVector rightVector
    equalSize (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = boolResult $ BM.equalSize leftMatrix rightMatrix
    equalSize _ _ = MI.withError MI.InvalidType

    rows :: MathEntity -> MI.ComputationResult MathEntity
    rows (MatrixEntity matrix) = scalarResult $ BM.rows matrix
    rows _ = MI.withError MI.InvalidType

    cols :: MathEntity -> MI.ComputationResult MathEntity
    cols (MatrixEntity matrix) = scalarResult $ BM.cols matrix
    cols _ = MI.withError MI.InvalidType

    isSquare :: MathEntity -> MI.ComputationResult MathEntity
    isSquare (MatrixEntity matrix) = boolResult $ BM.isSquare matrix
    isSquare _ = MI.withError MI.InvalidType

    isNull :: MathEntity -> MI.ComputationResult MathEntity
    isNull (VectorEntity vector) = boolResult $ BV.isNull vector
    isNull _ = MI.withError MI.InvalidType

    dist :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    dist (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeScalar
        where result = BV.dist leftVector rightVector
    dist _ _ = MI.withError MI.InvalidType

    angle :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    angle (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeScalar
        where result = BV.angle leftVector rightVector
    angle _ _ = MI.withError MI.InvalidType

    sum :: MathEntity -> MI.ComputationResult MathEntity
    sum (VectorEntity vector) = scalarResult $ BV.sum vector
    sum _ = MI.withError MI.InvalidType

    cumsum :: MathEntity -> MI.ComputationResult MathEntity
    cumsum (VectorEntity vector) = vectorResult $ BV.cumsum vector
    cumsum _ = MI.withError MI.InvalidType

    prod :: MathEntity -> MI.ComputationResult MathEntity
    prod (VectorEntity vector) = scalarResult $ BV.prod vector
    prod _ = MI.withError MI.InvalidType

    cumprod :: MathEntity -> MI.ComputationResult MathEntity
    cumprod (VectorEntity vector) = vectorResult $ BV.cumprod vector
    cumprod _ = MI.withError MI.InvalidType

    mean :: MathEntity -> MI.ComputationResult MathEntity
    mean (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.mean vector
    mean _ = MI.withError MI.InvalidType

    gmean :: MathEntity -> MI.ComputationResult MathEntity
    gmean (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.gmean vector
    gmean _ = MI.withError MI.InvalidType

    hmean :: MathEntity -> MI.ComputationResult MathEntity
    hmean (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.hmean vector
    hmean _ = MI.withError MI.InvalidType

    median :: MathEntity -> MI.ComputationResult MathEntity
    median (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.median vector
    median _ = MI.withError MI.InvalidType

    range :: MathEntity -> MI.ComputationResult MathEntity
    range (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.range vector
    range _ = MI.withError MI.InvalidType

    midrange :: MathEntity -> MI.ComputationResult MathEntity
    midrange (VectorEntity vector) = MI.unResolve result makeScalar
        where result = BV.midrange vector
    midrange _ = MI.withError MI.InvalidType

    mode :: MathEntity -> MI.ComputationResult MathEntity
    mode (VectorEntity vector) = MI.unResolve result makeVector
        where result = BV.mode vector
    mode _ = MI.withError MI.InvalidType

    sortAsc :: MathEntity -> MI.ComputationResult MathEntity
    sortAsc (VectorEntity vector) = MI.unResolve result makeVector
        where result = BV.sortAsc vector
    sortAsc _ = MI.withError MI.InvalidType

    sortDesc :: MathEntity -> MI.ComputationResult MathEntity
    sortDesc (VectorEntity vector) = MI.unResolve result makeVector
        where result = BV.sortDesc vector
    sortDesc _ = MI.withError MI.InvalidType

    isSortedAsc :: MathEntity -> MI.ComputationResult MathEntity
    isSortedAsc (VectorEntity vector) = MI.unResolve result makeBool
        where result = BV.isSortedAsc vector
    isSortedAsc _ = MI.withError MI.InvalidType

    isSortedDesc :: MathEntity -> MI.ComputationResult MathEntity
    isSortedDesc (VectorEntity vector) = MI.unResolve result makeBool
        where result = BV.isSortedDesc vector
    isSortedDesc _ = MI.withError MI.InvalidType
    
    isSorted :: MathEntity -> MI.ComputationResult MathEntity
    isSorted (VectorEntity vector) = MI.unResolve result makeBool
        where result = BV.isSorted vector
    isSorted _ = MI.withError MI.InvalidType

    sub1 :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    sub1 (VectorEntity vector) (ScalarEntity lowIndex) (ScalarEntity highIndex) = MI.unResolve result makeVector
        where result = BV.sub1 vector lowIndex highIndex
    sub1 _ _ _ = MI.withError MI.InvalidType

    concat :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    concat (VectorEntity leftVector) (VectorEntity rightVector) = vectorResult $ BV.concat leftVector rightVector
    concat _ _ = MI.withError MI.InvalidType

    merge :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    merge (VectorEntity leftVector) (VectorEntity rightVector) = MI.unResolve result makeVector
        where result = BV.merge leftVector rightVector
    merge _ _ = MI.withError MI.InvalidType

    rowVector :: MathEntity -> MI.ComputationResult MathEntity
    rowVector (VectorEntity vector) = matrixResult $ BM.rowVector vector
    rowVector _ = MI.withError MI.InvalidType

    colVector :: MathEntity -> MI.ComputationResult MathEntity
    colVector (VectorEntity vector) = matrixResult $ BM.colVector vector
    colVector _ = MI.withError MI.InvalidType

    det :: MathEntity -> MI.ComputationResult MathEntity
    det (MatrixEntity matrix) = MI.unResolve result makeScalar
        where result = BM.det matrix
    det _ = MI.withError MI.InvalidType

    transpose :: MathEntity -> MI.ComputationResult MathEntity
    transpose (MatrixEntity matrix) = matrixResult $ BM.transpose matrix
    transpose _ = MI.withError MI.InvalidType

    submatrix :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    submatrix (MatrixEntity matrix) (ScalarEntity lowIndex) (ScalarEntity highIndex) = MI.unResolve result makeMatrix
        where result = BM.sub2 matrix lowIndex highIndex
    submatrix _ _ _ = MI.withError MI.InvalidType

    addRow :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    addRow (MatrixEntity matrix) (ScalarEntity fromRowIndex) (ScalarEntity toRowIndex) = MI.unResolve result makeMatrix
        where result = BM.addRow matrix fromRowIndex toRowIndex
    addRow _ _ _ = MI.withError MI.InvalidType

    multRow :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    multRow (MatrixEntity matrix) (ScalarEntity multValue) (ScalarEntity rowIndex) = MI.unResolve result makeMatrix
        where result = BM.multRow matrix multValue rowIndex
    multRow _ _ _ = MI.withError MI.InvalidType

    swapRows :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    swapRows (MatrixEntity matrix) (ScalarEntity rowIndex1) (ScalarEntity rowIndex2) = MI.unResolve result makeMatrix
        where result = BM.swapRows matrix rowIndex1 rowIndex2
    swapRows _ _ _ = MI.withError MI.InvalidType

    addCol :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    addCol (MatrixEntity matrix) (ScalarEntity fromColIndex) (ScalarEntity toColIndex) = MI.unResolve result makeMatrix
        where result = BM.addCol matrix fromColIndex toColIndex
    addCol _ _ _ = MI.withError MI.InvalidType

    multCol :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    multCol (MatrixEntity matrix) (ScalarEntity multValue) (ScalarEntity colIndex) = MI.unResolve result makeMatrix
        where result = BM.multCol matrix multValue colIndex
    multCol _ _ _ = MI.withError MI.InvalidType

    swapCols :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    swapCols (MatrixEntity matrix) (ScalarEntity colIndex1) (ScalarEntity colIndex2) = MI.unResolve result makeMatrix
        where result = BM.swapCols matrix colIndex1 colIndex2
    swapCols _ _ _ = MI.withError MI.InvalidType

    not :: MathEntity -> MI.ComputationResult MathEntity
    not (BoolEntity True) = boolResult False
    not (BoolEntity False) = boolResult True
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
        where result = BV.get1 vec index
    get1 _ _ = MI.withError MI.InvalidType

    get2 :: MathEntity -> MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    get2 (MatrixEntity matrix) (ScalarEntity rowIndex) (ScalarEntity colIndex) = MI.unResolve result makeScalar
        where result = BM.get2 matrix rowIndex colIndex
    get2 _ _ _ = MI.withError MI.InvalidType

    identity :: MathEntity -> MI.ComputationResult MathEntity
    identity (ScalarEntity dimensions) = MI.unResolve result makeMatrix
        where result = BM.identity dimensions
    identity _ = MI.withError MI.InvalidType

    toBinary :: MathEntity -> MI.ComputationResult String
    toBinary (ScalarEntity scalar) = BS.toBinary scalar
    toBinary _ = MI.withError MI.InvalidType

    toHexadecimal :: MathEntity -> MI.ComputationResult String
    toHexadecimal (ScalarEntity scalar) = BS.toHexadecimal scalar
    toHexadecimal _ = MI.withError MI.InvalidType

    toOctal :: MathEntity -> MI.ComputationResult String
    toOctal (ScalarEntity scalar) = BS.toOctal scalar
    toOctal _ = MI.withError MI.InvalidType

    factorial :: MathEntity -> MI.ComputationResult MathEntity
    factorial (ScalarEntity scalar) = MI.unResolve result makeScalar
        where result = BS.factorial scalar
    factorial _ = MI.withError MI.InvalidType

    choose :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    choose (ScalarEntity choices) (ScalarEntity picks) = MI.unResolve result makeScalar
        where result = BS.choose choices picks
    choose _ _ = MI.withError MI.InvalidType

    perm :: MathEntity -> MathEntity -> MI.ComputationResult MathEntity
    perm (ScalarEntity choices) (ScalarEntity picks) = MI.unResolve result makeScalar
        where result = BS.perm choices picks
    perm _ _ = MI.withError MI.InvalidType

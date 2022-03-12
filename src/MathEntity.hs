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
    isScalar,
    isVector,
    isList,
    isMatrix,
    isBool,
    plus,
    minus,
    mult,
    H.hash,
    H.hashWithSalt,
    (==),
    (/=),
    show
) where
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

    instance H.Hashable MathEntity

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

    isScalar :: MathEntity -> Bool
    isScalar ScalarEntity{} = True
    isScalar _ = False

    isVector :: MathEntity -> Bool
    isVector VectorEntity{} = True
    isVector _ = False

    isList :: MathEntity -> Bool
    isList ListEntity{} = True
    isList _ = False

    isMatrix :: MathEntity -> Bool
    isMatrix MatrixEntity{} = True
    isMatrix _ = False

    isBool :: MathEntity -> Bool
    isBool BoolEntity{} = True
    isBool _ = False

    _entity :: MI.MathResult a -> (a -> MathEntity) -> MI.MathResult MathEntity
    _entity result constructor
        | MI.isSuccess result = MI.withValue $ constructor val
        | otherwise = MI.convert result
        where val = MI.value result

    plus :: MathEntity -> MathEntity -> MI.MathResult MathEntity
    plus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ ScalarEntity $ BS.splus leftScalar rightScalar
    plus (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ ListEntity $ BL.splusl leftScalar rightList
    plus (VectorEntity leftVec) (VectorEntity rightVec) = _entity result VectorEntity
        where result = BV.vplus leftVec rightVec
    plus (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ ListEntity $ BL.lpluss leftList rightScalar
    plus (ListEntity leftList) (ListEntity rightList) = _entity result ListEntity
        where result = BL.lplus leftList rightList
    plus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = _entity result MatrixEntity
        where result = BM.mplus leftMatrix rightMatrix
    plus _ _ = MI.withError MI.IncompatibleTypes

    minus :: MathEntity -> MathEntity -> MI.MathResult MathEntity
    minus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ ScalarEntity $ BS.sminus leftScalar rightScalar
    minus (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ ListEntity $ BL.sminusl leftScalar rightList
    minus (VectorEntity leftVec) (VectorEntity rightVec) = _entity result VectorEntity
        where result = BV.vminus leftVec rightVec
    minus (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ ListEntity $ BL.lminuss leftList rightScalar
    minus (ListEntity leftList) (ListEntity rightList) = _entity result ListEntity
        where result = BL.lminus leftList rightList
    minus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = _entity result MatrixEntity
        where result = BM.mminus leftMatrix rightMatrix
    minus _ _ = MI.withError MI.IncompatibleTypes

    mult :: MathEntity -> MathEntity -> MI.MathResult MathEntity
    mult (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ ScalarEntity $ BS.splus leftScalar rightScalar
    mult (ScalarEntity leftScalar) (VectorEntity rightVector) = _entity result VectorEntity
        where result = BV.smultv leftScalar rightVector
    mult (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ ListEntity $ BL.smultl leftScalar rightList
    mult (ScalarEntity leftScalar) (MatrixEntity rightMatrix) = MI.withValue $ MatrixEntity $ BM.smultm leftScalar rightMatrix
    mult (VectorEntity leftVector) (ScalarEntity rightScalar) = _entity result VectorEntity
        where result = BV.vmults leftVector rightScalar
    mult (VectorEntity leftVector) (MatrixEntity rightMatrix) = _entity result MatrixEntity
        where result = BM.vmultm leftVector rightMatrix
    mult (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ ListEntity $ BL.lmults leftList rightScalar
    mult (ListEntity leftList) (ListEntity rightList) = _entity result ListEntity
        where result = BL.lmult leftList rightList
    mult (MatrixEntity leftMatrix) (ScalarEntity rightScalar) = MI.withValue $ MatrixEntity $ BM.mmults leftMatrix rightScalar
    mult (MatrixEntity leftMatrix) (VectorEntity rightVector) = _entity result MatrixEntity
        where result = BM.mmultv leftMatrix rightVector
    mult (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = _entity result MatrixEntity
        where result = BM.mmult leftMatrix rightMatrix
    
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
    entityPlus,
    entityMinus,
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

    entityPlus :: MathEntity -> MathEntity -> MI.MathResult MathEntity
    entityPlus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ ScalarEntity $ BS.splus leftScalar rightScalar
    entityPlus (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ ListEntity $ BL.splusl leftScalar rightList
    entityPlus (VectorEntity leftVec) (VectorEntity rightVec) = _entity result VectorEntity
        where result = BV.vplus leftVec rightVec
    entityPlus (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ ListEntity $ BL.lpluss leftList rightScalar
    entityPlus (ListEntity leftList) (ListEntity rightList) = _entity result ListEntity
        where result = BL.lplus leftList rightList
    entityPlus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = _entity result MatrixEntity
        where result = BM.mplus leftMatrix rightMatrix
    entityPlus _ _ = MI.withError MI.IncompatibleTypes

    entityMinus :: MathEntity -> MathEntity -> MI.MathResult MathEntity
    entityMinus (ScalarEntity leftScalar) (ScalarEntity rightScalar) = MI.withValue $ ScalarEntity $ BS.sminus leftScalar rightScalar
    entityMinus (ScalarEntity leftScalar) (ListEntity rightList) = MI.withValue $ ListEntity $ BL.sminusl leftScalar rightList
    entityMinus (VectorEntity leftVec) (VectorEntity rightVec) = _entity result VectorEntity
        where result = BV.vminus leftVec rightVec
    entityMinus (ListEntity leftList) (ScalarEntity rightScalar) = MI.withValue $ ListEntity $ BL.lminuss leftList rightScalar
    entityMinus (ListEntity leftList) (ListEntity rightList) = _entity result ListEntity
        where result = BL.lminus leftList rightList
    entityMinus (MatrixEntity leftMatrix) (MatrixEntity rightMatrix) = _entity result MatrixEntity
        where result = BM.mminus leftMatrix rightMatrix
    entityMinus _ _ = MI.withError MI.IncompatibleTypes
    
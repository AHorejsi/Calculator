{-# LANGUAGE DeriveGeneric #-}

module MathEntity (
    MathEntity,
    EntityType(
        Integer,
        Real,
        Complex,
        Quaternion,
        Number,
        Vector,
        Matrix,
        Tensor,
        Tuple,
        Set,
        Boolean
    )
) where
    import Prelude hiding (abs, lcm, gcd, sqrt, exp, log, logBase, sin, cos, tan, sinh, cosh, tanh, asin, acos, atan, atan2, asinh, acosh, atanh, min, max, ceiling, floor, even, odd, sum, concat, not, and, or, mod, rem)
    import qualified GHC.Generics as G
    import qualified Data.Hashable as H
    import qualified Data.Sequence as S
    import qualified Data.Vector as V
    import qualified Actions as A
    import qualified BigNumber as BN
    import qualified BigVector as BV
    import qualified BigMatrix as BM
    import qualified BigTensor as BT
    import qualified BigSet as BS
    import qualified BigTuple as BL

    type EntityVector = BV.BigVector S.Seq
    type EntityMatrix = BM.BigMatrix S.Seq
    type EntityTensor = BT.BigTensor S.Seq V.Vector
    type EntitySet = BS.BigSet MathEntity
    type EntityTuple = BL.BigTuple MathEntity S.Seq

    data MathEntity =
        NumberEntity {
            _number :: BN.BigNumber
        } |
        VectorEntity {
            _vector :: EntityVector
        } |
        MatrixEntity {
            _matrix :: EntityMatrix
        } |
        TensorEntity {
            _tensor :: EntityTensor
        } |
        SetEntity {
            _set :: EntitySet
        } |
        TupleEntity {
            _tuple :: EntityTuple
        } |
        BoolEntity {
            _bool :: Bool
        } deriving (G.Generic)

    data EntityType =
        Integer |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
        Real |
        Complex |
        Quaternion |
        Number |
        Vector |
        Matrix |
        Tensor |
        Tuple |
        Set |
        Boolean
        deriving (Enum)

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module BigTensor (
    BigTensor
) where
    import Prelude hiding (negate, null)
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Foldable as Fo
    import qualified Data.Functor as Fu
    import qualified Data.Hashable as H
    import qualified Indexable as I
    import qualified Actions as A
    import qualified BigNumber as BN

    data BigTensor v d a = BigTensor {
        _values :: v (BN.BigNumber a),
        _dimensions :: d Int
    } deriving (G.Generic)

    instance (I.Indexable v, I.Indexable d, Eq a) => Eq (BigTensor v d a) where
        (==) (BigTensor leftVals leftDimensions) (BigTensor rightVals rightDimensions) = equalSize && equalValues
            where equalSize = I.equalIndexable leftDimensions rightDimensions
                  equalValues = I.equalIndexable leftVals rightVals

    instance (I.Indexable v, I.Indexable d, H.Hashable a) => H.Hashable (BigTensor v d a) where
        hashWithSalt salt (BigTensor values dimensions) = H.hashWithSalt dimensionHash valueHash
            where valueHash = I.hashIndexable salt values
                  dimensionHash = I.hashIndexable salt dimensions

    tensor :: (Fo.Foldable f, I.Indexable v, I.Indexable d) => f (BN.BigNumber a) -> f Int -> BigTensor v d a
    tensor values dimensions
        | (Fo.null values) || (Fo.null dimensions) = error "Must have at least one value and one dimension"
        | valueCount /= dimensionSize = error "Value count does not fit the specified dimensions"
        | otherwise = BigTensor containedValues containedDimensions
        where valueCount = Fo.length values
              dimensionSize = Fo.product dimensions
              containedValues = I.switch values
              containedDimensions = I.switch dimensions

    sizeInt :: (I.Indexable d) => BigTensor v d a -> Int -> Int
    sizeInt (BigTensor _ dimensions) = I.at dimensions

    size :: (RealFrac a, I.Indexable v, I.Indexable d) => BigTensor v d a -> BN.BigNumber a -> A.Computation (BN.BigNumber a)
    size tensor index
        | not $ BN.isInteger index = A.failure A.NotInteger "Index must be an integer"
        | otherwise = A.success $ BN.asNumber result
        where intIndex = BN.asIntegral index
              result = sizeInt tensor intIndex

    rankInt :: (I.Indexable d) => BigTensor v d a -> Int
    rankInt (BigTensor _ dimensions) = Fo.length dimensions

    rank :: (I.Indexable d, RealFrac a) => BigTensor v d a -> BN.BigNumber a
    rank = BN.asNumber . rankInt

    equalSize :: (I.Indexable d) => BigTensor v d a -> BigTensor v d a -> Bool
    equalSize (BigTensor _ leftDimensions) (BigTensor _ rightDimensions) = I.equalIndexable leftDimensions rightDimensions

    _binaryElementwise :: (I.Indexable v, I.Indexable d, Num a, Eq a) => BN.BinaryNumberAction a -> BigTensor v d a -> BigTensor v d a -> A.Computation (BigTensor v d a)
    _binaryElementwise action left@(BigTensor leftValues leftDimensions) right@(BigTensor rightValues _)
        | not $ equalSize left right = A.failure A.UnequalDimensions "Tensors must be of the same dimensions for this operation"
        | otherwise = A.success $ BigTensor (I.pairOn action leftValues rightValues) leftDimensions

    isScalar :: (Fo.Foldable v) => BigTensor v d a -> Bool
    isScalar (BigTensor values _) = 1 == (Fo.length values)

    isVector :: (I.Indexable d) => BigTensor v d a -> Bool
    isVector tensor = 1 == (rankInt tensor)

    isMatrix :: (I.Indexable d) => BigTensor v d a -> Bool
    isMatrix tensor = 2 == (rankInt tensor)

    plus :: (I.Indexable v, I.Indexable d, Num a, Eq a) => BigTensor v d a -> BigTensor v d a -> A.Computation (BigTensor v d a)
    plus = _binaryElementwise BN.plus

    minus :: (I.Indexable v, I.Indexable d, Num a, Eq a) => BigTensor v d a -> BigTensor v d a -> A.Computation (BigTensor v d a)
    minus = _binaryElementwise BN.minus

    scale :: (I.Indexable v, I.Indexable d, Num a, Eq a) => BigTensor v d a -> BigTensor v d a -> A.Computation (BigTensor v d a)
    scale = _binaryElementwise BN.multiply

    _unaryElementwise :: (Fu.Functor v, Num a, Eq a) => BN.UnaryNumberAction a -> BigTensor v d a -> BigTensor v d a
    _unaryElementwise action (BigTensor values dimensions) = BigTensor (Fu.fmap action values) dimensions

    scalarMultiplyLeft :: (Fu.Functor v, Num a, Eq a) => BN.BigNumber a -> BigTensor v d a -> BigTensor v d a
    scalarMultiplyLeft left = _unaryElementwise (BN.multiply left)

    scalarMultiplyRight :: (Fu.Functor v, Num a, Eq a) => BigTensor v d a -> BN.BigNumber a -> BigTensor v d a
    scalarMultiplyRight left right = _unaryElementwise (`BN.multiply` right) left

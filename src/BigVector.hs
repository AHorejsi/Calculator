{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module BigVector (
    BigVector,
    vector,
    sizeInt,
    size,
    null,
    equalSize,
    getInt,
    get,
    plus,
    minus,
    scale,
    dot,
    cross2D,
    cross3D,
    scalarMultiplyLeft,
    scalarMultiplyRight,
    negate,
    absolute,
    normalize,
    angle,
    distance
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

    newtype BigVector v a = BigVector {
        _values :: v (BN.BigNumber a)
    } deriving (G.Generic)

    instance (I.Indexable v, Eq a) => Eq (BigVector v a) where
        (==) left@(BigVector leftValues) right@(BigVector rightValues) = I.equalIndexable leftValues rightValues

    instance (I.Indexable v, H.Hashable a) => H.Hashable (BigVector v a) where
        hashWithSalt salt (BigVector values) = I.hashIndexable salt values

    vector :: (Fo.Foldable f, I.Indexable v) => f (BN.BigNumber a) -> BigVector v a
    vector values
        | Fo.null values = error "Vectors must have a length of at least 1"
        | otherwise = BigVector $ I.switch values

    sizeInt :: (Fo.Foldable v) => BigVector v a -> Int
    sizeInt (BigVector values) = Fo.length values

    size :: (Fo.Foldable v, Num a, Eq a) => BigVector v a -> BN.BigNumber a
    size = BN.asNumber . sizeInt

    null :: (Fo.Foldable v, Num a, Eq a) => BigVector v a -> Bool
    null (BigVector values) = Fo.all (==BN.zeroValue) values

    equalSize :: (Fo.Foldable v1, Fo.Foldable v2) => BigVector v1 a -> BigVector v2 b -> Bool
    equalSize left right = (sizeInt left) == (sizeInt right)

    getInt :: (I.Indexable v) => BigVector v a -> Int -> A.Computation (BN.BigNumber a)
    getInt vec@(BigVector values) index
        | (index < 0) || (index >= size) = A.failure A.IndexOutOfBounds "Index outside the bounds of the vector"
        | otherwise = A.success $ I.at values index
        where size = sizeInt vec

    get :: (I.Indexable v, RealFrac b) => BigVector v a -> BN.BigNumber b -> A.Computation (BN.BigNumber a)
    get vec index
        | not $ BN.isInteger index = A.failure A.NotInteger "All indices must be integers"
        | otherwise = getInt vec intIndex
        where intIndex = BN.asIntegral index

    _binaryElementwise :: (I.Indexable v, Num a) => BN.BinaryNumberAction a -> BigVector v a -> BigVector v a -> A.Computation (BigVector v a)
    _binaryElementwise action left@(BigVector leftValues) right@(BigVector rightValues)
        | not $ equalSize left right = A.failure A.UnequalDimensions "Vectors must be of equal dimensions for this action"
        | otherwise = (A.success . BigVector) $ I.pairOn action leftValues rightValues

    plus :: (I.Indexable v, Num a, Eq a) => BigVector v a -> BigVector v a -> A.Computation (BigVector v a)
    plus = _binaryElementwise BN.plus

    minus :: (I.Indexable v, Num a, Eq a) => BigVector v a -> BigVector v a -> A.Computation (BigVector v a)
    minus = _binaryElementwise BN.minus

    scale :: (I.Indexable v, Num a, Eq a) => BigVector v a -> BigVector v a -> A.Computation (BigVector v a)
    scale = _binaryElementwise BN.multiply

    dot :: (I.Indexable v, Num a, Eq a) => BigVector v a -> BigVector v a -> A.Computation (BN.BigNumber a)
    dot left@(BigVector leftValues) right@(BigVector rightValues)
        | not $ equalSize left right = A.failure A.UnequalDimensions "Vectors must have the same length for this operation"
        | otherwise = A.success $ Fo.foldr BN.plus BN.zeroValue (I.pairOn BN.multiply leftValues rightValues)

    cross2D :: (I.Indexable v, Num a, Eq a) => BigVector v a -> BigVector v a -> A.Computation (BN.BigNumber a)
    cross2D left right
        | 2 == leftSize && leftSize == rightSize = A.success $ BN.minus (BN.multiply leftXPos rightYPos) (BN.multiply leftYPos rightXPos)
        | otherwise = A.failure A.InvalidInput "Cross product can only be applied to 2D or 3D vectors"
        where leftSize = sizeInt left
              rightSize = sizeInt right
              leftXPos = A.value $ getInt left 0
              leftYPos = A.value $ getInt left 1
              rightXPos = A.value $ getInt right 0
              rightYPos = A.value $ getInt right 1

    cross3D :: (I.Indexable v, Num a, Eq a) => BigVector v a -> BigVector v a -> A.Computation (BigVector v a)
    cross3D left right
        | 3 == leftSize && leftSize == rightSize = (A.success . BigVector) $ I.switch [resultXPos, resultYPos, resultZPos]
        | otherwise = A.failure A.InvalidInput "Cross product can only be applied to 2D or 3D vectors"
        where leftSize = sizeInt left
              rightSize = sizeInt right
              leftXPos = A.value $ getInt left 0
              leftYPos = A.value $ getInt left 1
              leftZPos = A.value $ getInt left 2
              rightXPos = A.value $ getInt right 0
              rightYPos = A.value $ getInt right 1
              rightZPos = A.value $ getInt right 2
              resultXPos = BN.minus (BN.multiply leftYPos rightZPos) (BN.multiply leftZPos rightYPos)
              resultYPos = BN.minus (BN.multiply leftZPos rightXPos) (BN.multiply leftXPos rightZPos)
              resultZPos = BN.minus (BN.multiply leftXPos rightYPos) (BN.multiply leftYPos rightXPos)

    _unaryElementwise :: (Fu.Functor v, Num a) => BN.UnaryNumberAction a -> BigVector v a -> BigVector v a
    _unaryElementwise action (BigVector values) = BigVector $ Fu.fmap action values

    scalarMultiplyLeft :: (Fu.Functor v, Num a, Eq a) => BN.BigNumber a -> BigVector v a -> BigVector v a
    scalarMultiplyLeft left = _unaryElementwise (BN.multiply left)

    scalarMultiplyRight :: (Fu.Functor v, Num a, Eq a) => BigVector v a -> BN.BigNumber a -> BigVector v a
    scalarMultiplyRight left right = _unaryElementwise (`BN.multiply` right) left

    negate :: (Fu.Functor v, Num a, Eq a) => BigVector v a -> BigVector v a
    negate = scalarMultiplyLeft BN.negOneValue

    absolute :: (I.Indexable v, RealFloat a) => BigVector v a -> BN.BigNumber a
    absolute vec = (BN.squareRoot . A.value) $ dot vec vec

    normalize :: (I.Indexable v, RealFloat a, Eq a) => BigVector v a -> A.Computation (BigVector v a)
    normalize vec = A.resolveBinary denominator (A.success vec) scalarMultiplyLeft
        where denominator = BN.inverse $ absolute vec

    angle :: (I.Indexable v, RealFloat a, Eq a) => BigVector v a -> BigVector v a -> A.Computation (BN.BigNumber a)
    angle left right
        | (not $ equalSize left right) || (null left) || (null right) = A.failure A.InvalidInput "Vectors must be of equal dimensions and neither can be null"
        | otherwise = (BN.arccosine . A.value) $ BN.divide dotProd absProd
        where dotProd = A.value $ dot left right
              absProd = BN.multiply (absolute left) (absolute right)

    distance :: (I.Indexable v, RealFloat a) => BigVector v a -> BigVector v a -> A.Computation (BN.BigNumber a)
    distance left right
        | not $ equalSize left right = A.failure A.UnequalDimensions "Vectors must have equal dimensions for this operation"
        | otherwise = ((A.success . BN.squareRoot) . (Fo.foldr BN.plus BN.zeroValue)) $ Fu.fmap square (_values subtValue)
        where subtValue = A.value $ minus left right
              square = (`BN.power` BN.twoValue)

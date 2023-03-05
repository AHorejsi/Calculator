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
    distance,
    toContainer
) where
    import Prelude hiding (negate, null)
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Foldable as Fo
    import qualified Data.Functor as Fu
    import qualified Data.Hashable as H
    import qualified Indexable as I
    import qualified Actions as A
    import qualified Equality as E
    import qualified BigNumber as BN

    newtype BigVector v = BigVector {
        _values :: v BN.BigNumber
    } deriving (G.Generic)

    instance (I.Indexable v) => Eq (BigVector v) where
        (==) left@(BigVector leftValues) right@(BigVector rightValues) = sizeComp && valueComp
            where sizeComp = equalSize left right
                  valueComp = leftValues == rightValues

    instance (I.Indexable v) => E.Equality (BigVector v) where
        eq left right = A.success $ left == right

    instance (I.Indexable v) => H.Hashable (BigVector v) where
        hashWithSalt salt (BigVector values) = H.hashWithSalt salt values

    vector :: (Fo.Foldable f, I.Indexable v) => f BN.BigNumber -> BigVector v
    vector values
        | Fo.null values = error "Vectors must have a length of at least 1"
        | otherwise = BigVector $ I.switch values

    sizeInt :: (Fo.Foldable v) => BigVector v -> Int
    sizeInt (BigVector values) = Fo.length values

    size :: (Fo.Foldable v) => BigVector v -> BN.BigNumber
    size = BN.asNumber . sizeInt

    null :: (Fo.Foldable v) => BigVector v -> Bool
    null (BigVector values) = Fo.all (==BN.zero) values

    equalSize :: (Fo.Foldable v) => BigVector v -> BigVector v -> Bool
    equalSize left right = (sizeInt left) == (sizeInt right)

    getInt :: (I.Indexable v) => BigVector v -> Int -> A.Computation BN.BigNumber
    getInt vec@(BigVector values) index
        | (index < 0) || (index >= size) = A.failure A.IndexOutOfBounds "Index outside the bounds of the vector"
        | otherwise = A.success $ I.at values index
        where size = sizeInt vec

    get :: (I.Indexable v) => BigVector v -> BN.BigNumber -> A.Computation BN.BigNumber
    get vec index
        | not $ BN.isInteger index = A.failure A.NotInteger "All indices must be integers"
        | otherwise = getInt vec intIndex
        where intIndex = BN.asIntegral index

    _binaryElementwise :: (I.Indexable v) => BN.BinaryNumberAction -> BigVector v -> BigVector v -> A.Computation (BigVector v)
    _binaryElementwise action left@(BigVector leftValues) right@(BigVector rightValues)
        | not $ equalSize left right = A.failure A.UnequalDimensions "Vectors must be of equal dimensions for this action"
        | otherwise = (A.success . BigVector) $ I.pairOn action leftValues rightValues

    plus :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation (BigVector v)
    plus = _binaryElementwise BN.plus

    minus :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation (BigVector v)
    minus = _binaryElementwise BN.minus

    scale :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation (BigVector v)
    scale = _binaryElementwise BN.multiply

    dot :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation BN.BigNumber
    dot left@(BigVector leftValues) right@(BigVector rightValues)
        | not $ equalSize left right = A.failure A.UnequalDimensions "Vectors must have the same length for this operation"
        | otherwise = A.success $ Fo.foldr BN.plus BN.zero (I.pairOn BN.multiply leftValues rightValues)

    cross2D :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation BN.BigNumber
    cross2D left right
        | 2 == leftSize && leftSize == rightSize = A.success $ BN.minus (BN.multiply leftXPos rightYPos) (BN.multiply leftYPos rightXPos)
        | otherwise = A.failure A.InvalidInput "Cross product can only be applied to 2D or 3D vectors"
        where leftSize = sizeInt left
              rightSize = sizeInt right
              leftXPos = A.value $ getInt left 0
              leftYPos = A.value $ getInt left 1
              rightXPos = A.value $ getInt right 0
              rightYPos = A.value $ getInt right 1

    cross3D :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation (BigVector v)
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

    _unaryElementwise :: (Fu.Functor v) => BN.UnaryNumberAction -> BigVector v -> BigVector v
    _unaryElementwise action (BigVector values) = BigVector $ Fu.fmap action values

    scalarMultiplyLeft :: (Fu.Functor v) => BN.BigNumber -> BigVector v -> BigVector v
    scalarMultiplyLeft left = _unaryElementwise (BN.multiply left)

    scalarMultiplyRight :: (Fu.Functor v) => BigVector v -> BN.BigNumber -> BigVector v
    scalarMultiplyRight left right = _unaryElementwise (`BN.multiply` right) left

    negate :: (Fu.Functor v) => BigVector v -> BigVector v
    negate = scalarMultiplyLeft BN.negOne

    absolute :: (I.Indexable v) => BigVector v -> BN.BigNumber
    absolute vec = (BN.squareRoot . A.value) $ dot vec vec

    normalize :: (I.Indexable v) => BigVector v -> A.Computation (BigVector v)
    normalize vec = A.resolveBinary denominator (A.success vec) scalarMultiplyLeft
        where denominator = BN.inverse $ absolute vec

    angle :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation BN.BigNumber
    angle left right
        | (not $ equalSize left right) || (null left) || (null right) = A.failure A.InvalidInput "Vectors must be of equal dimensions and neither can be null"
        | otherwise = (BN.arccosine . A.value) $ BN.divide dotProd absProd
        where dotProd = A.value $ dot left right
              absProd = BN.multiply (absolute left) (absolute right)

    distance :: (I.Indexable v) => BigVector v -> BigVector v -> A.Computation BN.BigNumber
    distance left right
        | not $ equalSize left right = A.failure A.UnequalDimensions "Vectors must have equal dimensions for this operation"
        | otherwise = ((A.success . BN.squareRoot) . (Fo.foldr BN.plus BN.zero)) $ Fu.fmap square (_values subtValue)
        where subtValue = A.value $ minus left right
              square = (`BN.power` BN.two)

    toContainer :: (I.Indexable v1, I.Indexable v2) => BigVector v1 -> v2 BN.BigNumber
    toContainer (BigVector values) = I.switch values

{-# LANGUAGE DeriveGeneric #-}

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
    import qualified Equality as E
    import qualified BigNumber as BN

    data BigTensor v d = BigTensor {
        _values :: v BN.BigNumber,
        _dimensions :: d Int
    } deriving (G.Generic)

    instance (I.Indexable v, I.Indexable d) => Eq (BigTensor v d) where
        (==) (BigTensor leftVals leftDimensions) (BigTensor rightVals rightDimensions) = sizeComp && valueComp
            where sizeComp = leftDimensions == rightDimensions
                  valueComp = leftVals == rightVals

    instance (I.Indexable v, I.Indexable d) => E.Equality (BigTensor v d) where
        eq left right = A.success $ left == right

    instance (I.Indexable v, I.Indexable d) => H.Hashable (BigTensor v d) where
        hashWithSalt salt (BigTensor values dimensions) = H.hashWithSalt dimensionHash valueHash
            where valueHash = H.hashWithSalt salt values
                  dimensionHash = H.hashWithSalt salt dimensions

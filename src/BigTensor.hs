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
            where valueHash = Fo.sum $ Fu.fmap (H.hashWithSalt salt) values
                  dimensionHash = Fo.sum $ Fu.fmap (H.hashWithSalt salt) dimensions

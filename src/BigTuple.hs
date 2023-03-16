{-# LANGUAGE DeriveGeneric #-}

module BigTuple (
    BigTuple
) where
    import qualified GHC.Generics as G
    import qualified Data.Foldable as Fo
    import qualified Indexable as I

    newtype BigTuple v a = BigTuple {
        _values :: v a
    } deriving (G.Generic)

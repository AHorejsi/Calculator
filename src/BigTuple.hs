{-# LANGUAGE DeriveGeneric #-}

module BigTuple (
    BigTuple
) where
    import qualified GHC.Generics as G
    import qualified Data.Foldable as Fo
    import qualified Indexable as I

    newtype BigTuple a v = BigTuple {
        _values :: v a
    } deriving (G.Generic)

    tuple :: (Fo.Foldable f, I.Indexable v) => f a -> BigTuple a v
    tuple = BigTuple . I.switch

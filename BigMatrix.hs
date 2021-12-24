module BigMatrix (
    BigMatrix
) where
    import qualified Data.Vector as Vec
    import BigScalar

    newtype BigMatrix = BigMatrix {
        _table :: Vec.Vector BigScalar
    } deriving (Eq, Show)
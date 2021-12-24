module BigMatrix (
    BigMatrix,
    list,
    (==),
    (/=),
    show
) where
    import Text.Printf
    import qualified Data.Vector as V
    import MathInfo
    import BigScalar

    data BigMatrix = BigMatrix {
        _table :: V.Vector BigScalar,
        _rows :: Int,
        _cols :: Int
    } deriving (Eq)

    instance Show BigMatrix where
        show (BigMatrix table rows cols) = printf "Matrix\n%s" (_str table rows cols)

    list :: [[BigScalar]] -> BigMatrix
    list table = BigMatrix vec rowLength colLength
        where vec = V.concat $ map V.fromList table
              rowLength = length table
              colLength = length $ head table

    _str :: V.Vector BigScalar -> Int -> Int -> String
    _str table rowsLeft totalCols = printf "%s\n%s" (show currentRow) (_str rest (rowsLeft - 1) totalCols)
        where currentRow = V.take totalCols table
              rest = V.drop totalCols table

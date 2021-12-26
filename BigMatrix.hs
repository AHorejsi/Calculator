module BigMatrix (
    BigMatrix,
    mlist,
    mvec,
    mrepeat,
    mrows,
    mcols,
    msize,
    mequalSize,
    mget,
    mplus,
    mminus,
    smultm,
    mmults,
    (==),
    (/=),
    show
) where
    import Prelude hiding (repeat)
    import Text.Printf
    import qualified Data.Vector as V
    import MathInfo
    import BigScalar
    import BigVector

    data BigMatrix = BigMatrix {
        _table :: V.Vector BigScalar,
        _rows :: Int,
        _cols :: Int
    } deriving (Eq)

    instance Show BigMatrix where
        show (BigMatrix table rows cols) = printf "Matrix(%s)\n" (_str table rows cols)

    _str :: V.Vector BigScalar -> Int -> Int -> String
    _str table rowsLeft totalCols = printf "[%s]\n[%s]" (show currentRow) (_str rest (rowsLeft - 1) totalCols)
        where currentRow = V.take totalCols table
              rest = V.drop totalCols table

    mlist :: [[BigScalar]] -> BigMatrix
    mlist table = BigMatrix vec rowLength colLength
        where vec = V.fromList $ concat table
              rowLength = length table
              colLength = length $ head table

    mvec :: V.Vector (V.Vector BigScalar) -> BigMatrix
    mvec table = BigMatrix concatVec rowLength colLength
        where concatVec = V.foldr (V.++) V.empty table
              rowLength = V.length table
              colLength = V.length $ V.head table

    mrepeat :: BigScalar -> Int -> Int -> BigMatrix
    mrepeat scalar rows cols = BigMatrix (V.replicate elemCount scalar) rows cols
        where elemCount = rows * cols

    mrows :: BigMatrix -> Int
    mrows (BigMatrix _ rows _) = rows

    mcols :: BigMatrix -> Int
    mcols (BigMatrix _ _ cols) = cols

    msize :: BigMatrix -> Int
    msize matrix = (mrows matrix) * (mcols matrix)

    mequalSize :: BigMatrix -> BigMatrix -> Bool
    mequalSize (BigMatrix _ leftRows leftCols) (BigMatrix _ rightRows rightCols) = (leftRows == rightRows) && (leftCols == rightCols)

    mget :: BigMatrix -> Int -> Int -> MathResult BigScalar
    mget matrix@(BigMatrix table rows cols) rowIndex colIndex
        | rowIndex < 0 || rowIndex >= rows || colIndex < 0 || colIndex >= cols = withError InvalidIndex
        | otherwise = withValue $ table V.! (rowIndex * cols + colIndex)

    mplus :: BigMatrix -> BigMatrix -> MathResult BigMatrix
    mplus = _binaryOperation splus

    mminus :: BigMatrix -> BigMatrix -> MathResult BigMatrix
    mminus = _binaryOperation sminus

    _binaryOperation :: BinaryScalarOperation -> BigMatrix -> BigMatrix -> MathResult BigMatrix
    _binaryOperation operation left@(BigMatrix leftTable leftRows leftCols) right@(BigMatrix rightTable _ _)
        | not $ mequalSize left right = withError UnequalLength
        | otherwise = withValue $ BigMatrix (V.zipWith operation leftTable rightTable) leftRows leftCols

    smultm :: BigScalar -> BigMatrix -> BigMatrix
    smultm left = _unaryOperation (smult left)

    mmults :: BigMatrix -> BigScalar -> BigMatrix
    mmults left right = _unaryOperation ((flip smult) right) left

    _unaryOperation :: UnaryScalarOperation -> BigMatrix -> BigMatrix
    _unaryOperation operation (BigMatrix table rows cols) = BigMatrix (V.map operation table) rows cols

    mtranspose :: BigMatrix -> BigMatrix
    mtranspose matrix@(BigMatrix _ rows cols) = BigMatrix (_mtransposeHelper matrix 0 0) rows cols

    _mtransposeHelper :: BigMatrix -> Int -> Int -> V.Vector BigScalar
    _mtransposeHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | rowIndex == lastRowIndex && colIndex == lastColIndex = V.singleton elem
        | otherwise = V.cons elem (_mtransposeHelper matrix nextRowIndex nextColIndex)
        where lastRowIndex = rows - 1
              lastColIndex = cols - 1
              elem = value $ mget matrix rowIndex colIndex
              nextRowIndex = if rowIndex == lastRowIndex then 0 else rowIndex + 1
              nextColIndex = if rowIndex == lastRowIndex then colIndex + 1 else colIndex
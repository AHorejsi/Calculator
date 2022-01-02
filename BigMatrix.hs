{-# LANGUAGE DeriveGeneric #-}

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
    asRowMatrix,
    asColumnMatrix,
    vmultm,
    mmultv,
    mmult,
    mtranspose,
    (==),
    (/=),
    show
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Vector as V
    import qualified Data.Hashable as H
    import qualified MathInfo as MI
    import qualified BigScalar as BS
    import qualified BigVector as BV

    data BigMatrix = BigMatrix {
        _table :: V.Vector BS.BigScalar,
        _rows :: Int,
        _cols :: Int
    } deriving (G.Generic, Eq)

    instance H.Hashable BigMatrix where
        hashWithSalt salt (BigMatrix table _ _) = H.hashWithSalt salt (V.toList table)

    instance Show BigMatrix where
        show (BigMatrix table rows cols) = TP.printf "Matrix(%s)" (_str table rows cols)

    _str :: V.Vector BS.BigScalar -> Int -> Int -> String
    _str table rowsLeft totalCols = case rowsLeft of 0 -> ""
                                                     1 -> TP.printf "[%s]" (show currentRow)
                                                     _ -> TP.printf "[%s], %s" (show currentRow) (_str rest (rowsLeft - 1) totalCols)
        where currentRow = V.take totalCols table
              rest = V.drop totalCols table

    mlist :: [[BS.BigScalar]] -> BigMatrix
    mlist table = BigMatrix vec rowLength colLength
        where vec = V.fromList $ concat table
              rowLength = length table
              colLength = length $ head table

    mvec :: V.Vector (V.Vector BS.BigScalar) -> BigMatrix
    mvec table = BigMatrix concatVec rowLength colLength
        where concatVec = V.foldr (V.++) V.empty table
              rowLength = V.length table
              colLength = V.length $ V.head table

    mrepeat :: BS.BigScalar -> Int -> Int -> BigMatrix
    mrepeat scalar rows cols = BigMatrix (V.replicate elemCount scalar) rows cols
        where elemCount = rows * cols

    mrows :: BigMatrix -> Int
    mrows (BigMatrix _ rows _) = rows

    mcols :: BigMatrix -> Int
    mcols (BigMatrix _ _ cols) = cols

    msize :: BigMatrix -> Int
    msize (BigMatrix _ rows cols) = rows * cols

    mequalSize :: BigMatrix -> BigMatrix -> Bool
    mequalSize (BigMatrix _ leftRows leftCols) (BigMatrix _ rightRows rightCols) = (leftRows == rightRows) && (leftCols == rightCols)

    mget :: BigMatrix -> Int -> Int -> MI.MathResult BS.BigScalar
    mget matrix@(BigMatrix table rows cols) rowIndex colIndex
        | rowIndex < 0 || rowIndex >= rows || colIndex < 0 || colIndex >= cols = MI.withError MI.InvalidIndex
        | otherwise = MI.withValue $ table V.! (rowIndex * cols + colIndex)

    mplus :: BigMatrix -> BigMatrix -> MI.MathResult BigMatrix
    mplus = _binaryOperation BS.splus

    mminus :: BigMatrix -> BigMatrix -> MI.MathResult BigMatrix
    mminus = _binaryOperation BS.sminus

    mscale :: BigMatrix -> BigMatrix -> MI.MathResult BigMatrix
    mscale = _binaryOperation BS.smult

    _binaryOperation :: BS.BinaryScalarOperation -> BigMatrix -> BigMatrix -> MI.MathResult BigMatrix
    _binaryOperation operation left@(BigMatrix leftTable leftRows leftCols) right@(BigMatrix rightTable _ _)
        | not $ mequalSize left right = MI.withError MI.UnequalLength
        | otherwise = MI.withValue $ BigMatrix (V.zipWith operation leftTable rightTable) leftRows leftCols

    smultm :: BS.BigScalar -> BigMatrix -> BigMatrix
    smultm left = _unaryOperation (BS.smult left)

    mmults :: BigMatrix -> BS.BigScalar -> BigMatrix
    mmults left right = _unaryOperation ((flip BS.smult) right) left

    _unaryOperation :: BS.UnaryScalarOperation -> BigMatrix -> BigMatrix
    _unaryOperation operation (BigMatrix table rows cols) = BigMatrix (V.map operation table) rows cols

    asRowMatrix :: BV.BigVector -> BigMatrix
    asRowMatrix vec = BigMatrix (BV.asVector vec) 1 (BV.vsize vec)

    asColumnMatrix :: BV.BigVector -> BigMatrix
    asColumnMatrix vec = BigMatrix (BV.asVector vec) (BV.vsize vec) 1

    vmultm :: BV.BigVector -> BigMatrix -> MI.MathResult BigMatrix
    vmultm left = mmult (asColumnMatrix left)

    mmultv :: BigMatrix -> BV.BigVector -> MI.MathResult BigMatrix
    mmultv left right = mmult left (asRowMatrix right)

    mmult :: BigMatrix -> BigMatrix -> MI.MathResult BigMatrix
    mmult left@(BigMatrix _ leftRows leftCols) right@(BigMatrix _ rightRows rightCols)
        | leftCols /= rightRows = MI.withError MI.NotMultipliableMatrices
        | otherwise = MI.withValue $ BigMatrix (_mmultHelper left right 0 0 leftRows rightCols) leftRows rightCols

    _mmultHelper :: BigMatrix -> BigMatrix -> Int -> Int -> Int -> Int -> V.Vector BS.BigScalar
    _mmultHelper left right rowIndex colIndex leftRows rightCols
        | nextRowIndex == leftRows = V.singleton elem
        | otherwise = V.cons elem (_mmultHelper left right nextRowIndex nextColIndex leftRows rightCols)
        where elem = _mmultTraverse 0 rowIndex colIndex leftRows left right
              nextRowIndex = if colIndex == rightCols - 1 then rowIndex + 1 else rowIndex
              nextColIndex = if colIndex == rightCols - 1 then 0 else colIndex + 1

    _mmultTraverse :: Int -> Int -> Int -> Int -> BigMatrix -> BigMatrix -> BS.BigScalar
    _mmultTraverse index rowIndex colIndex endIndex left right
        | index == endIndex = elem
        | otherwise = BS.splus elem next
        where elem = BS.smult (MI.value $ mget left rowIndex index) (MI.value $ mget right index colIndex)
              next = _mmultTraverse (index + 1) rowIndex colIndex endIndex left right

    mdivs :: BigMatrix -> BS.BigScalar -> MI.MathResult BigMatrix
    mdivs left right
        | BS.zero == right = MI.withError MI.DivideByZero
        | otherwise = MI.withValue $ mmults left rightInv
        where rightInv = MI.value $ BS.sinv right

    --mdiv :: BigMatrix -> BigMatrix -> BigMatrix
    --mdiv left right = mmult left (minv right)

    mneg :: BigMatrix -> BigMatrix
    mneg = smultm BS.negOne

    --mdet :: BigMatrix -> MI.MathResult BigMatrix
    --mminor :: BigMatrix -> MI.MathResult BigMatrix
    --mcofactors :: BigMatrix -> BigMatrix
    --minv :: BigMatrix -> MI.MathResult BigMatrix
    
    mtranspose :: BigMatrix -> BigMatrix
    mtranspose matrix@(BigMatrix _ rows cols) = BigMatrix (_mtransposeHelper matrix 0 0) cols rows

    _mtransposeHelper :: BigMatrix -> Int -> Int -> V.Vector BS.BigScalar
    _mtransposeHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | nextColIndex == cols = V.singleton elem
        | otherwise = V.cons elem next
        where nextRowIndex = if rowIndex == rows - 1 then 0 else rowIndex + 1
              nextColIndex = if rowIndex == rows - 1 then colIndex + 1 else colIndex
              elem = MI.value $ mget matrix rowIndex colIndex
              next = _mtransposeHelper matrix nextRowIndex nextColIndex

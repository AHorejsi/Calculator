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
    isMatrixMultipliable,
    isSquare,
    mget,
    mplus,
    mminus,
    smultm,
    mmults,
    mmult,
    mdivs,
    mdiv,
    mdet,
    minv,
    mtranspose,
    msub,
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

    data BigMatrix = BigMatrix {
        _table :: V.Vector BS.BigScalar,
        _rows :: Int,
        _cols :: Int
    } deriving (G.Generic, Eq)

    instance H.Hashable BigMatrix where
        hashWithSalt salt (BigMatrix table _ _) = H.hashWithSalt salt (V.toList table)

    instance Show BigMatrix where
        show (BigMatrix table rows cols) = TP.printf "Matrix([%s])" (_str table rows cols)

    _str :: V.Vector BS.BigScalar -> Int -> Int -> String
    _str table rowsLeft totalCols = case rowsLeft of 0 -> ""
                                                     1 -> TP.printf "%s" (show currentRow)
                                                     _ -> TP.printf "%s, %s" (show currentRow) (_str rest (rowsLeft - 1) totalCols)
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

    isMatrixMultipliable :: BigMatrix -> BigMatrix -> Bool
    isMatrixMultipliable (BigMatrix _ _ leftCols) (BigMatrix _ rightRows _) = leftCols == rightRows

    isSquare :: BigMatrix -> Bool
    isSquare (BigMatrix _ rows cols) = rows == cols

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

    mmult :: BigMatrix -> BigMatrix -> MI.MathResult BigMatrix
    mmult left@(BigMatrix _ leftRows leftCols) right@(BigMatrix _ rightRows rightCols)
        | isMatrixMultipliable left right = MI.withError MI.NotMultipliableMatrices
        | otherwise = MI.withValue $ BigMatrix (_mmultHelper left right 0 0 leftRows rightCols) leftRows rightCols

    _mmultHelper :: BigMatrix -> BigMatrix -> Int -> Int -> Int -> Int -> V.Vector BS.BigScalar
    _mmultHelper left right rowIndex colIndex leftRows rightCols
        | nextRowIndex == leftRows = V.singleton elem
        | otherwise = V.cons elem (_mmultHelper left right nextRowIndex nextColIndex leftRows rightCols)
        where elem = _mmultTraverse 0 rowIndex colIndex leftRows left right
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex rightCols

    _mmultTraverse :: Int -> Int -> Int -> Int -> BigMatrix -> BigMatrix -> BS.BigScalar
    _mmultTraverse index rowIndex colIndex endIndex left right
        | index == endIndex = elem
        | otherwise = BS.splus elem next
        where elem = BS.smult (MI.value $ mget left rowIndex index) (MI.value $ mget right index colIndex)
              next = _mmultTraverse (index + 1) rowIndex colIndex endIndex left right

    _rowTraversal :: Int -> Int -> Int -> (Int, Int)
    _rowTraversal rowIndex colIndex cols = (nextRowIndex, nextColIndex)
        where nextRowIndex = if colIndex == cols - 1 then rowIndex + 1 else rowIndex
              nextColIndex = if colIndex == cols - 1 then 0 else colIndex + 1

    mdivs :: BigMatrix -> BS.BigScalar -> MI.MathResult BigMatrix
    mdivs left right
        | BS.zero == right = MI.withError MI.DivideByZero
        | otherwise = MI.withValue $ _errableUnaryOperation ((flip BS.sdiv) right) left

    _errableUnaryOperation :: BS.ErrableUnaryScalarOperation -> BigMatrix -> BigMatrix
    _errableUnaryOperation operation (BigMatrix table rows cols) = BigMatrix (V.map (MI.value . operation) table) rows cols

    mdiv :: BigMatrix -> BigMatrix -> MI.MathResult BigMatrix
    mdiv left right
        | not $ isSquare right = MI.withError MI.NonsquareMatrix
        | not $ isMatrixMultipliable left right = MI.withError MI.NotMultipliableMatrices
        | otherwise = mmult left (MI.value $ minv right)

    mneg :: BigMatrix -> BigMatrix
    mneg = smultm BS.negOne

    mdet :: BigMatrix -> MI.MathResult BS.BigScalar
    mdet matrix@(BigMatrix _ rows cols)
        | not $ isSquare matrix = MI.withError MI.NonsquareMatrix
        | 1 == rows = mget matrix 0 0
        | otherwise = MI.withValue $ _mdetHelper1 matrix

    _mdetHelper1 :: BigMatrix -> BS.BigScalar
    _mdetHelper1 matrix@(BigMatrix _ rows _)
        | 2 == rows = BS.sminus (BS.smult val00 val11) (BS.smult val01 val10)
        | otherwise = _mdetHelper2 matrix 0
        where val00 = MI.value $ mget matrix 0 0
              val01 = MI.value $ mget matrix 0 1
              val10 = MI.value $ mget matrix 1 0
              val11 = MI.value $ mget matrix 1 1

    _mdetHelper2 :: BigMatrix -> Int -> BS.BigScalar
    _mdetHelper2 matrix@(BigMatrix _ _ cols) colIndex
        | colIndex == cols = BS.zero
        | otherwise = BS.splus newElem (_mdetHelper2 matrix (colIndex + 1))
        where a = MI.value $ BS.spow BS.negOne (BS.integral colIndex)
              b = MI.value $ mget matrix 0 colIndex
              c = _mdetHelper1 $ MI.value $ msub matrix 0 colIndex
              newElem = foldr BS.smult BS.one [a, b, c]

    _mminor :: BigMatrix -> MI.MathResult BigMatrix
    _mminor matrix@(BigMatrix _ rows cols)
        | not $ isSquare matrix = MI.withError MI.NonsquareMatrix
        | otherwise = MI.withValue $ BigMatrix (_mminorHelper matrix 0 0) rows cols

    _mminorHelper :: BigMatrix -> Int -> Int -> V.Vector BS.BigScalar
    _mminorHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | rowIndex == rows = V.empty
        | otherwise = V.cons elem next
        where elem = MI.value $ mdet $ MI.value $ msub matrix rowIndex colIndex
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex cols
              next = _mminorHelper matrix nextRowIndex nextColIndex
    
    _mcofactors :: BigMatrix -> BigMatrix
    _mcofactors matrix@(BigMatrix _ rows cols) = BigMatrix (_mcofactorsHelper matrix 0 0) rows cols

    _mcofactorsHelper :: BigMatrix -> Int -> Int -> V.Vector BS.BigScalar
    _mcofactorsHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | rowIndex == rows = V.empty
        | otherwise = V.cons elem next
        where val = MI.value $ mget matrix rowIndex colIndex
              elem = if (0 == mod rowIndex 2) /= (0 == mod colIndex 2) then BS.sneg val else val
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex cols
              next = _mcofactorsHelper matrix nextRowIndex nextColIndex
        
    minv :: BigMatrix -> MI.MathResult BigMatrix
    minv matrix@(BigMatrix _ rows cols)
        | not $ isSquare matrix = MI.withError MI.NonsquareMatrix
        | BS.zero == detValue = MI.withError MI.ZeroDeterminant
        | otherwise = MI.withValue $ case rows of 1 -> BigMatrix (V.singleton invOfDetValue) 1 1
                                                  2 -> smultm invOfDetValue (mlist [[val00, val01], [val10, val11]])
                                                  _ -> smultm invOfDetValue (mtranspose $ _mcofactors (MI.value $ _mminor matrix))
        where detValue = MI.value $ mdet matrix
              invOfDetValue = MI.value $ BS.sinv detValue
              val00 = MI.value $ mget matrix 1 1
              val01 = BS.sneg $ MI.value $ mget matrix 0 1
              val10 = BS.sneg $ MI.value $ mget matrix 1 0
              val11 = MI.value $ mget matrix 0 0
    
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

    msub :: BigMatrix -> Int -> Int -> MI.MathResult BigMatrix
    msub matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | rowIndex < 0 || rowIndex >= rows || colIndex < 0 || colIndex >= cols = MI.withError MI.InvalidIndex
        | otherwise = MI.withValue $ BigMatrix (_msubHelper matrix rowIndex colIndex 0) (rows - 1) (cols - 1)

    _msubHelper :: BigMatrix -> Int -> Int -> Int -> V.Vector BS.BigScalar
    _msubHelper matrix@(BigMatrix _ _ cols) avoidRowIndex avoidColIndex index
        | index == (msize matrix) = V.empty
        | rowIndex == avoidRowIndex || colIndex == avoidColIndex = next
        | otherwise = V.cons elem next
        where rowIndex = div index cols
              colIndex = mod index cols
              elem = MI.value $ mget matrix rowIndex colIndex
              next = _msubHelper matrix avoidRowIndex avoidColIndex (index + 1)

{-# LANGUAGE DeriveGeneric #-}

module BigMatrix (
    BigMatrix,
    mlist,
    mseq,
    mrepeat,
    mrows,
    mcols,
    msize,
    mrowLength,
    mcolLength,
    mlength,
    mequalSize,
    isMatrixMultipliable,
    mIsSquare,
    mget,
    mplus,
    mminus,
    mscale,
    smultm,
    mmults,
    vmultm,
    mmultv,
    mRowVector,
    mColVector,
    mmult,
    mdivs,
    mdiv,
    mneg,
    mdet,
    minv,
    mtranspose,
    msub,
    (==),
    (/=),
    H.hash,
    H.hashWithSalt,
    show
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Sequence as S
    import qualified Data.Foldable as F
    import qualified Data.Hashable as H
    import qualified MathInfo as MI
    import qualified BigScalar as BS
    import qualified BigVector as BV

    data BigMatrix = BigMatrix {
        _table :: S.Seq BS.BigScalar,
        _rows :: Int,
        _cols :: Int
    } deriving (G.Generic, Eq)

    instance H.Hashable BigMatrix where
        hashWithSalt salt (BigMatrix table _ _) = H.hashWithSalt salt (F.toList table)

    instance Show BigMatrix where
        show (BigMatrix table rows cols) = TP.printf "Matrix([%s])" (_str table rows cols)

    _str :: S.Seq BS.BigScalar -> Int -> Int -> String
    _str table rowsLeft totalCols = case rowsLeft of 0 -> ""
                                                     1 -> TP.printf "%s" (show $ F.toList currentRow)
                                                     _ -> TP.printf "%s, %s" (show $ F.toList currentRow) (_str rest (rowsLeft - 1) totalCols)
        where currentRow = S.take totalCols table
              rest = S.drop totalCols table

    mlist :: [[BS.BigScalar]] -> BigMatrix
    mlist table = BigMatrix vec rowLength colLength
        where vec = S.fromList $ concat table
              rowLength = length table
              colLength = length $ head table

    mseq :: S.Seq (S.Seq BS.BigScalar) -> BigMatrix
    mseq table = BigMatrix concatVec rowLength colLength
        where concatVec = foldr (S.><) S.empty table
              rowLength = length table
              colLength = length $ S.index table 0

    mrepeat :: BS.BigScalar -> Int -> Int -> BigMatrix
    mrepeat scalar rows cols = BigMatrix (S.replicate elemCount scalar) rows cols
        where elemCount = rows * cols

    mrows :: BigMatrix -> BS.BigScalar
    mrows = BS.integral . mrowLength

    mcols :: BigMatrix -> BS.BigScalar
    mcols = BS.integral . mcolLength

    msize :: BigMatrix -> BS.BigScalar
    msize = BS.integral . mlength

    mrowLength :: BigMatrix -> Int
    mrowLength (BigMatrix _ rows _) = rows

    mcolLength :: BigMatrix -> Int
    mcolLength (BigMatrix _ _ cols) = cols

    mlength :: BigMatrix -> Int
    mlength (BigMatrix _ rows cols) = rows * cols

    mequalSize :: BigMatrix -> BigMatrix -> Bool
    mequalSize (BigMatrix _ leftRows leftCols) (BigMatrix _ rightRows rightCols) = (leftRows == rightRows) && (leftCols == rightCols)

    isMatrixMultipliable :: BigMatrix -> BigMatrix -> Bool
    isMatrixMultipliable (BigMatrix _ _ leftCols) (BigMatrix _ rightRows _) = leftCols == rightRows

    mIsSquare :: BigMatrix -> Bool
    mIsSquare (BigMatrix _ rows cols) = rows == cols

    mget :: BigMatrix -> Int -> Int -> MI.Result BS.BigScalar
    mget matrix@(BigMatrix table rows cols) rowIndex colIndex
        | rowIndex < 0 || rowIndex >= rows || colIndex < 0 || colIndex >= cols = MI.withError MI.InvalidIndex
        | otherwise = MI.withValue $ S.index table (rowIndex * cols + colIndex)

    mplus :: BigMatrix -> BigMatrix -> MI.Result BigMatrix
    mplus = _binaryOperation BS.splus

    mminus :: BigMatrix -> BigMatrix -> MI.Result BigMatrix
    mminus = _binaryOperation BS.sminus

    mscale :: BigMatrix -> BigMatrix -> MI.Result BigMatrix
    mscale = _binaryOperation BS.smult

    _binaryOperation :: BS.BinaryScalarOperation -> BigMatrix -> BigMatrix -> MI.Result BigMatrix
    _binaryOperation operation left@(BigMatrix leftTable leftRows leftCols) right@(BigMatrix rightTable _ _)
        | not $ mequalSize left right = MI.withError MI.UnequalLength
        | otherwise = MI.withValue $ BigMatrix (S.zipWith operation leftTable rightTable) leftRows leftCols

    smultm :: BS.BigScalar -> BigMatrix -> BigMatrix
    smultm left = _unaryOperation (BS.smult left)

    mmults :: BigMatrix -> BS.BigScalar -> BigMatrix
    mmults left right = _unaryOperation ((flip BS.smult) right) left

    vmultm :: BV.BigVector -> BigMatrix -> MI.Result BigMatrix
    vmultm left = mmult (mRowVector left)

    mmultv :: BigMatrix -> BV.BigVector -> MI.Result BigMatrix
    mmultv left right = mmult left (mColVector right)

    mRowVector :: BV.BigVector -> BigMatrix
    mRowVector vec = BigMatrix list 1 dimensions
        where dimensions = BV.vlength vec
              list = BV.asSeq vec
    
    mColVector :: BV.BigVector -> BigMatrix
    mColVector vec = BigMatrix list dimensions 1
        where dimensions = BV.vlength vec
              list = BV.asSeq vec

    _unaryOperation :: BS.UnaryScalarOperation -> BigMatrix -> BigMatrix
    _unaryOperation operation (BigMatrix table rows cols) = BigMatrix (fmap operation table) rows cols

    mmult :: BigMatrix -> BigMatrix -> MI.Result BigMatrix
    mmult left@(BigMatrix _ leftRows leftCols) right@(BigMatrix _ rightRows rightCols)
        | isMatrixMultipliable left right = MI.withError MI.NotMultipliableMatrices
        | otherwise = MI.withValue $ BigMatrix (_mmultHelper left right 0 0 leftRows rightCols) leftRows rightCols

    _mmultHelper :: BigMatrix -> BigMatrix -> Int -> Int -> Int -> Int -> S.Seq BS.BigScalar
    _mmultHelper left right rowIndex colIndex leftRows rightCols
        | nextRowIndex == leftRows = S.singleton elem
        | otherwise = elem S.<| next
        where elem = _mmultTraverse 0 rowIndex colIndex leftRows left right
              next = _mmultHelper left right nextRowIndex nextColIndex leftRows rightCols
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

    mdivs :: BigMatrix -> BS.BigScalar -> MI.Result BigMatrix
    mdivs left right
        | BS.zero == right = MI.withError MI.DivideByZero
        | otherwise = MI.withValue $ _errableUnaryOperation ((flip BS.sdiv) right) left

    _errableUnaryOperation :: BS.ErrableUnaryScalarOperation -> BigMatrix -> BigMatrix
    _errableUnaryOperation operation (BigMatrix table rows cols) = BigMatrix (fmap (MI.value . operation) table) rows cols

    mdiv :: BigMatrix -> BigMatrix -> MI.Result BigMatrix
    mdiv left right
        | not $ mIsSquare right = MI.withError MI.NonsquareMatrix
        | not $ isMatrixMultipliable left right = MI.withError MI.NotMultipliableMatrices
        | otherwise = mmult left (MI.value $ minv right)

    mneg :: BigMatrix -> BigMatrix
    mneg = smultm BS.negOne

    mdet :: BigMatrix -> MI.Result BS.BigScalar
    mdet matrix@(BigMatrix _ rows cols)
        | not $ mIsSquare matrix = MI.withError MI.NonsquareMatrix
        | 1 == rows = mget matrix 0 0
        | otherwise = MI.withValue $ _mdetHelper1 matrix

    _mdetHelper1 :: BigMatrix -> BS.BigScalar
    _mdetHelper1 matrix@(BigMatrix _ rows _)
        | 2 == rows = BS.sminus (BS.smult a d) (BS.smult b c)
        | otherwise = _mdetHelper2 matrix 0
        where a = MI.value $ mget matrix 0 0
              b = MI.value $ mget matrix 0 1
              c = MI.value $ mget matrix 1 0
              d = MI.value $ mget matrix 1 1

    _mdetHelper2 :: BigMatrix -> Int -> BS.BigScalar
    _mdetHelper2 matrix@(BigMatrix _ _ cols) colIndex
        | colIndex == cols = BS.zero
        | otherwise = BS.splus newElem (_mdetHelper2 matrix (colIndex + 1))
        where a = MI.value $ BS.spow BS.negOne (BS.integral colIndex)
              b = MI.value $ mget matrix 0 colIndex
              c = _mdetHelper1 $ MI.value $ _msubHelper1 matrix 0 colIndex
              newElem = foldr BS.smult BS.one [a, b, c]

    _mminor :: BigMatrix -> MI.Result BigMatrix
    _mminor matrix@(BigMatrix _ rows cols)
        | not $ mIsSquare matrix = MI.withError MI.NonsquareMatrix
        | otherwise = MI.withValue $ BigMatrix (_mminorHelper matrix 0 0) rows cols

    _mminorHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _mminorHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | rowIndex == rows = S.empty
        | otherwise = elem S.<| next
        where elem = MI.value $ mdet $ MI.value $ _msubHelper1 matrix rowIndex colIndex
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex cols
              next = _mminorHelper matrix nextRowIndex nextColIndex
    
    _mcofactors :: BigMatrix -> BigMatrix
    _mcofactors matrix@(BigMatrix table rows cols) = BigMatrix (_mcofactorsHelper table True) rows cols

    _mcofactorsHelper :: S.Seq BS.BigScalar -> Bool -> S.Seq BS.BigScalar
    _mcofactorsHelper table sign
        | S.null table = S.empty
        | otherwise = elem S.<| next
        where val = S.index table 0
              elem = if sign then BS.sneg val else val
              rest = S.drop 1 table
              next = _mcofactorsHelper rest (not sign)
        
    minv :: BigMatrix -> MI.Result BigMatrix
    minv matrix@(BigMatrix _ rows cols)
        | not $ mIsSquare matrix = MI.withError MI.NonsquareMatrix
        | BS.zero == detValue = MI.withError MI.ZeroDeterminant
        | otherwise = MI.withValue $ case rows of 1 -> BigMatrix (S.singleton invOfDetValue) 1 1
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

    _mtransposeHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _mtransposeHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | nextColIndex == cols = S.singleton elem
        | otherwise = elem S.<| next
        where lastRowIndex = rows - 1
              nextRowIndex = if rowIndex == lastRowIndex then 0 else rowIndex + 1
              nextColIndex = if rowIndex == lastRowIndex then colIndex + 1 else colIndex
              elem = MI.value $ mget matrix rowIndex colIndex
              next = _mtransposeHelper matrix nextRowIndex nextColIndex

    msub :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.Result BigMatrix
    msub matrix avoidRowIndex avoidColIndex = _msubHelper1 matrix avoidRowIndexInt avoidColIndexInt
        where avoidRowIndexInt = BS.asBuiltInInt avoidRowIndex
              avoidColIndexInt = BS.asBuiltInInt avoidColIndex

    _msubHelper1 :: BigMatrix -> Int -> Int -> MI.Result BigMatrix
    _msubHelper1 (BigMatrix table rows cols) avoidRowIndex avoidColIndex
        | avoidRowIndex < 0 || avoidRowIndex >= rows || avoidColIndex < 0 || avoidColIndex >= cols = MI.withError MI.InvalidIndex
        | otherwise = MI.withValue $ BigMatrix newTable (rows - 1) (cols - 1)
        where newTable = _msubHelper2 table rows cols avoidRowIndex avoidColIndex 0

    _msubHelper2 :: S.Seq BS.BigScalar -> Int -> Int -> Int -> Int -> Int -> S.Seq BS.BigScalar
    _msubHelper2 table rows cols avoidRowIndex avoidColIndex rowIndex
        | rowIndex == rows = S.empty
        | rowIndex == avoidRowIndex = nextRows
        | otherwise = currentRow S.>< nextRows
        where (row, restRows) = S.splitAt cols table
              startElems = S.take avoidColIndex row
              endElems = S.drop (avoidColIndex + 1) row
              currentRow = startElems S.>< endElems
              nextRows = _msubHelper2 restRows rows cols avoidRowIndex avoidColIndex (rowIndex + 1)

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module BigMatrix (
    BigMatrix,
    fromList,
    fromSeq,
    identity,
    rows,
    cols,
    rowLength,
    colLength,
    equalSize,
    isMatrixMultipliable,
    get2,
    getInt2,
    isSquare,
    leftVectorMult,
    rightVectorMult,
    rowVector,
    colVector,
    det,
    transpose,
    sub2,
    addRow,
    multRow,
    swapRows,
    addCol,
    multCol,
    swapCols
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Sequence as S
    import qualified Data.Foldable as F
    import qualified Data.Hashable as H
    import qualified Actions as A
    import qualified Stringify as Str
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
        show matrix = _str "Matrix[%s]" matrix show

    instance Str.Stringifier BigMatrix where
        stringify sets matrix = _str "[%s]" matrix (Str.stringify sets)

    _str :: String -> BigMatrix -> MI.UnaryAction BS.BigScalar String -> String
    _str format (BigMatrix table rows cols) converter = TP.printf format strVal
        where strVal = _strHelper table converter rows cols

    _strHelper :: S.Seq BS.BigScalar -> MI.UnaryAction BS.BigScalar String -> Int -> Int -> String
    _strHelper table converter rowsLeft totalCols = case rowsLeft of 0 -> ""
                                                                     1 -> show finalString
                                                                     _ -> TP.printf "%s|%s" finalString next
        where (currentRow, rest) = S.splitAt totalCols table
              stringList = fmap converter currentRow
              commaSeparated = S.intersperse "," stringList
              finalString = concat commaSeparated
              next = _strHelper rest converter (rowsLeft - 1) totalCols

    fromList :: [[BS.BigScalar]] -> BigMatrix
    fromList table = BigMatrix seq rowLength colLength
        where seq = S.fromList $ concat table
              rowLength = length table
              colLength = length $ head table

    fromSeq :: S.Seq (S.Seq BS.BigScalar) -> BigMatrix
    fromSeq table = BigMatrix concatSeq rowLength colLength
        where concatSeq = F.foldl' (S.><) S.empty table
              rowLength = length table
              colLength = length $ S.index table 0

    identity :: BS.BigScalar -> MI.ComputationResult BigMatrix
    identity dimensions
        | not $ BS.isExactInteger dimensions = MI.withError MI.InvalidValue
        | intDimensions < 0 = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix table intDimensions intDimensions
        where intDimensions = BS.asInt dimensions
              table = _makeIdentityMatrix 0 0 intDimensions

    _makeIdentityMatrix :: Int -> Int -> Int -> S.Seq BS.BigScalar
    _makeIdentityMatrix currentRow currentCol dimensions
        | currentRow == dimensions = S.empty
        | otherwise = elem S.<| next
        where elem = if currentRow == currentCol then BS.one else BS.zero
              (nextRowIndex, nextColIndex) = _rowTraversal currentRow currentCol dimensions
              next = _makeIdentityMatrix nextRowIndex nextColIndex dimensions

    rows :: BigMatrix -> BS.BigScalar
    rows = BS.integral . rowLength

    cols :: BigMatrix -> BS.BigScalar
    cols = BS.integral . colLength

    rowLength :: BigMatrix -> Int
    rowLength (BigMatrix _ rows _) = rows

    colLength :: BigMatrix -> Int
    colLength (BigMatrix _ _ cols) = cols

    equalSize :: BigMatrix -> BigMatrix -> Bool
    equalSize (BigMatrix _ leftRows leftCols) (BigMatrix _ rightRows rightCols) = (leftRows == rightRows) && (leftCols == rightCols)

    isMatrixMultipliable :: BigMatrix -> BigMatrix -> Bool
    isMatrixMultipliable (BigMatrix _ _ leftCols) (BigMatrix _ rightRows _) = leftCols == rightRows

    isSquare :: BigMatrix -> Bool
    isSquare (BigMatrix _ rows cols) = rows == cols

    get2 :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BS.BigScalar
    get2 matrix rowIndex colIndex
        | not $ (BS.isExactInteger rowIndex) && (BS.isExactInteger colIndex) = MI.withError MI.InvalidType
        | otherwise = getInt2 matrix intRowIndex intColIndex
        where intRowIndex = BS.asInt rowIndex
              intColIndex = BS.asInt colIndex

    getInt2 :: BigMatrix -> Int -> Int -> MI.ComputationResult BS.BigScalar
    getInt2 matrix@(BigMatrix table rows cols) rowIndex colIndex
        | (rowIndex < 0) || (rowIndex >= rows) || (colIndex < 0) || (colIndex >= cols) = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ S.index table actualIndex
        where actualIndex = rowIndex * cols + colIndex

    instance A.Addable BigMatrix where
        plus = _binaryAction A.unsafePlus

    instance A.Subtractable BigMatrix where
        minus = _binaryAction A.unsafeMinus

    instance A.Scalable BigMatrix where
        scale = _binaryAction A.unsafeMult

    _binaryAction :: BS.BinaryScalarAction -> BigMatrix -> BigMatrix -> MI.ComputationResult BigMatrix
    _binaryAction operation left@(BigMatrix leftTable leftRows leftCols) right@(BigMatrix rightTable _ _)
        | not $ equalSize left right = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix (S.zipWith operation leftTable rightTable) leftRows leftCols

    instance BS.ScalarMultipliable BigMatrix where
        rightScalarMult left right = MI.withValue $ BS.unsafeRightScalarMult left right
        leftScalarMult left right = MI.withValue $ BS.unsafeLeftScalarMult left right
        unsafeRightScalarMult left (BigMatrix rightTable rows cols) = BigMatrix newTable rows cols
            where newTable = fmap (A.unsafeMult left) rightTable
        unsafeLeftScalarMult (BigMatrix leftTable rows cols) right = BigMatrix newTable rows cols
            where newTable = fmap (`A.unsafeMult` right) leftTable

    rightVectorMult :: BV.BigVector -> BigMatrix -> MI.ComputationResult BigMatrix
    rightVectorMult left = A.mult (rowVector left)

    leftVectorMult :: BigMatrix -> BV.BigVector -> MI.ComputationResult BigMatrix
    leftVectorMult left right = A.mult left (colVector right)

    rowVector :: BV.BigVector -> BigMatrix
    rowVector vec = BigMatrix list 1 dimensions
        where dimensions = BV.length1 vec
              list = BV.toSeq vec
    
    colVector :: BV.BigVector -> BigMatrix
    colVector vec = BigMatrix list dimensions 1
        where dimensions = BV.length1 vec
              list = BV.toSeq vec

    _unaryAction :: BS.UnaryScalarAction -> BigMatrix -> BigMatrix
    _unaryAction operation (BigMatrix table rows cols) = BigMatrix (fmap operation table) rows cols

    instance A.Multipliable BigMatrix where
        mult left@(BigMatrix _ leftRows leftCols) right@(BigMatrix _ rightRows rightCols)
            | not $ isMatrixMultipliable left right = MI.withError MI.InvalidValue
            | otherwise = MI.withValue $ BigMatrix (_mmultHelper1 left right 0 0 leftRows rightCols) leftRows rightCols

    _mmultHelper1 :: BigMatrix -> BigMatrix -> Int -> Int -> Int -> Int -> S.Seq BS.BigScalar
    _mmultHelper1 left right rowIndex colIndex leftRows rightCols
        | nextRowIndex == leftRows = S.singleton elem
        | otherwise = elem S.<| next
        where elem = _mmultHelper2 0 leftRows rowIndex colIndex left right
              next = _mmultHelper1 left right nextRowIndex nextColIndex leftRows rightCols
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex rightCols

    _mmultHelper2 :: Int -> Int -> Int -> Int -> BigMatrix -> BigMatrix -> BS.BigScalar
    _mmultHelper2 index endIndex rowIndex colIndex left right
        | index == endIndex = BS.zero
        | otherwise = A.unsafePlus elem next
        where elem = A.unsafeMult leftElem rightElem
              next = _mmultHelper2 (index + 1) endIndex rowIndex colIndex left right
              leftElem = MI.value $ getInt2 left rowIndex index
              rightElem = MI.value $ getInt2 right index colIndex

    _rowTraversal :: Int -> Int -> Int -> (Int, Int)
    _rowTraversal rowIndex colIndex cols = (nextRowIndex, nextColIndex)
        where lastColIndex = cols - 1
              nextRowIndex = if colIndex == lastColIndex then rowIndex + 1 else rowIndex
              nextColIndex = if colIndex == lastColIndex then 0 else colIndex + 1

    instance A.Divisible BigMatrix where
        div left right = MI.errBinResolveRight left invResult A.mult
            where invResult = A.inv right
        inv matrix@(BigMatrix _ rows cols)
            | not $ isSquare matrix = MI.withError MI.InvalidState
            | BS.zero == detValue = MI.withError MI.InvalidValue
            | otherwise = MI.withValue $ case rows of 1 -> BigMatrix (S.singleton invOfDetValue) 1 1
                                                      2 -> let val00 = MI.value $ getInt2 matrix 1 1
                                                               val01 = (A.unsafeNeg . MI.value) $ getInt2 matrix 0 1
                                                               val10 = (A.unsafeNeg . MI.value) $ getInt2 matrix 1 0
                                                               val11 = MI.value $ getInt2 matrix 0 0 
                                                           in BS.unsafeRightScalarMult invOfDetValue (fromList [[val00, val01], [val10, val11]])
                                                      _ -> BS.unsafeRightScalarMult invOfDetValue ((transpose . _cofactors) $ _minor matrix)
            where detValue = MI.value $ det matrix
                  invOfDetValue = A.unsafeInv detValue

    instance A.Negatable BigMatrix where
        unsafeNeg = BS.unsafeRightScalarMult BS.negOne
        neg = MI.withValue . A.unsafeNeg

    det :: BigMatrix -> MI.ComputationResult BS.BigScalar
    det matrix@(BigMatrix _ rows cols)
        | not $ isSquare matrix = MI.withError MI.InvalidState
        | 1 == rows = (A.inv . MI.value) $ getInt2 matrix 0 0
        | otherwise = MI.withValue $ _detHelper1 matrix

    _detHelper1 :: BigMatrix -> BS.BigScalar
    _detHelper1 matrix@(BigMatrix _ rows _)
        | 2 == rows = A.unsafeMinus (A.unsafeMult a d) (A.unsafeMult b c)
        | otherwise = _detHelper2 matrix 0
        where a = MI.value $ getInt2 matrix 0 0
              b = MI.value $ getInt2 matrix 0 1
              c = MI.value $ getInt2 matrix 1 0
              d = MI.value $ getInt2 matrix 1 1

    _detHelper2 :: BigMatrix -> Int -> BS.BigScalar
    _detHelper2 matrix@(BigMatrix _ _ cols) colIndex
        | colIndex == cols = BS.zero
        | otherwise = A.unsafePlus newElem next
        where a = A.unsafePow BS.negOne (BS.integral colIndex)
              b = MI.value $ getInt2 matrix 0 colIndex
              c = (_detHelper1 . MI.value) $ _msubHelper1 matrix 0 colIndex
              newElem = F.foldr A.unsafeMult BS.one [c, b, a]
              next = _detHelper2 matrix (colIndex + 1)

    _minor :: BigMatrix -> BigMatrix
    _minor matrix@(BigMatrix _ rows cols) = BigMatrix newTable rows cols
        where newTable = _minorHelper matrix 0 0

    _minorHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _minorHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | rowIndex == rows = S.empty
        | otherwise = elem S.<| next
        where elem = MI.value $ det $ MI.value $ _msubHelper1 matrix rowIndex colIndex
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex cols
              next = _minorHelper matrix nextRowIndex nextColIndex
    
    _cofactors :: BigMatrix -> BigMatrix
    _cofactors matrix@(BigMatrix table rows cols) = BigMatrix newTable rows cols
        where newTable = _cofactorsHelper table True

    _cofactorsHelper :: S.Seq BS.BigScalar -> Bool -> S.Seq BS.BigScalar
    _cofactorsHelper table sign
        | S.null table = S.empty
        | otherwise = elem S.<| next
        where val = S.index table 0
              elem = if sign then A.unsafeNeg val else val
              rest = S.drop 1 table
              next = _cofactorsHelper rest (not sign)
    
    transpose :: BigMatrix -> BigMatrix
    transpose matrix@(BigMatrix _ rows cols) = BigMatrix (_transposeHelper matrix 0 0) cols rows

    _transposeHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _transposeHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | nextColIndex == cols = S.singleton elem
        | otherwise = elem S.<| next
        where lastRowIndex = rows - 1
              nextRowIndex = if rowIndex == lastRowIndex then 0 else rowIndex + 1
              nextColIndex = if rowIndex == lastRowIndex then colIndex + 1 else colIndex
              elem = MI.value $ getInt2 matrix rowIndex colIndex
              next = _transposeHelper matrix nextRowIndex nextColIndex

    sub2 :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    sub2 matrix avoidRowIndex avoidColIndex = _msubHelper1 matrix avoidRowIndexInt avoidColIndexInt
        where avoidRowIndexInt = BS.asInt avoidRowIndex
              avoidColIndexInt = BS.asInt avoidColIndex

    _msubHelper1 :: BigMatrix -> Int -> Int -> MI.ComputationResult BigMatrix
    _msubHelper1 (BigMatrix table rows cols) avoidRowIndex avoidColIndex
        | avoidRowIndex < 0 || avoidRowIndex >= rows || avoidColIndex < 0 || avoidColIndex >= cols = MI.withError MI.InvalidValue
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

    addRow :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    addRow matrix@(BigMatrix _ rows cols) fromRowIndex toRowIndex
        | not $ (BS.isExactInteger fromRowIndex) && (BS.isExactInteger toRowIndex) = MI.withError MI.InvalidValue
        | intFromRowIndex < 0 || intFromRowIndex >= rows || intToRowIndex < 0 || intToRowIndex >= rows = MI.withError MI.InvalidValue
        | intFromRowIndex == intToRowIndex = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newSeq rows cols
        where intFromRowIndex = BS.asInt fromRowIndex
              intToRowIndex = BS.asInt toRowIndex
              newSeq = _addRowHelper matrix intFromRowIndex intToRowIndex

    _addRowHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _addRowHelper (BigMatrix table rows cols) fromRowIndex toRowIndex = startElems S.>< newElems S.>< endElems
        where actualFromIndex = rows * fromRowIndex
              actualToIndex = rows * toRowIndex
              fromRow = S.take cols (S.drop actualFromIndex table)
              toRow = S.take cols (S.drop actualToIndex table)
              startElems = S.take actualToIndex table
              newElems = S.zipWith A.unsafePlus fromRow toRow
              endElems = S.drop (actualToIndex + cols) table

    multRow :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    multRow matrix@(BigMatrix _ rows cols) multValue rowIndex
        | not $ BS.isExactInteger rowIndex = MI.withError MI.InvalidValue
        | intRowIndex < 0 || intRowIndex >= rows = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intRowIndex = BS.asInt rowIndex
              newTable = _multRowHelper matrix multValue intRowIndex

    _multRowHelper :: BigMatrix -> BS.BigScalar -> Int -> S.Seq BS.BigScalar
    _multRowHelper (BigMatrix table rows cols) multValue rowIndex = startElems S.>< newElems S.>< endElems
        where actualIndex = rows * rowIndex
              multRow = S.take cols (S.drop actualIndex table)
              startElems = S.take actualIndex table
              newElems = fmap (A.unsafeMult multValue) multRow
              endElems = S.drop (actualIndex + cols) table

    swapRows :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    swapRows matrix@(BigMatrix _ rows cols) rowIndex1 rowIndex2
        | not $ (BS.isExactInteger rowIndex1) && (BS.isExactInteger rowIndex2) = MI.withError MI.InvalidValue
        | (intRowIndex1 < 0) || (intRowIndex1 >= rows) || (intRowIndex2 < 0) || (intRowIndex2 >= rows) = MI.withError MI.InvalidValue
        | intRowIndex1 == intRowIndex2 = MI.withValue matrix
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intRowIndex1 = BS.asInt rowIndex1
              intRowIndex2 = BS.asInt rowIndex2
              newTable = _swapRowsHelper matrix intRowIndex1 intRowIndex2
    
    _swapRowsHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _swapRowsHelper (BigMatrix table rows cols) rowIndex1 rowIndex2 = startElems S.>< higherElems S.>< midElems S.>< lowerElems S.>< endElems
        where actualRowIndex1 = rows * rowIndex1
              actualRowIndex2 = rows * rowIndex2
              lowerRowIndex = min actualRowIndex1 actualRowIndex2
              higherRowIndex = max actualRowIndex1 actualRowIndex2
              startElems = S.take lowerRowIndex table
              lowerElems = S.take cols (S.drop lowerRowIndex table)
              midElems = S.take (higherRowIndex - lowerRowIndex - cols) (S.drop (lowerRowIndex + cols) table)
              higherElems = S.take cols (S.drop higherRowIndex table)
              endElems = S.drop (higherRowIndex + cols) table

    addCol :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    addCol (BigMatrix table rows cols) fromColIndex toColIndex
        | not $ (BS.isExactInteger fromColIndex) && (BS.isExactInteger toColIndex) = MI.withError MI.InvalidValue
        | (intFromColIndex < 0) || (intFromColIndex >= cols) || (intToColIndex < 0) || (intToColIndex >= cols) = MI.withError MI.InvalidValue
        | intFromColIndex == intToColIndex = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intFromColIndex = BS.asInt fromColIndex
              intToColIndex = BS.asInt toColIndex
              newTable = _addColHelper table rows cols intFromColIndex intToColIndex 0

    _addColHelper :: S.Seq BS.BigScalar -> Int -> Int -> Int -> Int -> Int -> S.Seq BS.BigScalar
    _addColHelper table rows cols fromColIndex toColIndex currentRowIndex
        | rows == currentRowIndex = S.empty
        | otherwise = newRow S.>< nextRows
        where (row, restRows) = S.splitAt cols table
              fromElem = S.index row fromColIndex
              toElem = S.index row toColIndex
              newElem = S.singleton $ A.unsafePlus fromElem toElem
              startRow = S.take toColIndex row
              endRow = S.drop (toColIndex + 1) row
              newRow = startRow S.>< newElem S.>< endRow
              nextRows = _addColHelper restRows rows cols fromColIndex toColIndex (currentRowIndex + 1)

    multCol :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    multCol (BigMatrix table rows cols) multValue colIndex
        | not $ BS.isExactInteger colIndex = MI.withError MI.InvalidValue
        | intColIndex < 0 || intColIndex >= cols = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intColIndex = BS.asInt colIndex
              newTable = _multColHelper table rows cols multValue intColIndex 0

    _multColHelper :: S.Seq BS.BigScalar -> Int -> Int -> BS.BigScalar -> Int -> Int -> S.Seq BS.BigScalar
    _multColHelper table rows cols multValue colIndex currentRowIndex
        | rows == currentRowIndex = S.empty
        | otherwise = newRow S.>< nextRows
        where (row, restRows) = S.splitAt cols table
              colElem = S.index row colIndex
              newElem = S.singleton $ A.unsafeMult multValue colElem
              startRow = S.take colIndex row
              endRow = S.drop (colIndex + 1) row
              newRow = startRow S.>< newElem S.>< endRow
              nextRows = _multColHelper restRows rows cols multValue colIndex (currentRowIndex + 1)

    swapCols :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    swapCols (BigMatrix table rows cols) colIndex1 colIndex2
        | not $ (BS.isExactInteger colIndex1) && (BS.isExactInteger colIndex2) = MI.withError MI.InvalidValue
        | intColIndex1 < 0 || intColIndex1 >= cols || intColIndex2 < 0 || intColIndex2 >= cols = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intColIndex1 = BS.asInt colIndex1
              intColIndex2 = BS.asInt colIndex2
              newTable = _swapColsHelper table rows cols intColIndex1 intColIndex2 0

    _swapColsHelper :: S.Seq BS.BigScalar -> Int -> Int -> Int -> Int -> Int -> S.Seq BS.BigScalar
    _swapColsHelper table rows cols colIndex1 colIndex2 currentRowIndex
        | rows == currentRowIndex = S.empty
        | otherwise = newRow S.>< nextRows
        where (row, restRows) = S.splitAt cols table
              minColIndex = min colIndex1 colIndex2
              maxColIndex = max colIndex1 colIndex2
              minElem = S.singleton $ S.index row minColIndex
              maxElem = S.singleton $ S.index row maxColIndex
              startRow = S.take minColIndex row
              midRow = S.take (maxColIndex - minColIndex - 1) (S.drop (minColIndex + 1) row)
              endRow = S.drop (maxColIndex + 1) row
              newRow = startRow S.>< maxElem S.>< midRow S.>< minElem S.>< endRow
              nextRows = _swapColsHelper restRows rows cols colIndex1 colIndex2 (currentRowIndex + 1)

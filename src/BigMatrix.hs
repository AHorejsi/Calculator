{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module BigMatrix (
    BigMatrix,
    matrix,
    rowsInt,
    rows,
    columnsInt,
    columns,
    equalSize,
    isMatrixMultipliable,
    square,
    null,
    get,
    getInt,
    scalarMultiplyLeft,
    scalarMultiplyRight,
    negate,
    matrixMultiply,
    determinant,
    submatrix,
    transpose,
    matrixInverse,
    matrixDivide,
    addRow,
    multRow,
    swapRows,
    addCol,
    multCol,
    swapCols,
    toContainer,
    to2dContainer
) where
    import Prelude hiding (negate, null)
    import qualified GHC.Generics as G
    import qualified Data.Either as E
    import qualified Data.Foldable as Fo
    import qualified Data.Functor as Fu
    import qualified Data.Hashable as H
    import qualified Indexable as I
    import qualified Actions as A
    import qualified BigNumber as BN
    import qualified BigVector as BV

    data BigMatrix v a = BigMatrix {
        _values :: v (BN.BigNumber a),
        _rows :: Int,
        _columns :: Int
    }

    instance (I.Indexable v, Eq a) => Eq (BigMatrix v a) where
        (==) left@(BigMatrix leftValues _ _) right@(BigMatrix rightValues _ _) = I.equalIndexable leftValues rightValues

    instance (I.Indexable v, H.Hashable a) => H.Hashable (BigMatrix v a) where
        hashWithSalt salt (BigMatrix values rows cols) = H.hashWithSalt sizeHash hashed
            where rowHash = H.hashWithSalt salt rows
                  colHash = H.hashWithSalt salt cols
                  sizeHash = H.hashWithSalt rowHash colHash
                  hashed = H.hashWithSalt salt values

    matrix :: (I.Indexable f, I.Indexable v) => f (BN.BigNumber a) -> Int -> Int -> BigMatrix v a
    matrix values rows cols
        | rows <= 0 || cols <= 0 = error "The number of rows must be greater than 0 and the number of columns must be greater than 0"
        | valueCount /= dimensionSize = error "Value count does not fit the specified dimensions"
        | otherwise = BigMatrix containedValues rows cols
        where valueCount = Fo.length values
              dimensionSize = rows * cols
              containedValues = I.switch values

    rowsInt :: BigMatrix v a -> Int
    rowsInt (BigMatrix _ rows _) = rows

    rows :: (Num a) => BigMatrix v a -> BN.BigNumber a
    rows = BN.asNumber . rowsInt

    columnsInt :: BigMatrix v a -> Int
    columnsInt (BigMatrix _ _ cols) = cols

    columns :: (Num a) => BigMatrix v a -> BN.BigNumber a
    columns = BN.asNumber . columnsInt

    equalSize :: BigMatrix v a -> BigMatrix v a -> Bool
    equalSize (BigMatrix _ leftRows leftCols) (BigMatrix _ rightRows rightCols) = (leftRows == rightRows) && (leftCols == rightCols)

    isMatrixMultipliable :: BigMatrix v a -> BigMatrix v a -> Bool
    isMatrixMultipliable (BigMatrix _ _ leftCols) (BigMatrix _ rightRows _) = leftCols == rightRows

    square :: BigMatrix v a -> Bool
    square (BigMatrix _ rows cols) = rows == cols

    null :: (Fo.Foldable v, Num a, Eq a) => BigMatrix v a -> Bool
    null (BigMatrix values _ _) = Fo.all (==BN.zero) values

    get :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BN.BigNumber a)
    get matrix rowIndex colIndex
        | not $ (BN.isInteger rowIndex) && (BN.isInteger colIndex) = A.failure A.NotInteger "Indices must be nonnegative integers"
        | otherwise = getInt matrix intRowIndex intColIndex
        where intRowIndex = BN.asIntegral rowIndex
              intColIndex = BN.asIntegral colIndex

    getInt :: (I.Indexable v) => BigMatrix v a -> Int -> Int -> A.Computation (BN.BigNumber a)
    getInt (BigMatrix values rows cols) rowIndex colIndex
        | (rowIndex < 0) || (rowIndex >= rows) || (colIndex < 0) || (colIndex >= cols) = A.failure A.IndexOutOfBounds "Indices outside the bounds of the matrix"
        | otherwise = A.success $ I.at values actualIndex
        where actualIndex = rowIndex * cols + colIndex

    _binaryElementwise :: (I.Indexable v, Num a, Eq a) => BN.BinaryNumberAction a -> BigMatrix v a -> BigMatrix v a -> A.Computation (BigMatrix v a)
    _binaryElementwise action left@(BigMatrix leftValues leftRows leftCols) right@(BigMatrix rightValues _ _)
        | not $ equalSize left right = A.failure A.UnequalDimensions "Matrices must be of the same dimensions for this operation"
        | otherwise = A.success $ BigMatrix (I.pairOn action leftValues rightValues) leftRows leftCols

    plus :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> BigMatrix v a -> A.Computation (BigMatrix v a)
    plus = _binaryElementwise BN.plus

    minus :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> BigMatrix v a -> A.Computation (BigMatrix v a)
    minus = _binaryElementwise BN.minus

    scale :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> BigMatrix v a -> A.Computation (BigMatrix v a)
    scale = _binaryElementwise BN.multiply

    _unaryElementwise :: (Fu.Functor v, Num a, Eq a) => BN.UnaryNumberAction a -> BigMatrix v a -> BigMatrix v a
    _unaryElementwise action (BigMatrix values rows cols) = BigMatrix resultValues rows cols
        where resultValues = Fu.fmap action values

    scalarMultiplyLeft :: (Fu.Functor v, Num a, Eq a) => BN.BigNumber a -> BigMatrix v a -> BigMatrix v a
    scalarMultiplyLeft left = _unaryElementwise (BN.multiply left)

    scalarMultiplyRight :: (Fu.Functor v, Num a, Eq a) => BigMatrix v a -> BN.BigNumber a -> BigMatrix v a
    scalarMultiplyRight left right = _unaryElementwise (`BN.multiply` right) left

    negate :: (Fu.Functor v, Num a, Eq a) => BigMatrix v a -> BigMatrix v a
    negate = scalarMultiplyLeft BN.negOne

    _rowTraversal :: Int -> Int -> Int -> (Int, Int)
    _rowTraversal rowIndex colIndex cols = (nextRowIndex, nextColIndex)
        where lastColIndex = cols - 1
              nextRowIndex = if colIndex == lastColIndex then rowIndex + 1 else rowIndex
              nextColIndex = if colIndex == lastColIndex then 0 else colIndex + 1

    _matrixMultiplyHelper2 :: (I.Indexable v, Num a, Eq a) => Int -> Int -> Int -> Int -> BigMatrix v a -> BigMatrix v a -> BN.BigNumber a
    _matrixMultiplyHelper2 index endIndex rowIndex colIndex left right
        | index == endIndex = BN.zero
        | otherwise = BN.plus elem next
        where leftElem = A.value $ getInt left rowIndex index
              rightElem = A.value $ getInt right index colIndex
              elem = BN.multiply leftElem rightElem
              next = _matrixMultiplyHelper2 (index + 1) endIndex rowIndex colIndex left right

    _matrixMultiplyHelper1 :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> BigMatrix v a -> Int -> Int -> Int -> Int -> v (BN.BigNumber a)
    _matrixMultiplyHelper1 left right rowIndex colIndex rows cols
        | nextRowIndex == rows = I.only elem
        | otherwise = I.prepend elem next
        where elem = _matrixMultiplyHelper2 0 rows rowIndex colIndex left right
              next = _matrixMultiplyHelper1 left right nextRowIndex nextColIndex rows cols
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex cols

    matrixMultiply :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> BigMatrix v a -> A.Computation (BigMatrix v a)
    matrixMultiply left@(BigMatrix _ leftRows leftCols) right@(BigMatrix _ rightRows rightCols)
        | not $ isMatrixMultipliable left right = A.failure A.InvalidInput "Left matrix must have the same number of columns as the right matrix has rows"
        | otherwise = A.success $ BigMatrix resultValues leftRows rightCols
        where resultValues = _matrixMultiplyHelper1 left right 0 0 leftRows rightCols

    _determinantHelper2 :: (I.Indexable v, RealFloat a, Eq a) => BigMatrix v a -> Int -> BN.BigNumber a
    _determinantHelper2 mat@(BigMatrix _ _ cols) colIndex
        | colIndex == cols = BN.zero
        | otherwise = BN.plus elem next
        where a = BN.power BN.negOne (BN.asNumber colIndex)
              b = A.value $ getInt mat 0 colIndex
              c = (_determinantHelper1 . A.value) $ _submatrixHelper1 mat 0 colIndex
              elem = Fo.foldr BN.multiply BN.one [c, b, a]
              next = _determinantHelper2 mat (colIndex + 1)

    _determinantHelper1 :: (I.Indexable v, RealFloat a, Eq a) => BigMatrix v a -> BN.BigNumber a
    _determinantHelper1 mat@(BigMatrix _ rows _)
        | 2 == rows = BN.minus (BN.multiply a d) (BN.multiply b c)
        | otherwise = _determinantHelper2 mat 0
        where a = A.value $ getInt mat 0 0
              b = A.value $ getInt mat 0 1
              c = A.value $ getInt mat 1 0
              d = A.value $ getInt mat 1 1

    determinant :: (I.Indexable v, RealFloat a, Eq a) => BigMatrix v a -> A.Computation (BN.BigNumber a)
    determinant mat@(BigMatrix _ rows cols)
        | not $ square mat = A.failure A.NonsquareMatrix "Matrix must be square to have a determinant"
        | 1 == rows = getInt mat 0 0
        | otherwise = A.success $ _determinantHelper1 mat

    _submatrixHelper2 :: (I.Indexable v) => v (BN.BigNumber a) -> Int -> Int -> Int -> Int -> Int -> v (BN.BigNumber a)
    _submatrixHelper2 values rows cols avoidRowIndex avoidColIndex rowIndex
        | rowIndex == rows = I.empty
        | rowIndex == avoidRowIndex = nextRows
        | otherwise = I.attach currentRow nextRows
        where (rowValues, restRows) = I.part cols values
              startElems = I.draw avoidColIndex rowValues
              endElems = I.skip (avoidColIndex + 1) rowValues
              currentRow = I.attach startElems endElems
              nextRows = _submatrixHelper2 restRows rows cols avoidRowIndex avoidColIndex (rowIndex + 1)

    _submatrixHelper1 :: (I.Indexable v) => BigMatrix v a -> Int -> Int -> A.Computation (BigMatrix v a)
    _submatrixHelper1 (BigMatrix values rows cols) avoidRowIndex avoidColIndex
        | (avoidRowIndex < 0) || (avoidRowIndex >= rows) || (avoidColIndex < 0) || (avoidColIndex >= cols) = A.failure A.IndexOutOfBounds "Indices outside the bounds of the matrix"
        | otherwise = A.success $ BigMatrix resultValues (rows - 1) (cols - 1)
        where resultValues = _submatrixHelper2 values rows cols avoidRowIndex avoidColIndex 0

    submatrix :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BigMatrix v a)
    submatrix matrix avoidRowIndex avoidColIndex
        | not $ (BN.isInteger avoidRowIndex) && (BN.isInteger avoidColIndex) = A.failure A.NotInteger "Indices must be integers"
        | otherwise = _submatrixHelper1 matrix intAvoidRowIndex intAvoidColIndex
        where intAvoidRowIndex = BN.asIntegral avoidRowIndex
              intAvoidColIndex = BN.asIntegral avoidColIndex

    _minorHelper :: (I.Indexable v, RealFloat a, Eq a) => BigMatrix v a -> Int -> Int -> v (BN.BigNumber a)
    _minorHelper mat@(BigMatrix _ rows cols) rowIndex colIndex
        | rowIndex == rows = I.empty
        | otherwise = I.prepend elem next
        where submat = A.value $ _submatrixHelper1 mat rowIndex colIndex
              elem = A.value $ determinant submat
              (nextRowIndex, nextColIndex) = _rowTraversal rowIndex colIndex cols
              next = _minorHelper mat nextRowIndex nextColIndex

    _minor :: (I.Indexable v, RealFloat a, Eq a) => BigMatrix v a -> BigMatrix v a
    _minor mat@(BigMatrix _ rows cols) = BigMatrix resultValues rows cols
        where resultValues = _minorHelper mat 0 0

    _cofactorsHelper :: (I.Indexable v, Num a, Eq a) => v (BN.BigNumber a) -> Bool -> v (BN.BigNumber a)
    _cofactorsHelper values sign
        | Fo.null values = I.empty
        | otherwise = I.prepend elem next
        where val = I.front values
              rest = I.trail values
              elem = if sign then BN.negate val else val
              next = _cofactorsHelper rest (not sign)

    _cofactors :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> BigMatrix v a
    _cofactors mat@(BigMatrix values rows cols) = BigMatrix resultValues rows cols
        where resultValues = _cofactorsHelper values True

    _transposeHelper :: (I.Indexable v) => BigMatrix v a -> Int -> Int -> v (BN.BigNumber a)
    _transposeHelper mat@(BigMatrix _ rows cols) rowIndex colIndex
        | nextColIndex == cols = I.only elem
        | otherwise = I.prepend elem next
        where elem = A.value $ getInt mat rowIndex colIndex
              lastRowIndex = rows - 1
              nextRowIndex = if rowIndex == lastRowIndex then 0 else rowIndex + 1
              nextColIndex = if rowIndex == lastRowIndex then colIndex + 1 else colIndex
              next = _transposeHelper mat nextRowIndex nextColIndex

    transpose :: (I.Indexable v) => BigMatrix v a -> BigMatrix v a
    transpose mat@(BigMatrix _ rows cols) = BigMatrix resultValues cols rows
        where resultValues = _transposeHelper mat 0 0

    matrixInverse :: (I.Indexable v, RealFloat a, Eq a) => BigMatrix v a -> A.Computation (BigMatrix v a)
    matrixInverse mat@(BigMatrix _ rows cols)
        | not $ square mat = A.failure A.NonsquareMatrix "A matrix must be square to have an inverse"
        | BN.zero == detValue = A.failure A.DeterminantOfZero "A matrix must not have a determinant of zero to have an inverse"
        | otherwise = A.success $ case rows of 
                                    1 -> BigMatrix (I.only invOfDetValue) 1 1
                                    2 -> scalarMultiplyLeft invOfDetValue mat
                                    _ -> scalarMultiplyLeft invOfDetValue ((transpose . _cofactors) $ _minor mat)
        where detValue = A.value $ determinant mat
              invOfDetValue = A.value $ BN.inverse detValue

    matrixDivide :: (I.Indexable v, RealFloat a, Eq a) => BigMatrix v a -> BigMatrix v a -> A.Computation (BigMatrix v a)
    matrixDivide left right = A.resolveErrableBinary leftVal rightInverse matrixMultiply
        where rightInverse = matrixInverse right
              leftVal = A.success left

    _addRowHelper :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> Int -> Int -> v (BN.BigNumber a)
    _addRowHelper (BigMatrix values rows cols) fromRowIndex toRowIndex = I.link [startElems, newElems, endElems]
        where actualFromIndex = rows * fromRowIndex
              actualToIndex = rows * toRowIndex
              fromRow = I.draw cols (I.skip actualFromIndex values)
              toRow = I.draw cols (I.skip actualToIndex values)
              startElems = I.draw actualToIndex values
              newElems = I.pairOn BN.plus fromRow toRow
              endElems = I.skip (actualToIndex + cols) values

    addRow :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BigMatrix v a)
    addRow mat@(BigMatrix _ rows cols) fromRow toRow
        | not $ (BN.isInteger fromRow) && (BN.isInteger toRow) = A.failure A.NotInteger "Row Indices must be nonnegative integers"
        | (fromRowInt < 0) || (fromRowInt >= rows) || (toRowInt < 0) || (toRowInt >= rows) = A.failure A.IndexOutOfBounds "Row Indices outside the bounds of the matrix"
        | fromRowInt == toRowInt = A.failure A.InvalidInput "Row Indices must be distinct"
        | otherwise = A.success $ BigMatrix resultValues rows cols
        where fromRowInt = BN.asIntegral fromRow
              toRowInt = BN.asIntegral toRow
              resultValues = _addRowHelper mat fromRowInt toRowInt

    _multRowHelper :: (I.Indexable v, Num a, Eq a) => BigMatrix v a -> BN.BigNumber a -> Int -> v (BN.BigNumber a)
    _multRowHelper (BigMatrix values rows cols) multValue rowIndex = I.link [startElems, newElems, endElems]
        where actualRowIndex = rows * rowIndex
              multRow = I.draw cols (I.skip actualRowIndex values)
              startElems = I.draw actualRowIndex values
              newElems = Fu.fmap (BN.multiply multValue) multRow
              endElems = I.skip (actualRowIndex + cols) values

    multRow :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BigMatrix v a)
    multRow mat@(BigMatrix _ rows cols) multValue rowIndex
        | not $ BN.isInteger rowIndex = A.failure A.NotInteger "Row Index must be a nonnegative integer"
        | (intRowIndex < 0) || (intRowIndex >= rows) = A.failure A.IndexOutOfBounds "Row index outside the bounds of the matrix"
        | otherwise = A.success $ BigMatrix resultValues rows cols
        where intRowIndex = BN.asIntegral rowIndex
              resultValues = _multRowHelper mat multValue intRowIndex

    _swapRowsHelper :: (I.Indexable v) => BigMatrix v a -> Int -> Int -> v (BN.BigNumber a)
    _swapRowsHelper (BigMatrix values rows cols) rowIndex1 rowIndex2 = I.link [startElems, lowerElems, midElems, higherElems, endElems]
        where actualRowIndex1 = rows * rowIndex1
              actualRowIndex2 = rows * rowIndex2
              minRowIndex = min actualRowIndex1 actualRowIndex2
              maxRowIndex = max actualRowIndex1 actualRowIndex2
              startElems = I.draw minRowIndex values
              lowerElems = I.draw cols (I.skip minRowIndex values)
              midElems = I.draw (maxRowIndex - minRowIndex - cols) (I.skip (minRowIndex + cols) values)
              higherElems = I.draw cols (I.skip maxRowIndex values)
              endElems = I.skip (maxRowIndex + cols) values

    swapRows :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BigMatrix v a)
    swapRows mat@(BigMatrix _ rows cols) rowIndex1 rowIndex2
        | not $ (BN.isInteger rowIndex1) && (BN.isInteger rowIndex2) = A.failure A.NotInteger "Row indices must be nonnegative integers"
        | (intRowIndex1 < 0) || (intRowIndex1 >= rows) || (intRowIndex1 < 0) || (intRowIndex2 >= rows) = A.failure A.IndexOutOfBounds "Row indices outside the bounds of the matrix"
        | otherwise = A.success $ BigMatrix resultValues rows cols
        where intRowIndex1 = BN.asIntegral rowIndex1
              intRowIndex2 = BN.asIntegral rowIndex2
              resultValues = _swapRowsHelper mat intRowIndex1 intRowIndex2

    _addColHelper :: (I.Indexable v, Num a, Eq a) => v (BN.BigNumber a) -> Int -> Int -> Int -> v (BN.BigNumber a)
    _addColHelper table cols fromCol toCol
        | Fo.null table = I.empty
        | otherwise = I.attach newRow nextRows
        where currentRow = I.draw cols table
              restRows = I.skip cols table
              fromElem = I.at currentRow fromCol
              toElem = I.at currentRow toCol
              newElem = BN.plus fromElem toElem
              startElems = I.draw (toCol - 1) currentRow
              endElems = I.skip (toCol + 1) currentRow
              newRow = I.link [startElems, I.only newElem, endElems]
              nextRows = _addColHelper restRows cols fromCol toCol

    addCol :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BigMatrix v a)
    addCol (BigMatrix table rows cols) fromCol toCol
        | not $ (BN.isInteger fromCol) && (BN.isInteger toCol) = A.failure A.NotInteger "Column indices must be nonnegative integers"
        | (fromColInt < 0) || (fromColInt >= cols) || (toColInt < 0) || (toColInt >= cols) = A.failure A.IndexOutOfBounds "Row Indices outside the bounds of the matrix"
        | fromColInt == toColInt = A.failure A.InvalidInput "Column indices must be distinct"
        | otherwise = A.success $ BigMatrix resultValues rows cols
        where fromColInt = BN.asIntegral fromCol
              toColInt = BN.asIntegral toCol
              resultValues = _addColHelper table cols fromColInt toColInt

    _multColHelper :: (I.Indexable v, Num a, Eq a) => v (BN.BigNumber a) -> Int -> BN.BigNumber a -> Int -> v (BN.BigNumber a)
    _multColHelper table cols multValue colIndex
        | Fo.null table = I.empty
        | otherwise = I.attach newRow nextRows
        where currentRow = I.draw cols table
              restRows = I.skip cols table
              elem = I.at currentRow colIndex
              newElem = BN.multiply multValue elem
              startElems = I.draw (colIndex - 1) currentRow
              endElems = I.skip (colIndex + 1) currentRow
              newRow = I.link [startElems, I.only newElem, endElems]
              nextRows = _multColHelper restRows cols multValue colIndex

    multCol :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BigMatrix v a)
    multCol (BigMatrix table rows cols) multValue colIndex
        | not $ BN.isInteger colIndex = A.failure A.NotInteger "Column index must be a nonnegative integer"
        | (intColIndex < 0) || (intColIndex >= cols) = A.failure A.IndexOutOfBounds "Column index outside the bounds of the matrix"
        | otherwise = A.success $ BigMatrix resultValues rows cols
        where intColIndex = BN.asIntegral colIndex
              resultValues = _multColHelper table cols multValue intColIndex

    _swapColsHelper :: (I.Indexable v) => v (BN.BigNumber a) -> Int -> Int -> Int -> v (BN.BigNumber a)
    _swapColsHelper table cols colIndex1 colIndex2
        | Fo.null table = I.empty
        | otherwise = I.attach newRow nextRows
        where currentRow = I.draw cols table
              restRows = I.skip cols table
              minColIndex = min colIndex1 colIndex2
              maxColIndex = max colIndex1 colIndex2
              lowerElem = I.at currentRow minColIndex
              higherElem = I.at currentRow maxColIndex
              startElems = I.draw minColIndex currentRow
              midElems = I.draw (maxColIndex - minColIndex) (I.skip minColIndex currentRow)
              endElems = I.skip maxColIndex currentRow
              newRow = I.link [startElems, I.only lowerElem, midElems, I.only higherElem, endElems]
              nextRows = _swapColsHelper restRows cols colIndex1 colIndex2

    swapCols :: (I.Indexable v, RealFrac a) => BigMatrix v a -> BN.BigNumber a -> BN.BigNumber a -> A.Computation (BigMatrix v a)
    swapCols (BigMatrix table rows cols) colIndex1 colIndex2
        | not $ (BN.isInteger colIndex1) && (BN.isInteger colIndex2) = A.failure A.NotInteger "Column indices must be nonnegative integers"
        | (intColIndex1 < 0) || (intColIndex1 >= cols) || (intColIndex2 < 0) || (intColIndex2 >= cols) = A.failure A.IndexOutOfBounds "Column indices outside the bounds of the matrix"
        | otherwise = A.success $ BigMatrix resultValues rows cols
        where intColIndex1 = BN.asIntegral colIndex1
              intColIndex2 = BN.asIntegral colIndex2
              resultValues = _swapColsHelper table cols intColIndex1 intColIndex2

    toContainer :: (I.Indexable v1, I.Indexable v2) => BigMatrix v1 a -> v2 (BN.BigNumber a)
    toContainer (BigMatrix values _ _) = I.switch values

    to2dContainer :: (I.Indexable v1, I.Indexable v2) => BigMatrix v1 a -> v2 (v2 (BN.BigNumber a))
    to2dContainer (BigMatrix values _ cols) = _to2dContainerHelper (I.switch values) cols

    _to2dContainerHelper :: (I.Indexable v) => v (BN.BigNumber a) -> Int -> v (v (BN.BigNumber a))
    _to2dContainerHelper values cols
        | Fo.null values = I.empty
        | otherwise = I.prepend currentRow nextRows
        where currentRow = I.draw cols values
              restRows = I.skip cols values
              nextRows = _to2dContainerHelper restRows cols

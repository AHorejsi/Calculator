{-# LANGUAGE DeriveGeneric #-}

module BigMatrix (
    BigMatrix,
    mlist,
    mseq,
    mrepeat,
    midentity,
    mrows,
    mcols,
    msize,
    mrowLength,
    mcolLength,
    mlength,
    mEqualSize,
    isMatrixMultipliable,
    mIsSquare,
    mget,
    mgetInt,
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
    mAddRow,
    mMultRow,
    mSwapRows,
    mAddCol,
    mMultCol,
    mSwapCols
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Sequence as S
    import qualified Data.Foldable as F
    import qualified Data.Hashable as H
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
        show (BigMatrix table rows cols) = TP.printf "Matrix[%s]" (_str table show rows cols)

    instance Str.Stringifier BigMatrix where
        stringify (BigMatrix table rows cols) = TP.printf "[%s]" (_str table Str.stringify rows cols)

    _str :: S.Seq BS.BigScalar -> (BS.BigScalar -> String) -> Int -> Int -> String
    _str table converter rowsLeft totalCols = case rowsLeft of 0 -> ""
                                                               1 -> TP.printf "%s" finalString
                                                               _ -> TP.printf "%s|%s" finalString next
        where (currentRow, rest) = S.splitAt totalCols table
              stringList = fmap converter currentRow
              commaSeparated = S.intersperse "," stringList
              finalString = F.foldl' (++) "" commaSeparated
              next = _str rest converter (rowsLeft - 1) totalCols

    mlist :: [[BS.BigScalar]] -> BigMatrix
    mlist table = BigMatrix seq rowLength colLength
        where seq = S.fromList $ concat table
              rowLength = length table
              colLength = length $ head table

    mseq :: S.Seq (S.Seq BS.BigScalar) -> BigMatrix
    mseq table = BigMatrix concatSeq rowLength colLength
        where concatSeq = F.foldl' (S.><) S.empty table
              rowLength = length table
              colLength = length $ S.index table 0

    mrepeat :: BS.BigScalar -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mrepeat scalar rows cols
        | not $ BS.isExactInteger rows && BS.isExactInteger cols = MI.withError MI.InvalidValue
        | intRows < 0 || intCols < 0 = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix table intRows intCols
        where intRows = BS.asBuiltInInt rows
              intCols = BS.asBuiltInInt cols
              elemCount = intRows * intCols
              table = S.replicate elemCount scalar

    midentity :: BS.BigScalar -> MI.ComputationResult BigMatrix
    midentity dimensions
        | not $ BS.isExactInteger dimensions = MI.withError MI.InvalidValue
        | intDimensions < 0 = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix table intDimensions intDimensions
        where intDimensions = BS.asBuiltInInt dimensions
              table = _makeIdentityMatrix 0 0 intDimensions

    _makeIdentityMatrix :: Int -> Int -> Int -> S.Seq BS.BigScalar
    _makeIdentityMatrix currentRow currentCol dimensions
        | currentRow == dimensions = S.empty
        | otherwise = elem S.<| (_makeIdentityMatrix nextRowIndex nextColIndex dimensions)
        where elem = if currentRow == currentCol then BS.one else BS.zero
              (nextRowIndex, nextColIndex) = _rowTraversal currentRow currentCol dimensions

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

    mEqualSize :: BigMatrix -> BigMatrix -> Bool
    mEqualSize (BigMatrix _ leftRows leftCols) (BigMatrix _ rightRows rightCols) = (leftRows == rightRows) && (leftCols == rightCols)

    isMatrixMultipliable :: BigMatrix -> BigMatrix -> Bool
    isMatrixMultipliable (BigMatrix _ _ leftCols) (BigMatrix _ rightRows _) = leftCols == rightRows

    mIsSquare :: BigMatrix -> Bool
    mIsSquare (BigMatrix _ rows cols) = rows == cols

    mget :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BS.BigScalar
    mget matrix rowIndex colIndex
        | not $ (BS.isExactInteger rowIndex) && (BS.isExactInteger colIndex) = MI.withError MI.InvalidType
        | otherwise = mgetInt matrix intRowIndex intColIndex
        where intRowIndex = BS.asBuiltInInt rowIndex
              intColIndex = BS.asBuiltInInt colIndex

    mgetInt :: BigMatrix -> Int -> Int -> MI.ComputationResult BS.BigScalar
    mgetInt matrix@(BigMatrix table rows cols) rowIndex colIndex
        | (rowIndex < 0) || (rowIndex >= rows) || (colIndex < 0) || (colIndex >= cols) = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ S.index table actualIndex
        where actualIndex = rowIndex * cols + colIndex

    mplus :: BigMatrix -> BigMatrix -> MI.ComputationResult BigMatrix
    mplus = _binaryOperation BS.splus

    mminus :: BigMatrix -> BigMatrix -> MI.ComputationResult BigMatrix
    mminus = _binaryOperation BS.sminus

    mscale :: BigMatrix -> BigMatrix -> MI.ComputationResult BigMatrix
    mscale = _binaryOperation BS.smult

    _binaryOperation :: BS.BinaryScalarAction -> BigMatrix -> BigMatrix -> MI.ComputationResult BigMatrix
    _binaryOperation operation left@(BigMatrix leftTable leftRows leftCols) right@(BigMatrix rightTable _ _)
        | not $ mEqualSize left right = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix (S.zipWith operation leftTable rightTable) leftRows leftCols

    smultm :: BS.BigScalar -> BigMatrix -> BigMatrix
    smultm left = _unaryOperation (BS.smult left)

    mmults :: BigMatrix -> BS.BigScalar -> BigMatrix
    mmults left right = _unaryOperation ((flip BS.smult) right) left

    vmultm :: BV.BigVector -> BigMatrix -> MI.ComputationResult BigMatrix
    vmultm left = mmult (mRowVector left)

    mmultv :: BigMatrix -> BV.BigVector -> MI.ComputationResult BigMatrix
    mmultv left right = mmult left (mColVector right)

    mRowVector :: BV.BigVector -> BigMatrix
    mRowVector vec = BigMatrix list 1 dimensions
        where dimensions = BV.vlength vec
              list = BV.asSeq vec
    
    mColVector :: BV.BigVector -> BigMatrix
    mColVector vec = BigMatrix list dimensions 1
        where dimensions = BV.vlength vec
              list = BV.asSeq vec

    _unaryOperation :: BS.UnaryScalarAction -> BigMatrix -> BigMatrix
    _unaryOperation operation (BigMatrix table rows cols) = BigMatrix (fmap operation table) rows cols

    mmult :: BigMatrix -> BigMatrix -> MI.ComputationResult BigMatrix
    mmult left@(BigMatrix _ leftRows leftCols) right@(BigMatrix _ rightRows rightCols)
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
        | otherwise = BS.splus elem next
        where elem = BS.smult leftElem rightElem
              next = _mmultHelper2 (index + 1) endIndex rowIndex colIndex left right
              leftElem = MI.value $ mgetInt left rowIndex index
              rightElem = MI.value $ mgetInt right index colIndex

    _rowTraversal :: Int -> Int -> Int -> (Int, Int)
    _rowTraversal rowIndex colIndex cols = (nextRowIndex, nextColIndex)
        where lastColIndex = cols - 1
              nextRowIndex = if colIndex == lastColIndex then rowIndex + 1 else rowIndex
              nextColIndex = if colIndex == lastColIndex then 0 else colIndex + 1

    mdivs :: BigMatrix -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mdivs left right
        | BS.zero == right = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ _errableUnaryOperation ((flip BS.sdiv) right) left

    _errableUnaryOperation :: BS.ErrableUnaryScalarAction -> BigMatrix -> BigMatrix
    _errableUnaryOperation operation (BigMatrix table rows cols) = BigMatrix newTable rows cols
        where newTable = fmap (MI.value . operation) table

    mdiv :: BigMatrix -> BigMatrix -> MI.ComputationResult BigMatrix
    mdiv left right
        | not $ mIsSquare right = MI.withError MI.InvalidState
        | not $ isMatrixMultipliable left right = MI.withError MI.InvalidValue
        | otherwise = MI.errBinResolveRight left invResult mmult
        where invResult = minv right

    mneg :: BigMatrix -> BigMatrix
    mneg = smultm BS.negOne

    mdet :: BigMatrix -> MI.ComputationResult BS.BigScalar
    mdet matrix@(BigMatrix _ rows cols)
        | not $ mIsSquare matrix = MI.withError MI.InvalidState
        | 1 == rows = mgetInt matrix 0 0
        | otherwise = MI.withValue $ _mdetHelper1 matrix

    _mdetHelper1 :: BigMatrix -> BS.BigScalar
    _mdetHelper1 matrix@(BigMatrix _ rows _)
        | 2 == rows = BS.sminus (BS.smult a d) (BS.smult b c)
        | otherwise = _mdetHelper2 matrix 0
        where a = MI.value $ mgetInt matrix 0 0
              b = MI.value $ mgetInt matrix 0 1
              c = MI.value $ mgetInt matrix 1 0
              d = MI.value $ mgetInt matrix 1 1

    _mdetHelper2 :: BigMatrix -> Int -> BS.BigScalar
    _mdetHelper2 matrix@(BigMatrix _ _ cols) colIndex
        | colIndex == cols = BS.zero
        | otherwise = BS.splus newElem (_mdetHelper2 matrix (colIndex + 1))
        where a = MI.value $ BS.spow BS.negOne (BS.integral colIndex)
              b = MI.value $ mgetInt matrix 0 colIndex
              c = _mdetHelper1 $ MI.value $ _msubHelper1 matrix 0 colIndex
              newElem = F.foldr BS.smult BS.one [c, b, a]

    _mminor :: BigMatrix -> MI.ComputationResult BigMatrix
    _mminor matrix@(BigMatrix _ rows cols)
        | not $ mIsSquare matrix = MI.withError MI.InvalidState
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
        
    minv :: BigMatrix -> MI.ComputationResult BigMatrix
    minv matrix@(BigMatrix _ rows cols)
        | not $ mIsSquare matrix = MI.withError MI.InvalidState
        | BS.zero == detValue = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ case rows of 1 -> BigMatrix (S.singleton invOfDetValue) 1 1
                                                  2 -> smultm invOfDetValue (mlist [[val00, val01], [val10, val11]])
                                                  _ -> smultm invOfDetValue (mtranspose $ _mcofactors (MI.value $ _mminor matrix))
        where detValue = MI.value $ mdet matrix
              invOfDetValue = MI.value $ BS.sinv detValue
              val00 = MI.value $ mgetInt matrix 1 1
              val01 = BS.sneg $ MI.value $ mgetInt matrix 0 1
              val10 = BS.sneg $ MI.value $ mgetInt matrix 1 0
              val11 = MI.value $ mgetInt matrix 0 0
    
    mtranspose :: BigMatrix -> BigMatrix
    mtranspose matrix@(BigMatrix _ rows cols) = BigMatrix (_mtransposeHelper matrix 0 0) cols rows

    _mtransposeHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _mtransposeHelper matrix@(BigMatrix _ rows cols) rowIndex colIndex
        | nextColIndex == cols = S.singleton elem
        | otherwise = elem S.<| next
        where lastRowIndex = rows - 1
              nextRowIndex = if rowIndex == lastRowIndex then 0 else rowIndex + 1
              nextColIndex = if rowIndex == lastRowIndex then colIndex + 1 else colIndex
              elem = MI.value $ mgetInt matrix rowIndex colIndex
              next = _mtransposeHelper matrix nextRowIndex nextColIndex

    msub :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    msub matrix avoidRowIndex avoidColIndex = _msubHelper1 matrix avoidRowIndexInt avoidColIndexInt
        where avoidRowIndexInt = BS.asBuiltInInt avoidRowIndex
              avoidColIndexInt = BS.asBuiltInInt avoidColIndex

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

    mAddRow :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mAddRow matrix@(BigMatrix _ rows cols) fromRowIndex toRowIndex
        | not $ (BS.isExactInteger fromRowIndex) && (BS.isExactInteger toRowIndex) = MI.withError MI.InvalidValue
        | intFromRowIndex < 0 || intFromRowIndex >= rows || intToRowIndex < 0 || intToRowIndex >= rows = MI.withError MI.InvalidValue
        | intFromRowIndex == intToRowIndex = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newSeq rows cols
        where intFromRowIndex = BS.asBuiltInInt fromRowIndex
              intToRowIndex = BS.asBuiltInInt toRowIndex
              newSeq = _mAddRowHelper matrix intFromRowIndex intToRowIndex

    _mAddRowHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _mAddRowHelper (BigMatrix table rows cols) fromRowIndex toRowIndex = startElems S.>< newElems S.>< endElems
        where actualFromIndex = rows * fromRowIndex
              actualToIndex = rows * toRowIndex
              fromRow = S.take cols (S.drop actualFromIndex table)
              toRow = S.take cols (S.drop actualToIndex table)
              startElems = S.take actualToIndex table
              newElems = S.zipWith BS.splus fromRow toRow
              endElems = S.drop (actualToIndex + cols) table

    mMultRow :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mMultRow matrix@(BigMatrix _ rows cols) multValue rowIndex
        | not $ BS.isExactInteger rowIndex = MI.withError MI.InvalidValue
        | intRowIndex < 0 || intRowIndex >= rows = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intRowIndex = BS.asBuiltInInt rowIndex
              newTable = _mMultRowHelper matrix multValue intRowIndex

    _mMultRowHelper :: BigMatrix -> BS.BigScalar -> Int -> S.Seq BS.BigScalar
    _mMultRowHelper (BigMatrix table rows cols) multValue rowIndex = startElems S.>< newElems S.>< endElems
        where actualIndex = rows * rowIndex
              multRow = S.take cols (S.drop actualIndex table)
              startElems = S.take actualIndex table
              newElems = fmap (BS.smult multValue) multRow
              endElems = S.drop (actualIndex + cols) table

    mSwapRows :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mSwapRows matrix@(BigMatrix _ rows cols) rowIndex1 rowIndex2
        | not $ (BS.isExactInteger rowIndex1) && (BS.isExactInteger rowIndex2) = MI.withError MI.InvalidValue
        | (intRowIndex1 < 0) || (intRowIndex1 >= rows) || (intRowIndex2 < 0) || (intRowIndex2 >= rows) = MI.withError MI.InvalidValue
        | intRowIndex1 == intRowIndex2 = MI.withValue matrix
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intRowIndex1 = BS.asBuiltInInt rowIndex1
              intRowIndex2 = BS.asBuiltInInt rowIndex2
              newTable = _mSwapRowsHelper matrix intRowIndex1 intRowIndex2
    
    _mSwapRowsHelper :: BigMatrix -> Int -> Int -> S.Seq BS.BigScalar
    _mSwapRowsHelper (BigMatrix table rows cols) rowIndex1 rowIndex2 = startElems S.>< higherElems S.>< midElems S.>< lowerElems S.>< endElems
        where actualRowIndex1 = rows * rowIndex1
              actualRowIndex2 = rows * rowIndex2
              lowerRowIndex = min actualRowIndex1 actualRowIndex2
              higherRowIndex = max actualRowIndex1 actualRowIndex2
              startElems = S.take lowerRowIndex table
              lowerElems = S.take cols (S.drop lowerRowIndex table)
              midElems = S.take (higherRowIndex - lowerRowIndex - cols) (S.drop (lowerRowIndex + cols) table)
              higherElems = S.take cols (S.drop higherRowIndex table)
              endElems = S.drop (higherRowIndex + cols) table

    mAddCol :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mAddCol (BigMatrix table rows cols) fromColIndex toColIndex
        | not $ (BS.isExactInteger fromColIndex) && (BS.isExactInteger toColIndex) = MI.withError MI.InvalidValue
        | (intFromColIndex < 0) || (intFromColIndex >= cols) || (intToColIndex < 0) || (intToColIndex >= cols) = MI.withError MI.InvalidValue
        | intFromColIndex == intToColIndex = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intFromColIndex = BS.asBuiltInInt fromColIndex
              intToColIndex = BS.asBuiltInInt toColIndex
              newTable = _mAddColHelper table rows cols intFromColIndex intToColIndex 0

    _mAddColHelper :: S.Seq BS.BigScalar -> Int -> Int -> Int -> Int -> Int -> S.Seq BS.BigScalar
    _mAddColHelper table rows cols fromColIndex toColIndex currentRowIndex
        | rows == currentRowIndex = S.empty
        | otherwise = newRow S.>< nextRows
        where (row, restRows) = S.splitAt cols table
              fromElem = S.index row fromColIndex
              toElem = S.index row toColIndex
              newElem = S.singleton $ BS.splus fromElem toElem
              startRow = S.take toColIndex row
              endRow = S.drop (toColIndex + 1) row
              newRow = startRow S.>< newElem S.>< endRow
              nextRows = _mAddColHelper restRows rows cols fromColIndex toColIndex (currentRowIndex + 1)

    mMultCol :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mMultCol (BigMatrix table rows cols) multValue colIndex
        | not $ BS.isExactInteger colIndex = MI.withError MI.InvalidValue
        | intColIndex < 0 || intColIndex >= cols = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intColIndex = BS.asBuiltInInt colIndex
              newTable = _mMultColHelper table rows cols multValue intColIndex 0

    _mMultColHelper :: S.Seq BS.BigScalar -> Int -> Int -> BS.BigScalar -> Int -> Int -> S.Seq BS.BigScalar
    _mMultColHelper table rows cols multValue colIndex currentRowIndex
        | rows == currentRowIndex = S.empty
        | otherwise = newRow S.>< nextRows
        where (row, restRows) = S.splitAt cols table
              colElem = S.index row colIndex
              newElem = S.singleton $ BS.smult multValue colElem
              startRow = S.take colIndex row
              endRow = S.drop (colIndex + 1) row
              newRow = startRow S.>< newElem S.>< endRow
              nextRows = _mMultColHelper restRows rows cols multValue colIndex (currentRowIndex + 1)

    mSwapCols :: BigMatrix -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigMatrix
    mSwapCols (BigMatrix table rows cols) colIndex1 colIndex2
        | not $ (BS.isExactInteger colIndex1) && (BS.isExactInteger colIndex2) = MI.withError MI.InvalidValue
        | intColIndex1 < 0 || intColIndex1 >= cols || intColIndex2 < 0 || intColIndex2 >= cols = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigMatrix newTable rows cols
        where intColIndex1 = BS.asBuiltInInt colIndex1
              intColIndex2 = BS.asBuiltInInt colIndex2
              newTable = _mSwapColsHelper table rows cols intColIndex1 intColIndex2 0

    _mSwapColsHelper :: S.Seq BS.BigScalar -> Int -> Int -> Int -> Int -> Int -> S.Seq BS.BigScalar
    _mSwapColsHelper table rows cols colIndex1 colIndex2 currentRowIndex
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
              nextRows = _mSwapColsHelper restRows rows cols colIndex1 colIndex2 (currentRowIndex + 1)

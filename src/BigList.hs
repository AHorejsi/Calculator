{-# LANGUAGE DeriveGeneric #-}

module BigList (
    BigList,
    lempty,
    llist,
    lseq,
    lrepeat,
    lincrement,
    lsize,
    llength,
    lEqualSize,
    lget,
    splusl,
    lpluss,
    lplus,
    lplusPad,
    sminusl,
    lminuss,
    lminus,
    lminusPad,
    smultl,
    lmults,
    lmult,
    lmultPad,
    sdivl,
    ldivs,
    ldiv,
    ldivPad,
    spowl,
    lpows,
    lpow,
    lpowPad,
    lneg,
    lminimum,
    lmaximum,
    lsum,
    lcumsum,
    lprod,
    lcumprod,
    lmean,
    lgmean,
    lhmean,
    lmedian,
    lrange,
    lmidrange,
    lmode,
    lsortAsc,
    lsortDesc,
    lIsSortedAsc,
    lIsSortedDesc,
    lIsSorted,
    lsub,
    lconcat,
    lmerge
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Maybe as M
    import qualified Data.Sequence as S
    import qualified Data.Foldable as F
    import qualified Data.Hashable as H
    import qualified Data.HashSet as HS
    import qualified Data.HashMap.Lazy as HM
    import qualified Stringify as Str
    import qualified MathInfo as MI
    import qualified BigScalar as BS

    newtype BigList = BigList {
        _vals :: S.Seq BS.BigScalar
    } deriving (G.Generic, Eq)

    instance H.Hashable BigList where
        hashWithSalt salt (BigList vals) = H.hashWithSalt salt (F.toList vals)

    instance Show BigList where
        show list = _str list show

    instance Str.Stringifier BigList where
        stringify list = TP.printf "{%s}" result
            where result = _str list Str.stringify

    _str :: BigList -> (BS.BigScalar -> String) -> String
    _str (BigList pos) converter = F.foldl' (++) "" commaSeparated
        where stringList = fmap converter pos
              commaSeparated = S.intersperse "," stringList

    lempty :: BigList
    lempty = BigList S.empty

    lseq :: S.Seq BS.BigScalar -> BigList
    lseq = BigList

    llist :: [BS.BigScalar] -> BigList
    llist = lseq . S.fromList

    lrepeat :: BS.BigScalar -> BS.BigScalar -> MI.Result BigList
    lrepeat scalar count
        | not $ BS.isExactInteger count = MI.withError MI.InvalidType
        | intCount < 0 = MI.withError MI.InvalidValue
        | otherwise =  MI.withValue $ _pad intCount scalar lempty
        where intCount = BS.asBuiltInInt count

    lincrement :: BS.BigScalar -> BS.BigScalar -> BS.BigScalar -> MI.Result BigList
    lincrement initial increment count
        | not $ BS.isExactInteger count = MI.withError MI.InvalidType
        | intCount < 0 = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigList $ _computeRange initial increment intCount
        where intCount = BS.asBuiltInInt count
    
    _computeRange :: BS.BigScalar -> BS.BigScalar -> Int -> S.Seq BS.BigScalar
    _computeRange _ _ 0 = S.empty
    _computeRange current increment count = current S.<| next
        where elem = BS.splus current increment
              next = _computeRange elem increment (count - 1)

    lsize :: BigList -> BS.BigScalar
    lsize = BS.integral . llength

    llength :: BigList -> Int
    llength (BigList vals) = S.length vals

    lEqualSize :: BigList -> BigList -> Bool
    lEqualSize left right = (llength left) == (llength right)

    lget :: BigList -> BS.BigScalar -> MI.Result BS.BigScalar
    lget list@(BigList vals) index
        | not $ BS.isExactInteger index = MI.withError MI.InvalidType
        | M.isNothing gotten = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ M.fromJust gotten
        where gotten = vals S.!? intIndex
              intIndex = BS.asBuiltInInt index

    splusl :: BS.BigScalar -> BigList -> BigList
    splusl left = _unaryOperation (BS.splus left)

    lpluss :: BigList -> BS.BigScalar -> BigList
    lpluss = flip splusl

    lplus :: BigList -> BigList -> MI.Result BigList
    lplus = _binaryOperation BS.splus

    lplusPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lplusPad = _binaryOperationPad BS.splus

    sminusl :: BS.BigScalar -> BigList -> BigList
    sminusl left = _unaryOperation (BS.sminus left)

    lminuss :: BigList -> BS.BigScalar -> BigList
    lminuss left right = _unaryOperation ((flip BS.sminus) right) left

    lminus :: BigList -> BigList -> MI.Result BigList
    lminus = _binaryOperation BS.sminus

    lminusPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lminusPad = _binaryOperationPad BS.sminus

    smultl :: BS.BigScalar -> BigList -> BigList
    smultl left = _unaryOperation (BS.smult left)

    lmults :: BigList -> BS.BigScalar -> BigList
    lmults left right = _unaryOperation ((flip BS.smult) right) left

    lmult :: BigList -> BigList -> MI.Result BigList
    lmult = _binaryOperation BS.smult

    lmultPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lmultPad = _binaryOperationPad BS.smult

    sdivl :: BS.BigScalar -> BigList -> MI.Result BigList
    sdivl left = _errableUnaryOperation (BS.sdiv left)

    ldivs :: BigList -> BS.BigScalar -> MI.Result BigList
    ldivs left right = _errableUnaryOperation ((flip BS.sdiv) right) left

    ldiv :: BigList -> BigList -> MI.Result BigList
    ldiv = _errableBinaryOperation BS.sdiv
    
    ldivPad :: BS.BigScalar -> BigList -> BigList -> MI.Result BigList
    ldivPad = _errableBinaryOperationPad BS.sdiv

    spowl :: BS.BigScalar -> BigList -> MI.Result BigList
    spowl left = _errableUnaryOperation (BS.spow left)

    lpows :: BigList -> BS.BigScalar -> MI.Result BigList
    lpows left right = _errableUnaryOperation ((flip BS.spow) right) left

    lpow :: BigList -> BigList -> MI.Result BigList
    lpow = _errableBinaryOperation BS.spow

    lpowPad :: BS.BigScalar -> BigList -> BigList -> MI.Result BigList
    lpowPad = _errableBinaryOperationPad BS.spow

    lneg :: BigList -> BigList
    lneg = smultl BS.negOne

    _unaryOperation :: BS.UnaryScalarAction -> BigList -> BigList
    _unaryOperation operation (BigList vals) = BigList $ fmap operation vals

    _errableUnaryOperation :: BS.ErrableUnaryScalarAction -> BigList -> MI.Result BigList
    _errableUnaryOperation operation (BigList vals)
        | S.null errors = MI.withValue $ BigList $ fmap MI.value result
        | otherwise = MI.withErrorSet $ HS.unions $ F.toList $ fmap MI.errorSet errors
        where result = fmap operation vals
              errors = S.filter MI.isSuccess result

    _binaryOperation :: BS.BinaryScalarAction -> BigList -> BigList -> MI.Result BigList
    _binaryOperation operation left@(BigList leftVals) right@(BigList rightVals)
        | not $ lEqualSize left right = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigList $ S.zipWith operation leftVals rightVals

    _binaryOperationPad :: BS.BinaryScalarAction -> BS.BigScalar -> BigList -> BigList -> BigList
    _binaryOperationPad operation scalar left right = MI.value $ _binaryOperation operation leftPadded rightPadded
        where leftSize = llength left
              rightSize = llength right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff scalar left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff scalar right else right

    _errableBinaryOperation :: BS.ErrableBinaryScalarAction -> BigList -> BigList -> MI.Result BigList
    _errableBinaryOperation operation (BigList leftVals) (BigList rightVals)
        | S.null errors = MI.withValue $ BigList $ fmap MI.value result
        | otherwise = MI.withErrorSet $ HS.unions $ F.toList $ fmap MI.errorSet errors
        where result = S.zipWith operation leftVals rightVals
              errors = S.filter MI.isSuccess result

    _errableBinaryOperationPad :: BS.ErrableBinaryScalarAction -> BS.BigScalar -> BigList -> BigList -> MI.Result BigList
    _errableBinaryOperationPad operation scalar left right = _errableBinaryOperation operation leftPadded rightPadded
        where leftSize = llength left
              rightSize = llength right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff scalar left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff scalar right else right

    _pad :: Int -> BS.BigScalar -> BigList -> BigList
    _pad amount scalar (BigList vals) = BigList $ vals S.>< (S.replicate amount scalar)

    lminimum :: BigList -> MI.Result BS.BigScalar
    lminimum (BigList vals)
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = _lminmaxHelper BS.smin headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    lmaximum :: BigList -> MI.Result BS.BigScalar
    lmaximum (BigList vals)
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = _lminmaxHelper BS.smax headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    _lminmaxHelper :: BS.ErrableBinaryScalarAction -> BS.BigScalar -> S.Seq BS.BigScalar -> MI.Result BS.BigScalar
    _lminmaxHelper func best vals
        | MI.isFailure newBest = MI.withError MI.InvalidValue
        | otherwise = _lminmaxHelper func (MI.value newBest) rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals
              newBest = func best headVal

    _containsNonreal :: S.Seq BS.BigScalar -> Bool
    _containsNonreal seq = or $ fmap (not . BS.sIsReal) seq

    lsum :: BigList -> BS.BigScalar
    lsum (BigList vals) = F.foldr BS.splus BS.zero vals

    lcumsum :: BigList -> BigList
    lcumsum (BigList vals) = BigList $ S.scanl BS.splus headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    lprod :: BigList -> BS.BigScalar
    lprod (BigList vals) = F.foldl' BS.smult BS.one vals

    lcumprod :: BigList -> BigList
    lcumprod (BigList vals) = BigList $ S.scanl BS.smult headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    lmean :: BigList -> MI.Result BS.BigScalar
    lmean list@(BigList vals)
        | 0 == (llength list) = MI.withError MI.InvalidState
        | otherwise = BS.sdiv sumValue listSize
        where listSize = lsize list
              sumValue = lsum list

    lgmean :: BigList -> MI.Result BS.BigScalar
    lgmean list@(BigList vals)
        | 0 == (llength list) = MI.withError MI.InvalidState
        | otherwise = BS.spow prodValue (MI.value $ BS.sinv listSize)
        where listSize = lsize list
              prodValue = lprod list

    lhmean :: BigList -> MI.Result BS.BigScalar
    lhmean list@(BigList vals)
        | 0 == (llength list) = MI.withError MI.InvalidState
        | otherwise = MI.errBinResolveRight listSize invListSum BS.sdiv
        where listSize = lsize list
              invList = _inverseList list
              invListSum = MI.unResolve invList lsum
              
    _inverseList :: BigList -> MI.Result BigList
    _inverseList (BigList vals)
        | M.isNothing fail = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ lseq $ fmap MI.value invList
        where invList = fmap BS.sinv vals
              fail = S.findIndexL MI.isFailure invList

    lmedian :: BigList -> MI.Result BS.BigScalar
    lmedian list@(BigList vals)
        | 0 == listSize = MI.withError MI.InvalidState
        | _containsNonreal vals = MI.withError MI.InvalidType
        | even listSize = BS.sdiv (BS.splus (_quickSelect (halfSize - 1) vals) (_quickSelect halfSize vals)) BS.two
        | otherwise = MI.withValue $ _quickSelect halfSize vals
        where listSize = llength list
              halfSize = div listSize 2

    _quickSelect :: Int -> S.Seq BS.BigScalar -> BS.BigScalar
    _quickSelect index vals
        | index < leftSize = _quickSelect index left
        | index > leftSize = _quickSelect (index - leftSize - 1) right
        | otherwise = headVal
        where headVal = S.index vals 0
              tail = S.drop 1 vals
              (left, right) = S.partition (\other -> MI.value $ BS.sLessEqual other headVal) tail
              leftSize = F.length left

    lrange :: BigList -> MI.Result BS.BigScalar
    lrange list
        | MI.isFailure minValue = MI.withError MI.InvalidValue
        | otherwise = MI.binCombine maxValue minValue BS.sminus
        where minValue = lminimum list
              maxValue = lmaximum list

    lmidrange :: BigList -> MI.Result BS.BigScalar
    lmidrange list
        | MI.isFailure minValue = MI.withError MI.InvalidValue
        | otherwise = MI.errBinResolveLeft minmaxSum BS.two BS.sdiv
        where minValue = lminimum list
              maxValue = lmaximum list
              minmaxSum = MI.binCombine minValue maxValue BS.splus

    lmode :: BigList -> BigList
    lmode (BigList vals)
        | 1 == maxCount = lempty
        | otherwise = llist $ HM.keys modeMap
        where counts = _count vals HM.empty
              maxCount = maximum $ HM.elems counts
              modeMap = HM.filter (==maxCount) counts

    _count :: S.Seq BS.BigScalar -> HM.HashMap BS.BigScalar Int -> HM.HashMap BS.BigScalar Int
    _count vals counter
        | S.null vals = counter
        | M.isNothing mappedVal = _count rest (HM.insert headVal 1 counter)
        | otherwise = _count rest (HM.adjust (+1) headVal counter)
        where headVal = S.index vals 0
              rest = S.drop 1 vals
              mappedVal = HM.lookup headVal counter

    lsortAsc :: BigList -> MI.Result BigList
    lsortAsc (BigList vals) = _lmergeSortHelper1 BS.sLessEqual vals

    lsortDesc :: BigList -> MI.Result BigList
    lsortDesc (BigList vals) = _lmergeSortHelper1 BS.sGreaterEqual vals

    _lmergeSortHelper1 :: BS.ErrableComparisonAction -> S.Seq BS.BigScalar -> MI.Result BigList
    _lmergeSortHelper1 ordFunc vals
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ lseq $ _lmergeSortHelper2 (\left right -> MI.value $ ordFunc left right) vals

    _lmergeSortHelper2 :: BS.ComparisonAction -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar
    _lmergeSortHelper2 ordFunc vals
        | size < 2 = vals
        | otherwise = _merge ordFunc (_lmergeSortHelper2 ordFunc left) (_lmergeSortHelper2 ordFunc right)
        where size = F.length vals
              halfLength = div size 2
              (left, right) = S.splitAt halfLength vals

    _merge :: BS.ComparisonAction -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar
    _merge ordFunc left right
        | S.null left = right
        | S.null right = left
        | ordFunc leftHead rightHead = leftHead S.<| (_merge ordFunc leftRest right)
        | otherwise = rightHead S.<| (_merge ordFunc left rightRest)
        where leftHead = S.index left 0
              rightHead = S.index right 0
              leftRest = S.drop 1 left
              rightRest = S.drop 1 right
    
    lIsSortedAsc :: BigList -> MI.Result Bool
    lIsSortedAsc (BigList vals) = _lIsSortedHelper1 vals BS.sLessEqual

    lIsSortedDesc :: BigList -> MI.Result Bool
    lIsSortedDesc (BigList vals) = _lIsSortedHelper1 vals BS.sGreaterEqual

    _lIsSortedHelper1 :: S.Seq BS.BigScalar -> BS.ErrableComparisonAction -> MI.Result Bool
    _lIsSortedHelper1 vals ordFunc
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ _lIsSortedHelper2 vals (\left right -> MI.value $ ordFunc left right)

    _lIsSortedHelper2 :: S.Seq BS.BigScalar -> BS.ComparisonAction -> Bool
    _lIsSortedHelper2 vals ordFunc
        | (F.length vals) <= 1 = True
        | otherwise = (ordFunc elem next) && (_lIsSortedHelper2 rest ordFunc)
        where elem = S.index vals 0
              next = S.index vals 1
              rest = S.drop 1 vals

    lIsSorted :: BigList -> MI.Result Bool
    lIsSorted list
        | MI.isFailure sortedAsc = MI.convert sortedAsc
        | otherwise = MI.withValue $ (MI.value sortedAsc) || (MI.value sortedDesc)
        where sortedAsc = lIsSortedAsc list
              sortedDesc = lIsSortedDesc list

    lsub :: BigList -> BS.BigScalar -> BS.BigScalar -> MI.Result BigList
    lsub list@(BigList vals) lowIndex highIndex
        | lowIndexInt < 0 || lowIndexInt >= listSize || highIndexInt < 0 || highIndexInt >= listSize || lowIndexInt > highIndexInt = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ lseq sublist
        where listSize = llength list
              lowIndexInt = BS.asBuiltInInt lowIndex
              highIndexInt = BS.asBuiltInInt highIndex
              sublist = S.take (highIndexInt - lowIndexInt) (S.drop lowIndexInt vals)

    lconcat :: BigList -> BigList -> BigList
    lconcat (BigList leftVals) (BigList rightVals) = lseq $ leftVals S.>< rightVals

    lmerge :: BigList -> BigList -> MI.Result BigList
    lmerge left@(BigList leftVals) right@(BigList rightVals)
        | MI.isFailure leftSortAsc = MI.withError MI.InvalidType
        | (MI.value leftSortAsc) && (MI.value rightSortAsc) = MI.withValue $ lseq $ _merge (\leftVal rightVal -> MI.value $ BS.sGreaterEqual leftVal rightVal) leftVals rightVals
        | (MI.value leftSortDesc) && (MI.value rightSortDesc) = MI.withValue $ lseq $ _merge (\leftVal rightVal -> MI.value $ BS.sGreaterEqual leftVal rightVal) leftVals rightVals
        | otherwise = MI.withError MI.InvalidState
        where leftSortAsc = lIsSortedAsc left
              rightSortAsc = lIsSortedAsc right
              leftSortDesc = lIsSortedDesc left
              rightSortDesc = lIsSortedDesc right

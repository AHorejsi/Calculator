{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module BigList (
    BigList,
    lempty,
    llist,
    lseq,
    lrepeat,
    lincrement,
    splusl,
    lpluss,
    lplusPad,
    sminusl,
    lminuss,
    lminusPad,
    smultl,
    lmults,
    lmultPad,
    sdivl,
    ldivs,
    ldivPad,
    spowl,
    lpows,
    lpow,
    lpowPad,
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
    import qualified Actions as A
    import qualified Stringify as Str
    import qualified MathInfo as MI
    import qualified BigScalar as BS

    newtype BigList = BigList {
        _vals :: S.Seq BS.BigScalar
    } deriving (G.Generic, Eq)

    instance H.Hashable BigList where
        hashWithSalt salt (BigList vals) = H.hashWithSalt salt (F.toList vals)

    instance Show BigList where
        show list = _str "List[%s]" list show

    instance Str.Stringifier BigList where
        stringify sets list = _str "{%s}" list (Str.stringify sets)

    _str :: String -> BigList -> MI.UnaryAction BS.BigScalar String -> String
    _str format list converter = TP.printf format strVal
        where strVal = _strHelper list converter

    _strHelper :: BigList -> MI.UnaryAction BS.BigScalar String -> String
    _strHelper (BigList pos) converter = concat commaSeparated
        where stringList = fmap converter pos
              commaSeparated = S.intersperse "," stringList

    lempty :: BigList
    lempty = BigList S.empty

    lseq :: S.Seq BS.BigScalar -> BigList
    lseq = BigList

    llist :: [BS.BigScalar] -> BigList
    llist = lseq . S.fromList

    lrepeat :: BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigList
    lrepeat scalar count
        | not $ BS.isExactInteger count = MI.withError MI.InvalidType
        | intCount < 0 = MI.withError MI.InvalidValue
        | otherwise =  MI.withValue $ _pad intCount scalar lempty
        where intCount = BS.asInt count

    lincrement :: BS.BigScalar -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigList
    lincrement initial increment count
        | not $ BS.isExactInteger count = MI.withError MI.InvalidType
        | intCount < 0 = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigList $ _computeRange initial increment intCount
        where intCount = BS.asInt count
    
    _computeRange :: BS.BigScalar -> BS.BigScalar -> Int -> S.Seq BS.BigScalar
    _computeRange _ _ 0 = S.empty
    _computeRange current increment count = current S.<| next
        where elem = A.unsafePlus current increment
              next = _computeRange elem increment (count - 1)

    instance BS.Sized BigList where
        intSize (BigList vals) = S.length vals
        getInt list@(BigList vals) index
            | index < 0 || index >= size = MI.withError MI.InvalidValue
            | otherwise = MI.withValue $ S.index vals index
            where size = BS.intSize list

    splusl :: BS.BigScalar -> BigList -> BigList
    splusl left = _unaryAction (A.unsafePlus left)

    lpluss :: BigList -> BS.BigScalar -> BigList
    lpluss = flip splusl

    instance A.Addable BigList where
        plus = _binaryAction A.unsafePlus

    lplusPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lplusPad = _binaryActionPad A.unsafePlus

    sminusl :: BS.BigScalar -> BigList -> BigList
    sminusl left = _unaryAction (A.unsafeMinus left)

    lminuss :: BigList -> BS.BigScalar -> BigList
    lminuss left right = _unaryAction ((flip A.unsafeMinus) right) left

    instance A.Subtractable BigList where
        minus = _binaryAction A.unsafeMinus

    lminusPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lminusPad = _binaryActionPad A.unsafeMinus

    smultl :: BS.BigScalar -> BigList -> BigList
    smultl left = _unaryAction (A.unsafeMult left)

    lmults :: BigList -> BS.BigScalar -> BigList
    lmults left right = _unaryAction ((flip A.unsafeMult) right) left

    instance A.Multipliable BigList where
        mult = _binaryAction A.unsafeMult

    lmultPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lmultPad = _binaryActionPad A.unsafeMult

    sdivl :: BS.BigScalar -> BigList -> MI.ComputationResult BigList
    sdivl left = _errableUnaryAction (A.div left)

    ldivs :: BigList -> BS.BigScalar -> MI.ComputationResult BigList
    ldivs left right = _errableUnaryAction ((flip A.div) right) left

    instance A.Divisible BigList where
        div = _errableBinaryAction A.div
        inv = _errableUnaryAction A.inv
    
    ldivPad :: BS.BigScalar -> BigList -> BigList -> MI.ComputationResult BigList
    ldivPad = _errableBinaryActionPad A.div

    spowl :: BS.BigScalar -> BigList -> BigList
    spowl left = _unaryAction (A.unsafePow left)

    lpows :: BigList -> BS.BigScalar -> BigList
    lpows left right = _unaryAction ((flip A.unsafePow) right) left

    lpow :: BigList -> BigList -> MI.ComputationResult BigList
    lpow = _errableBinaryAction A.pow

    lpowPad :: BS.BigScalar -> BigList -> BigList -> MI.ComputationResult BigList
    lpowPad = _errableBinaryActionPad A.pow

    instance A.Negatable BigList where
        unsafeNeg = smultl BS.negOne
        neg = MI.withValue . A.unsafeNeg

    _unaryAction :: BS.UnaryScalarAction -> BigList -> BigList
    _unaryAction action (BigList vals) = BigList $ fmap action vals

    _errableUnaryAction :: BS.ErrableUnaryScalarAction -> BigList -> MI.ComputationResult BigList
    _errableUnaryAction action (BigList vals)
        | S.null errors = (MI.withValue . BigList) $ fmap MI.value result
        | otherwise = MI.withErrorSet $ HS.unions $ F.toList $ fmap MI.errorSet errors
        where result = fmap action vals
              errors = S.filter MI.isSuccess result

    _binaryAction :: BS.BinaryScalarAction -> BigList -> BigList -> MI.ComputationResult BigList
    _binaryAction action left@(BigList leftVals) right@(BigList rightVals)
        | not $ BS.equalSize left right = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigList $ S.zipWith action leftVals rightVals

    _binaryActionPad :: BS.BinaryScalarAction -> BS.BigScalar -> BigList -> BigList -> BigList
    _binaryActionPad action padVal left right = MI.value $ _binaryAction action leftPadded rightPadded
        where leftSize = BS.intSize left
              rightSize = BS.intSize right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff padVal left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff padVal right else right

    _errableBinaryAction :: BS.ErrableBinaryScalarAction -> BigList -> BigList -> MI.ComputationResult BigList
    _errableBinaryAction action (BigList leftVals) (BigList rightVals)
        | S.null errors = (MI.withValue . BigList) $ fmap MI.value result
        | otherwise = (MI.withErrorSet . HS.unions . F.toList) $ fmap MI.errorSet errors
        where result = S.zipWith action leftVals rightVals
              errors = S.filter MI.isSuccess result

    _errableBinaryActionPad :: BS.ErrableBinaryScalarAction -> BS.BigScalar -> BigList -> BigList -> MI.ComputationResult BigList
    _errableBinaryActionPad action padVal left right = _errableBinaryAction action leftPadded rightPadded
        where leftSize = BS.intSize left
              rightSize = BS.intSize right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff padVal left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff padVal right else right

    _pad :: Int -> BS.BigScalar -> BigList -> BigList
    _pad amount padVal (BigList vals) = BigList $ vals S.>< padValList
        where padValList = S.replicate amount padVal

    lminimum :: BigList -> MI.ComputationResult BS.BigScalar
    lminimum (BigList vals)
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = _lminmaxHelper A.min headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    lmaximum :: BigList -> MI.ComputationResult BS.BigScalar
    lmaximum (BigList vals)
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = _lminmaxHelper A.max headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    _lminmaxHelper :: BS.ErrableBinaryScalarAction -> BS.BigScalar -> S.Seq BS.BigScalar -> MI.ComputationResult BS.BigScalar
    _lminmaxHelper func best vals
        | S.null vals = MI.withValue best
        | otherwise = _lminmaxHelper func newBest rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals
              newBest = MI.value $ func best headVal

    _containsNonreal :: S.Seq BS.BigScalar -> Bool
    _containsNonreal seq = or $ fmap (not . BS.sIsReal) seq

    lsum :: BigList -> BS.BigScalar
    lsum (BigList vals) = F.foldr A.unsafePlus BS.zero vals

    lcumsum :: BigList -> BigList
    lcumsum (BigList vals) = BigList $ S.scanl A.unsafePlus headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    lprod :: BigList -> BS.BigScalar
    lprod (BigList vals) = F.foldl' A.unsafeMult BS.one vals

    lcumprod :: BigList -> BigList
    lcumprod (BigList vals) = BigList $ S.scanl A.unsafeMult headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    lmean :: BigList -> MI.ComputationResult BS.BigScalar
    lmean list@(BigList vals)
        | 0 == (BS.intSize list) = MI.withError MI.InvalidState
        | otherwise = A.div sumValue listSize
        where listSize = BS.size list
              sumValue = lsum list

    lgmean :: BigList -> MI.ComputationResult BS.BigScalar
    lgmean list@(BigList vals)
        | 0 == (BS.intSize list) = MI.withError MI.InvalidState
        | otherwise = A.pow prodValue (A.unsafeInv listSize)
        where listSize = BS.size list
              prodValue = lprod list

    lhmean :: BigList -> MI.ComputationResult BS.BigScalar
    lhmean list@(BigList vals)
        | 0 == (BS.intSize list) = MI.withError MI.InvalidState
        | otherwise = MI.errBinResolveRight listSize invListSum A.div
        where listSize = BS.size list
              invList = A.inv list
              invListSum = MI.unResolve invList lsum

    lmedian :: BigList -> MI.ComputationResult BS.BigScalar
    lmedian list@(BigList vals)
        | 0 == listSize = MI.withError MI.InvalidState
        | _containsNonreal vals = MI.withError MI.InvalidType
        | even listSize = A.mult (A.unsafePlus (_quickSelect (halfSize - 1) vals) (_quickSelect halfSize vals)) BS.half
        | otherwise = MI.withValue $ _quickSelect halfSize vals
        where listSize = BS.intSize list
              halfSize = div listSize 2

    _quickSelect :: Int -> S.Seq BS.BigScalar -> BS.BigScalar
    _quickSelect index vals
        | index < leftSize = _quickSelect index left
        | index > leftSize = _quickSelect (index - leftSize - 1) right
        | otherwise = headVal
        where headVal = S.index vals 0
              tail = S.drop 1 vals
              (left, right) = S.partition (\other -> MI.value $ A.lessEqual other headVal) tail
              leftSize = F.length left

    lrange :: BigList -> MI.ComputationResult BS.BigScalar
    lrange list
        | MI.isFailure minValue = MI.withError MI.InvalidValue
        | otherwise = MI.binCombine maxValue minValue A.unsafeMinus
        where minValue = lminimum list
              maxValue = lmaximum list

    lmidrange :: BigList -> MI.ComputationResult BS.BigScalar
    lmidrange list
        | MI.isFailure minValue = MI.withError MI.InvalidValue
        | otherwise = MI.binResolveLeft minmaxSum BS.half A.unsafeMult
        where minValue = lminimum list
              maxValue = lmaximum list
              minmaxSum = MI.binCombine minValue maxValue A.unsafePlus

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

    lsortAsc :: BigList -> MI.ComputationResult BigList
    lsortAsc (BigList vals) = _lmergeSortHelper1 A.lessEqual vals

    lsortDesc :: BigList -> MI.ComputationResult BigList
    lsortDesc (BigList vals) = _lmergeSortHelper1 A.greaterEqual vals

    _lmergeSortHelper1 :: MI.ErrableBinaryPredicate BS.BigScalar BS.BigScalar -> S.Seq BS.BigScalar -> MI.ComputationResult BigList
    _lmergeSortHelper1 ordFunc vals
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ lseq $ _lmergeSortHelper2 (\left right -> MI.value $ ordFunc left right) vals

    _lmergeSortHelper2 :: MI.BinaryPredicate BS.BigScalar BS.BigScalar -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar
    _lmergeSortHelper2 ordFunc vals
        | size < 2 = vals
        | otherwise = _merge ordFunc (_lmergeSortHelper2 ordFunc left) (_lmergeSortHelper2 ordFunc right)
        where size = F.length vals
              halfLength = div size 2
              (left, right) = S.splitAt halfLength vals

    _merge :: MI.BinaryPredicate BS.BigScalar BS.BigScalar -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar
    _merge ordFunc left right
        | S.null left = right
        | S.null right = left
        | ordFunc leftHead rightHead = leftHead S.<| (_merge ordFunc leftRest right)
        | otherwise = rightHead S.<| (_merge ordFunc left rightRest)
        where leftHead = S.index left 0
              rightHead = S.index right 0
              leftRest = S.drop 1 left
              rightRest = S.drop 1 right
    
    lIsSortedAsc :: BigList -> MI.ComputationResult Bool
    lIsSortedAsc (BigList vals) = _lIsSortedHelper1 vals A.lessEqual

    lIsSortedDesc :: BigList -> MI.ComputationResult Bool
    lIsSortedDesc (BigList vals) = _lIsSortedHelper1 vals A.greaterEqual

    _lIsSortedHelper1 :: S.Seq BS.BigScalar -> MI.ErrableBinaryPredicate BS.BigScalar BS.BigScalar -> MI.ComputationResult Bool
    _lIsSortedHelper1 vals ordFunc
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ _lIsSortedHelper2 vals (\left right -> MI.value $ ordFunc left right)

    _lIsSortedHelper2 :: S.Seq BS.BigScalar -> MI.BinaryPredicate BS.BigScalar BS.BigScalar -> Bool
    _lIsSortedHelper2 vals ordFunc
        | (F.length vals) <= 1 = True
        | otherwise = (ordFunc elem next) && (_lIsSortedHelper2 rest ordFunc)
        where elem = S.index vals 0
              next = S.index vals 1
              rest = S.drop 1 vals

    lIsSorted :: BigList -> MI.ComputationResult Bool
    lIsSorted list
        | MI.isFailure sortedAsc = MI.convert sortedAsc
        | otherwise = MI.withValue $ (MI.value sortedAsc) || (MI.value sortedDesc)
        where sortedAsc = lIsSortedAsc list
              sortedDesc = lIsSortedDesc list

    lsub :: BigList -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigList
    lsub list@(BigList vals) lowIndex highIndex
        | lowIndexInt < 0 || lowIndexInt >= listSize || highIndexInt < 0 || highIndexInt >= listSize || lowIndexInt > highIndexInt = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ lseq sublist
        where listSize = BS.intSize list
              lowIndexInt = BS.asInt lowIndex
              highIndexInt = BS.asInt highIndex
              sublist = S.take (highIndexInt - lowIndexInt) (S.drop lowIndexInt vals)

    lconcat :: BigList -> BigList -> BigList
    lconcat (BigList leftVals) (BigList rightVals) = lseq $ leftVals S.>< rightVals

    lmerge :: BigList -> BigList -> MI.ComputationResult BigList
    lmerge left@(BigList leftVals) right@(BigList rightVals)
        | MI.isFailure leftSortAsc = MI.withError MI.InvalidType
        | (MI.value leftSortAsc) && (MI.value rightSortAsc) = MI.withValue $ lseq $ _merge (\leftVal rightVal -> MI.value $ A.greaterEqual leftVal rightVal) leftVals rightVals
        | (MI.value leftSortDesc) && (MI.value rightSortDesc) = MI.withValue $ lseq $ _merge (\leftVal rightVal -> MI.value $ A.greaterEqual leftVal rightVal) leftVals rightVals
        | otherwise = MI.withError MI.InvalidState
        where leftSortAsc = lIsSortedAsc left
              rightSortAsc = lIsSortedAsc right
              leftSortDesc = lIsSortedDesc left
              rightSortDesc = lIsSortedDesc right

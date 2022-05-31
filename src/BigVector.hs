{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module BigVector (
    BigVector,
    fromList,
    fromSeq,
    toList,
    toSeq,
    empty,
    isNull,
    length1,
    size1,
    equalSize1,
    getInt1,
    get1,
    dot,
    cross,
    dist,
    angle,
    findMin,
    findMax,
    mean,
    gmean,
    hmean,
    median,
    range,
    midrange,
    mode,
    sum,
    cumsum,
    prod,
    cumprod,
    sortAsc,
    sortDesc,
    isSortedAsc,
    isSortedDesc,
    isSorted,
    sub1,
    concat,
    merge
) where
    import Prelude hiding (sum, concat)
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Sequence as S
    import qualified Data.Foldable as F
    import qualified Data.Hashable as H
    import qualified Data.HashMap.Lazy as HM
    import qualified Actions as A
    import qualified Stringify as Str
    import qualified MathInfo as MI
    import qualified BigScalar as BS
    
    newtype BigVector = BigVector {
        _pos :: S.Seq BS.BigScalar
    } deriving (G.Generic, Eq)

    instance H.Hashable BigVector where
        hashWithSalt salt (BigVector pos) = H.hashWithSalt salt (F.toList pos)

    instance Show BigVector where
        show vec = _str "Vector[%s]" vec show

    instance Str.Stringifier BigVector where
        stringify sets vec = _str "<%s>" vec (Str.stringify sets)

    _str :: String -> BigVector -> MI.UnaryAction BS.BigScalar String -> String
    _str format vec converter = TP.printf format strVal
        where strVal = _strHelper vec converter

    _strHelper :: BigVector -> MI.UnaryAction BS.BigScalar String -> String
    _strHelper (BigVector pos) converter = F.foldl' (++) "" commaSeparated
        where stringList = fmap converter pos
              commaSeparated = S.intersperse "," stringList

    fromList :: [BS.BigScalar] -> BigVector
    fromList list = BigVector $ S.fromList list

    fromSeq :: S.Seq BS.BigScalar -> BigVector
    fromSeq = BigVector

    toList :: BigVector -> [BS.BigScalar]
    toList (BigVector pos) = F.toList pos

    toSeq :: BigVector -> S.Seq BS.BigScalar
    toSeq (BigVector pos) = pos

    empty :: BigVector
    empty = BigVector S.Empty

    isNull :: BigVector -> Bool
    isNull (BigVector pos) = F.all (==BS.zero) pos

    length1 :: BigVector -> Int
    length1 (BigVector pos) = S.length pos

    size1 :: BigVector -> BS.BigScalar
    size1 = BS.integral . length1

    equalSize1 :: BigVector -> BigVector -> Bool
    equalSize1 left right = (length1 left) == (length1 right)

    getInt1 :: BigVector -> Int -> MI.ComputationResult BS.BigScalar
    getInt1 vec@(BigVector pos) index
        | index < 0 || index >= size = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ S.index pos index
        where size = length1 vec

    get1 :: BigVector -> BS.BigScalar -> MI.ComputationResult BS.BigScalar
    get1 vec index
        | not $ BS.isExactInteger index = MI.withError MI.InvalidValue
        | otherwise = getInt1 vec intIndex
        where intIndex = BS.asInt index        

    instance A.Addable BigVector where
        plus = _binaryAction A.unsafePlus

    instance A.Subtractable BigVector where
        minus = _binaryAction A.unsafeMinus

    instance BS.ScalarMultipliable BigVector where
        rightScalarMult left right = MI.withValue $ BS.unsafeRightScalarMult left right
        leftScalarMult left right = MI.withValue $ BS.unsafeLeftScalarMult left right
        unsafeRightScalarMult left (BigVector rightPos) = fromSeq $ fmap (A.unsafeMult left) rightPos
        unsafeLeftScalarMult (BigVector leftPos) right = fromSeq $ fmap (`A.unsafeMult` right) leftPos

    instance A.Scalable BigVector where
        scale = _binaryAction A.unsafeMult

    _binaryAction :: BS.BinaryScalarAction -> BigVector -> BigVector -> MI.ComputationResult BigVector
    _binaryAction action left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ equalSize1 left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . fromSeq) $ S.zipWith action leftPos rightPos

    dot :: BigVector -> BigVector -> MI.ComputationResult BS.BigScalar
    dot left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ equalSize1 left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . _sum) $ S.zipWith A.unsafeMult leftPos rightPos

    _sum :: S.Seq BS.BigScalar -> BS.BigScalar
    _sum = F.foldr A.unsafePlus BS.zero

    cross :: BigVector -> BigVector -> MI.ComputationResult BigVector
    cross left right
        | 3 == leftSize && leftSize == rightSize = MI.withValue $ fromList [resultXPos, resultYPos, resultZPos]
        | otherwise = MI.withError MI.InvalidValue
        where leftSize = length1 left
              rightSize = length1 right
              leftXPos = MI.value $ getInt1 left 0
              leftYPos = MI.value $ getInt1 left 1
              leftZPos = MI.value $ getInt1 left 2
              rightXPos = MI.value $ getInt1 right 0
              rightYPos = MI.value $ getInt1 right 1
              rightZPos = MI.value $ getInt1 right 2
              resultXPos = A.unsafeMinus (A.unsafeMult leftYPos rightZPos) (A.unsafeMult leftZPos rightYPos)
              resultYPos = A.unsafeMinus (A.unsafeMult leftZPos rightXPos) (A.unsafeMult leftXPos rightZPos)
              resultZPos = A.unsafeMinus (A.unsafeMult leftXPos rightYPos) (A.unsafeMult leftYPos rightXPos)

    instance A.Negatable BigVector where
        neg = BS.rightScalarMult BS.negOne

    instance BS.Graphable BigVector where
        absol = MI.withValue . BS.unsafeAbsol
        unsafeAbsol vec = (A.unsafeSqrt . MI.value) $ dot vec vec
        norm vec
            | isNull vec = MI.withError MI.InvalidValue
            | otherwise = BS.leftScalarMult vec (A.unsafeDiv BS.one (BS.unsafeAbsol vec))

    findMin :: BigVector -> MI.ComputationResult BS.BigScalar
    findMin (BigVector S.Empty) = MI.withError MI.InvalidState
    findMin (BigVector vals) = _findMinMax A.min headVal vals
        where headVal = S.index vals 0

    findMax :: BigVector -> MI.ComputationResult BS.BigScalar
    findMax (BigVector S.Empty) = MI.withError MI.InvalidState
    findMax (BigVector vals) = _findMinMax A.max headVal vals
        where headVal = S.index vals 0

    _findMinMax :: BS.ErrableBinaryScalarAction -> BS.BigScalar -> S.Seq BS.BigScalar -> MI.ComputationResult BS.BigScalar
    _findMinMax action best S.Empty = MI.withValue best
    _findMinMax action best (val S.:<| vals) = MI.errBinCombine result next action
        where result = action best val
              next = _findMinMax action (MI.value result) vals

    sum :: BigVector -> BS.BigScalar
    sum (BigVector vals) = _sum vals

    cumsum :: BigVector -> BigVector
    cumsum (BigVector S.Empty) = empty
    cumsum (BigVector vals) = fromSeq $ S.scanl A.unsafePlus headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    prod :: BigVector -> BS.BigScalar
    prod (BigVector vals) = F.foldl' A.unsafeMult BS.one vals

    cumprod :: BigVector -> BigVector
    cumprod (BigVector S.Empty) = empty
    cumprod (BigVector vals) = fromSeq $ S.scanl A.unsafeMult headVal rest
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    mean :: BigVector -> MI.ComputationResult BS.BigScalar
    mean (BigVector S.Empty) = MI.withError MI.InvalidState
    mean list = A.div sumValue listSize
        where listSize = size1 list
              sumValue = sum list

    gmean :: BigVector -> MI.ComputationResult BS.BigScalar
    gmean (BigVector S.Empty) = MI.withError MI.InvalidState
    gmean list = A.pow prodValue (A.unsafeInv listSize)
        where listSize = size1 list
              prodValue = prod list

    hmean :: BigVector -> MI.ComputationResult BS.BigScalar
    hmean (BigVector S.Empty) = MI.withError MI.InvalidState
    hmean list@(BigVector vals)
        | F.elem BS.zero vals = MI.withError MI.InvalidValue
        | otherwise = A.div listSize invListSum 
        where listSize = size1 list
              invList = fromSeq $ fmap A.unsafeInv vals
              invListSum = sum invList

    median :: BigVector -> MI.ComputationResult BS.BigScalar
    median list@(BigVector vals)
        | 0 == listSize = MI.withError MI.InvalidState
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | even listSize = let first = _quickSelect (halfSize - 1) vals
                              second = _quickSelect halfSize vals
                          in  A.mult (A.unsafePlus first second) BS.half
        | otherwise = MI.withValue $ _quickSelect halfSize vals
        where listSize = length1 list
              halfSize = div listSize 2

    _containsNonreal :: S.Seq BS.BigScalar -> Bool
    _containsNonreal seq = or $ fmap (not . BS.sIsReal) seq

    _quickSelect :: Int -> S.Seq BS.BigScalar -> BS.BigScalar
    _quickSelect index vals
        | index < leftSize = _quickSelect index left
        | index > leftSize = _quickSelect (index - leftSize - 1) right
        | otherwise = headVal
        where headVal = S.index vals 0
              tail = S.drop 1 vals
              (left, right) = S.partition (`A.unsafeLessEqual` headVal) tail
              leftSize = S.length left

    range :: BigVector -> MI.ComputationResult BS.BigScalar
    range (BigVector S.Empty) = MI.withError MI.InvalidState
    range list = MI.binCombine maxValue minValue A.unsafeMinus
        where minValue = findMin list
              maxValue = findMax list

    midrange :: BigVector -> MI.ComputationResult BS.BigScalar
    midrange (BigVector S.Empty) = MI.withError MI.InvalidState
    midrange list = MI.binResolveLeft minmaxSum BS.half A.unsafeMult
        where minValue = findMin list
              maxValue = findMax list
              minmaxSum = MI.binCombine minValue maxValue A.unsafePlus
              
    mode :: BigVector -> MI.ComputationResult BigVector
    mode (BigVector vals)
        | 1 == maxCount = MI.withValue empty
        | otherwise = (MI.withValue . fromList) $ HM.keys modeMap
        where counts = _count vals HM.empty
              maxCount = maximum $ HM.elems counts
              modeMap = HM.filter (==maxCount) counts

    _count :: S.Seq BS.BigScalar -> HM.HashMap BS.BigScalar Int -> HM.HashMap BS.BigScalar Int
    _count S.Empty counter = counter
    _count vals counter
        | HM.member headVal counter = _count rest (HM.insert headVal 1 counter)
        | otherwise = _count rest (HM.adjust (+1) headVal counter)
        where headVal = S.index vals 0
              rest = S.drop 1 vals

    sortAsc :: BigVector -> MI.ComputationResult BigVector
    sortAsc (BigVector vals)
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . fromSeq) $ _sortHelper A.unsafeLessEqual vals

    sortDesc :: BigVector -> MI.ComputationResult BigVector
    sortDesc (BigVector vals)
        | _containsNonreal vals = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . fromSeq) $ _sortHelper A.unsafeGreaterEqual vals

    _sortHelper :: MI.BinaryPredicate BS.BigScalar BS.BigScalar -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar
    _sortHelper ordFunc vals
        | size <= 1 = vals
        | otherwise = _merge ordFunc leftSorted rightSorted
        where size = S.length vals
              halfLength = div size 2
              (left, right) = S.splitAt halfLength vals
              leftSorted = _sortHelper ordFunc left
              rightSorted = _sortHelper ordFunc right

    _merge :: MI.BinaryPredicate BS.BigScalar BS.BigScalar -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar -> S.Seq BS.BigScalar
    _merge _ S.Empty right = right
    _merge _ left S.Empty = left
    _merge ordFunc left right
        | ordFunc leftHead rightHead = leftHead S.<| (_merge ordFunc leftRest right)
        | otherwise = rightHead S.<| (_merge ordFunc left rightRest)
        where leftHead = S.index left 0
              rightHead = S.index right 0
              leftRest = S.drop 1 left
              rightRest = S.drop 1 right

    isSortedAsc :: BigVector -> MI.ComputationResult Bool
    isSortedAsc (BigVector vals) = _isSortedHelper A.lessEqual vals

    isSortedDesc :: BigVector -> MI.ComputationResult Bool
    isSortedDesc (BigVector vals) = _isSortedHelper A.greaterEqual vals

    _isSortedHelper :: MI.ErrableBinaryPredicate BS.BigScalar BS.BigScalar -> S.Seq BS.BigScalar -> MI.ComputationResult Bool
    _isSortedHelper comp vals
        | S.length vals <= 1 = MI.withValue True
        | otherwise = MI.binCombine result next (&&)
        where firstVal = S.index vals 0
              secondVal = S.index vals 1
              result = comp firstVal secondVal
              rest = S.drop 1 vals
              next = _isSortedHelper comp rest

    isSorted :: BigVector -> MI.ComputationResult Bool
    isSorted vec = MI.binCombine ascSort descSort (||)
        where ascSort = isSortedAsc vec
              descSort = isSortedDesc vec

    dist :: BigVector -> BigVector -> MI.ComputationResult BS.BigScalar
    dist left right
        | not $ equalSize1 left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . A.unsafeSqrt . _sum) $ fmap square (_pos subtValue)
        where subtValue = A.unsafeMinus left right
              square = MI.value . ((flip A.pow) BS.two)

    angle :: BigVector -> BigVector -> MI.ComputationResult BS.BigScalar
    angle left right
        | (not $ equalSize1 left right) || (isNull left) || (isNull right) = MI.withError MI.InvalidValue
        | otherwise = BS.sacos $ A.unsafeDiv dotProd absProd
        where dotProd = MI.value $ dot left right
              absProd = A.unsafeMult (BS.unsafeAbsol left) (BS.unsafeAbsol right)

    sub1 :: BigVector -> BS.BigScalar -> BS.BigScalar -> MI.ComputationResult BigVector
    sub1 vec@(BigVector vals) lowIndex highIndex
        | lowIndexInt < 0 || lowIndexInt >= vecLength || highIndexInt < 0 || highIndexInt >= vecLength || lowIndexInt > highIndexInt = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ fromSeq sublist
        where vecLength = length1 vec
              lowIndexInt = BS.asInt lowIndex
              highIndexInt = BS.asInt highIndex
              sublist = S.take (highIndexInt - lowIndexInt) (S.drop lowIndexInt vals)

    concat :: BigVector -> BigVector -> BigVector
    concat (BigVector leftVals) (BigVector rightVals) = BigVector $ leftVals S.>< rightVals

    merge :: BigVector -> BigVector -> MI.ComputationResult BigVector
    merge left@(BigVector leftVals) right@(BigVector rightVals)
        | MI.isFailure leftSortAsc = MI.withError MI.InvalidType
        | (MI.value leftSortAsc) && (MI.value rightSortAsc) = (MI.withValue . fromSeq) $ _merge A.unsafeLessEqual leftVals rightVals
        | (MI.value leftSortDesc) && (MI.value rightSortDesc) = (MI.withValue . fromSeq) $ _merge A.unsafeGreaterEqual leftVals rightVals
        | otherwise = MI.withError MI.InvalidState
        where leftSortAsc = isSortedAsc left
              rightSortAsc = isSortedAsc right
              leftSortDesc = isSortedDesc left
              rightSortDesc = isSortedDesc right

{-# LANGUAGE DeriveGeneric #-}

module BigList (
    BigList,
    lempty,
    llist,
    lvec,
    lrepeat,
    lincrement,
    lsize,
    lequalSize,
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
    lmedian,
    lrange,
    lmidrange,
    lmode,
    lsortAsc,
    lsortDesc,
    (==),
    (/=),
    show
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Maybe as M
    import qualified Data.Vector as V
    import qualified Data.Hashable as H
    import qualified Data.HashSet as HS
    import qualified Data.HashMap.Lazy as HM
    import qualified MathInfo as MI
    import qualified BigScalar as BS

    newtype BigList = BigList {
        _vals :: V.Vector BS.BigScalar
    } deriving (G.Generic, Eq)

    instance H.Hashable BigList where
        hashWithSalt salt (BigList vals) = H.hashWithSalt salt (V.toList vals)

    instance Show BigList where
        show (BigList vals) = TP.printf "List[%s]" (_str vals)

    _str :: V.Vector BS.BigScalar -> String
    _str vals = case V.length vals of 0 -> ""
                                      1 -> TP.printf "%s" headVal
                                      _ -> TP.printf "%s, %s" headVal (_str rest)
        where headVal = show $ V.head vals
              rest = V.tail vals

    lempty :: BigList
    lempty = BigList V.empty

    lvec :: V.Vector BS.BigScalar -> BigList
    lvec = BigList

    llist :: [BS.BigScalar] -> BigList
    llist = BigList . V.fromList

    lrepeat :: BS.BigScalar -> Int -> BigList
    lrepeat scalar count = _pad count scalar lempty

    lincrement :: BS.BigScalar -> BS.BigScalar -> Int -> BigList
    lincrement initial increment count = BigList $ _computeRange initial increment count
    
    _computeRange :: BS.BigScalar -> BS.BigScalar -> Int -> V.Vector BS.BigScalar
    _computeRange _ _ 0 = V.empty
    _computeRange current increment count = V.cons current (_computeRange next increment (count - 1))
        where next = BS.splus current increment

    lsize :: BigList -> Int
    lsize (BigList vals) = V.length vals

    lequalSize :: BigList -> BigList -> Bool
    lequalSize left right = (lsize left) == (lsize right)

    lget :: BigList -> Int -> MI.MathResult BS.BigScalar
    lget list@(BigList vals) index
        | M.isNothing gotten = MI.withError MI.InvalidIndex
        | otherwise = MI.withValue $ M.fromJust gotten
        where gotten = vals V.!? index

    splusl :: BS.BigScalar -> BigList -> BigList
    splusl left = _unaryOperation (BS.splus left)

    lpluss :: BigList -> BS.BigScalar -> BigList
    lpluss = flip splusl

    lplus :: BigList -> BigList -> MI.MathResult BigList
    lplus = _binaryOperation BS.splus

    lplusPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lplusPad = _binaryOperationPad BS.splus

    sminusl :: BS.BigScalar -> BigList -> BigList
    sminusl left = _unaryOperation (BS.sminus left)

    lminuss :: BigList -> BS.BigScalar -> BigList
    lminuss left right = _unaryOperation ((flip BS.sminus) right) left

    lminus :: BigList -> BigList -> MI.MathResult BigList
    lminus = _binaryOperation BS.sminus

    lminusPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lminusPad = _binaryOperationPad BS.sminus

    smultl :: BS.BigScalar -> BigList -> BigList
    smultl left = _unaryOperation (BS.smult left)

    lmults :: BigList -> BS.BigScalar -> BigList
    lmults left right = _unaryOperation ((flip BS.smult) right) left

    lmult :: BigList -> BigList -> MI.MathResult BigList
    lmult = _binaryOperation BS.smult

    lmultPad :: BS.BigScalar -> BigList -> BigList -> BigList
    lmultPad = _binaryOperationPad BS.smult

    sdivl :: BS.BigScalar -> BigList -> MI.MathResult BigList
    sdivl left = _errableUnaryOperation (BS.sdiv left)

    ldivs :: BigList -> BS.BigScalar -> MI.MathResult BigList
    ldivs left right = _errableUnaryOperation ((flip BS.sdiv) right) left

    ldiv :: BigList -> BigList -> MI.MathResult BigList
    ldiv = _errableBinaryOperation BS.sdiv
    
    ldivPad :: BS.BigScalar -> BigList -> BigList -> MI.MathResult BigList
    ldivPad = _errableBinaryOperationPad BS.sdiv

    spowl :: BS.BigScalar -> BigList -> MI.MathResult BigList
    spowl left = _errableUnaryOperation (BS.spow left)

    lpows :: BigList -> BS.BigScalar -> MI.MathResult BigList
    lpows left right = _errableUnaryOperation ((flip BS.spow) right) left

    lpow :: BigList -> BigList -> MI.MathResult BigList
    lpow = _errableBinaryOperation BS.spow

    lpowPad :: BS.BigScalar -> BigList -> BigList -> MI.MathResult BigList
    lpowPad = _errableBinaryOperationPad BS.spow

    lneg :: BigList -> BigList
    lneg = smultl BS.negOne

    _unaryOperation :: BS.UnaryScalarOperation -> BigList -> BigList
    _unaryOperation operation (BigList vals) = BigList $ V.map operation vals

    _errableUnaryOperation :: BS.ErrableUnaryScalarOperation -> BigList -> MI.MathResult BigList
    _errableUnaryOperation operation (BigList vals)
        | V.null errors = MI.withValue $ BigList $ V.map MI.value result
        | otherwise = MI.withErrorSet $ HS.unions $ V.toList $ V.map MI.errorSet errors
        where result = V.map operation vals
              errors = V.filter MI.isSuccess result

    _binaryOperation :: BS.BinaryScalarOperation -> BigList -> BigList -> MI.MathResult BigList
    _binaryOperation operation left@(BigList leftVals) right@(BigList rightVals)
        | not $ lequalSize left right = MI.withError MI.UnequalLength
        | otherwise = MI.withValue $ BigList $ V.zipWith operation leftVals rightVals

    _binaryOperationPad :: BS.BinaryScalarOperation -> BS.BigScalar -> BigList -> BigList -> BigList
    _binaryOperationPad operation scalar left right = MI.value $ _binaryOperation operation leftPadded rightPadded
        where leftSize = lsize left
              rightSize = lsize right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff scalar left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff scalar right else right

    _errableBinaryOperation :: BS.ErrableBinaryScalarOperation -> BigList -> BigList -> MI.MathResult BigList
    _errableBinaryOperation operation (BigList leftVals) (BigList rightVals)
        | V.null errors = MI.withValue $ BigList $ V.map MI.value result
        | otherwise = MI.withErrorSet $ HS.unions $ V.toList $ V.map MI.errorSet errors
        where result = V.zipWith operation leftVals rightVals
              errors = V.filter MI.isSuccess result

    _errableBinaryOperationPad :: BS.ErrableBinaryScalarOperation -> BS.BigScalar -> BigList -> BigList -> MI.MathResult BigList
    _errableBinaryOperationPad operation scalar left right = _errableBinaryOperation operation leftPadded rightPadded
        where leftSize = lsize left
              rightSize = lsize right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff scalar left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff scalar right else right

    _pad :: Int -> BS.BigScalar -> BigList -> BigList
    _pad amount scalar (BigList vals) = BigList $ V.concat [vals, V.replicate amount scalar]

    lminimum :: BigList -> MI.MathResult BS.BigScalar
    lminimum (BigList vals)
        | _containsNonreal vals = MI.withError MI.NoncomparableType
        | otherwise = _lminmaxHelper BS.smin (V.head vals) (V.tail vals)

    lmaximum :: BigList -> MI.MathResult BS.BigScalar
    lmaximum (BigList vals)
        | _containsNonreal vals = MI.withError MI.NoncomparableType
        | otherwise = _lminmaxHelper BS.smax (V.head vals) (V.tail vals)

    _lminmaxHelper :: BS.ErrableBinaryScalarOperation -> BS.BigScalar -> V.Vector BS.BigScalar -> MI.MathResult BS.BigScalar
    _lminmaxHelper func currentBest rest
        | MI.isFailure newBest = MI.withError MI.NoncomparableType
        | otherwise = _lminmaxHelper func (MI.value newBest) nextRest
        where headVal = V.head rest
              nextRest = V.tail rest
              newBest = func currentBest headVal

    _containsNonreal :: V.Vector BS.BigScalar -> Bool
    _containsNonreal = V.any $ not . BS.isReal

    lsum :: BigList -> BS.BigScalar
    lsum (BigList vals) = V.foldr BS.splus BS.zero vals

    lcumsum :: BigList -> BigList
    lcumsum (BigList vals) = BigList $ V.scanl BS.splus (V.head vals) (V.tail vals)

    lprod :: BigList -> BS.BigScalar
    lprod (BigList vals) = V.foldr BS.smult BS.one vals

    lcumprod :: BigList -> BigList
    lcumprod (BigList vals) = BigList $ V.scanl BS.smult (V.head vals) (V.tail vals)

    lmean :: BigList -> MI.MathResult BS.BigScalar
    lmean list@(BigList vals)
        | 0 == listSize = MI.withError MI.ZeroLength
        | otherwise = BS.sdiv sumValue (BS.integral listSize)
        where listSize = lsize list
              sumValue = lsum list

    lgmean :: BigList -> MI.MathResult BS.BigScalar
    lgmean list@(BigList vals)
        | 0 == listSize = MI.withError MI.ZeroLength
        | otherwise = BS.spow prodValue (MI.value $ BS.sinv $ BS.integral listSize)
        where listSize = lsize list
              prodValue = lprod list

    lmedian :: BigList -> MI.MathResult BS.BigScalar
    lmedian list@(BigList vals)
        | 0 == listSize = MI.withError MI.ZeroLength
        | _containsNonreal vals = MI.withError MI.InvalidType
        | even listSize = BS.sdiv (BS.splus (_quickSelect (halfSize - 1) vals) (_quickSelect halfSize vals)) BS.two
        | otherwise = MI.withValue $ _quickSelect halfSize vals
        where listSize = lsize list
              halfSize = ceiling $ (fromIntegral listSize) / 2

    _quickSelect :: Int -> V.Vector BS.BigScalar -> BS.BigScalar
    _quickSelect index vals
        | index < leftSize = _quickSelect index left
        | index > leftSize = _quickSelect (index - leftSize - 1) right
        | otherwise = headVal
        where headVal = V.head vals
              rest = V.tail vals
              (left, right) = V.partition (\other -> LT == (MI.value $ BS.scompare other headVal)) rest
              leftSize = V.length left

    lrange :: BigList -> MI.MathResult BS.BigScalar
    lrange list
        | MI.isFailure minValue = MI.withError MI.NoncomparableType
        | otherwise = MI.binCombine maxValue minValue BS.sminus
        where minValue = lminimum list
              maxValue = lmaximum list

    lmidrange :: BigList -> MI.MathResult BS.BigScalar
    lmidrange list
        | MI.isFailure minValue = MI.withError MI.NoncomparableType
        | otherwise = MI.errBinResolveLeft minmaxSum BS.two BS.sdiv
        where minValue = lminimum list
              maxValue = lmaximum list
              minmaxSum = MI.binCombine minValue maxValue BS.splus

    lmode :: BigList -> BigList
    lmode (BigList vals) = llist $ HM.keys modeMap
        where counts = _count vals HM.empty
              maxCount = maximum $ HM.elems counts
              modeMap = HM.filter (==maxCount) counts

    _count :: V.Vector BS.BigScalar -> HM.HashMap BS.BigScalar Int -> HM.HashMap BS.BigScalar Int
    _count vals counter
        | 0 == vecLength = counter
        | M.isNothing mappedVal = _count rest (HM.insert headVal 1 counter)
        | otherwise = _count rest (HM.adjust (+1) headVal counter)
        where vecLength = V.length vals
              headVal = V.head vals
              rest = V.tail vals
              mappedVal = HM.lookup headVal counter

    lsortAsc :: BigList -> MI.MathResult BigList
    lsortAsc (BigList vals) = _lsort BS.sLessEqual vals

    lsortDesc :: BigList -> MI.MathResult BigList
    lsortDesc (BigList vals) = _lsort BS.sGreaterEqual vals

    _lsort :: (BS.BigScalar -> BS.BigScalar -> MI.MathResult Bool) -> V.Vector BS.BigScalar -> MI.MathResult BigList
    _lsort ordFunc vals
        | _containsNonreal vals = MI.withError MI.InvalidType
        | otherwise = MI.withValue $ lvec $ _lsortHelper (\left right -> MI.value $ ordFunc left right) vals

    _lsortHelper :: (BS.BigScalar -> BS.BigScalar -> Bool) -> V.Vector BS.BigScalar -> V.Vector BS.BigScalar
    _lsortHelper ordFunc vals = V.empty
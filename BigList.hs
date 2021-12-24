module BigList (
    BigList,
    empty,
    list,
    vec,
    repeat,
    range,
    size,
    equalSize,
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
    (==),
    (/=),
    show
) where
    import Prelude hiding (repeat)
    import Text.Printf
    import Data.Maybe
    import qualified Data.Vector as V
    import qualified Data.HashSet as HS
    import qualified Data.HashMap.Lazy as HM
    import MathInfo
    import BigScalar

    newtype BigList = BigList {
        _vals :: V.Vector BigScalar
    } deriving (Eq)

    instance Show BigList where
        show (BigList vals) = printf "List[%s]" (_str vals)

    _str :: V.Vector BigScalar -> String
    _str vals = case V.length vals of 0 -> ""
                                      1 -> printf "%s" headVal
                                      _ -> printf "%s, %s" headVal (_str rest)
        where headVal = show $ V.head vals
              rest = V.tail vals

    empty :: BigList
    empty = BigList V.empty

    vec :: V.Vector BigScalar -> BigList
    vec = BigList

    list :: [BigScalar] -> BigList
    list scalars = BigList $ V.fromList scalars

    repeat :: BigScalar -> Int -> BigList
    repeat scalar count = _pad count scalar empty

    range :: BigScalar -> BigScalar -> Int -> BigList
    range initial increment count = BigList $ _computeRange initial increment count
    
    _computeRange :: BigScalar -> BigScalar -> Int -> V.Vector BigScalar
    _computeRange _ _ 0 = V.empty
    _computeRange current increment count = V.cons current (_computeRange next increment (count - 1))
        where next = splus current increment

    size :: BigList -> Int
    size (BigList vals) = V.length vals

    equalSize :: BigList -> BigList -> Bool
    equalSize left right = (size left) == (size right)

    lget :: BigList -> Int -> MathResult BigScalar
    lget list@(BigList vals) index
        | isNothing gotten = withError InvalidIndex
        | otherwise = withValue $ fromJust gotten
        where gotten = vals V.!? index

    splusl :: BigScalar -> BigList -> BigList
    splusl left = _unaryOperation (splus left)

    lpluss :: BigList -> BigScalar -> BigList
    lpluss = flip splusl

    lplus :: BigList -> BigList -> MathResult BigList
    lplus = _binaryOperation splus

    lplusPad :: BigScalar -> BigList -> BigList -> BigList
    lplusPad = _binaryOperationPad splus

    sminusl :: BigScalar -> BigList -> BigList
    sminusl left = _unaryOperation (sminus left)

    lminuss :: BigList -> BigScalar -> BigList
    lminuss left right = _unaryOperation ((flip sminus) right) left

    lminus :: BigList -> BigList -> MathResult BigList
    lminus = _binaryOperation sminus

    lminusPad :: BigScalar -> BigList -> BigList -> BigList
    lminusPad = _binaryOperationPad sminus

    smultl :: BigScalar -> BigList -> BigList
    smultl left = _unaryOperation (smult left)

    lmults :: BigList -> BigScalar -> BigList
    lmults left right = _unaryOperation ((flip smult) right) left

    lmult :: BigList -> BigList -> MathResult BigList
    lmult = _binaryOperation smult

    lmultPad :: BigScalar -> BigList -> BigList -> BigList
    lmultPad = _binaryOperationPad smult

    sdivl :: BigScalar -> BigList -> MathResult BigList
    sdivl left = _errableUnaryOperation (sdiv left)

    ldivs :: BigList -> BigScalar -> MathResult BigList
    ldivs left right = _errableUnaryOperation ((flip sdiv) right) left

    ldiv :: BigList -> BigList -> MathResult BigList
    ldiv = _errableBinaryOperation sdiv
    
    ldivPad :: BigScalar -> BigList -> BigList -> MathResult BigList
    ldivPad = _errableBinaryOperationPad sdiv

    spowl :: BigScalar -> BigList -> MathResult BigList
    spowl left = _errableUnaryOperation (spow left)

    lpows :: BigList -> BigScalar -> MathResult BigList
    lpows left right = _errableUnaryOperation ((flip spow) right) left

    lpow :: BigList -> BigList -> MathResult BigList
    lpow = _errableBinaryOperation spow

    lpowPad :: BigScalar -> BigList -> BigList -> MathResult BigList
    lpowPad = _errableBinaryOperationPad spow

    lneg :: BigList -> BigList
    lneg = smultl negOne

    _unaryOperation :: UnaryScalarOperation -> BigList -> BigList
    _unaryOperation operation (BigList vals) = BigList $ V.map operation vals

    _errableUnaryOperation :: ErrableUnaryScalarOperation -> BigList -> MathResult BigList
    _errableUnaryOperation operation (BigList vals)
        | V.null errors = withValue $ BigList $ V.map value result
        | otherwise = withErrorSet $ HS.unions $ V.toList $ V.map errorSet errors
        where result = V.map operation vals
              errors = V.filter isSuccess result

    _binaryOperation :: BinaryScalarOperation -> BigList -> BigList -> MathResult BigList
    _binaryOperation operation left@(BigList leftVals) right@(BigList rightVals)
        | not $ equalSize left right = withError UnequalLength
        | otherwise = withValue $ BigList $ V.zipWith operation leftVals rightVals

    _binaryOperationPad :: BinaryScalarOperation -> BigScalar -> BigList -> BigList -> BigList
    _binaryOperationPad operation scalar left right = value $ _binaryOperation operation leftPadded rightPadded
        where leftSize = size left
              rightSize = size right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff scalar left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff scalar right else right

    _errableBinaryOperation :: ErrableBinaryScalarOperation -> BigList -> BigList -> MathResult BigList
    _errableBinaryOperation operation (BigList leftVals) (BigList rightVals)
        | V.null errors = withValue $ BigList $ V.map value result
        | otherwise = withErrorSet $ HS.unions $ V.toList $ V.map errorSet errors
        where result = V.zipWith operation leftVals rightVals
              errors = V.filter isSuccess result

    _errableBinaryOperationPad :: ErrableBinaryScalarOperation -> BigScalar -> BigList -> BigList -> MathResult BigList
    _errableBinaryOperationPad operation scalar left right = _errableBinaryOperation operation leftPadded rightPadded
        where leftSize = size left
              rightSize = size right
              sizeDiff = abs $ leftSize - rightSize
              leftPadded = if leftSize < rightSize then _pad sizeDiff scalar left else left
              rightPadded = if leftSize > rightSize then _pad sizeDiff scalar right else right

    _pad :: Int -> BigScalar -> BigList -> BigList
    _pad amount scalar (BigList vals) = BigList $ V.concat [vals, V.replicate amount scalar]

    lminimum :: BigList -> MathResult BigScalar
    lminimum (BigList vals)
        | _containsNonreal vals = withError NoncomparableType
        | otherwise = _lminmaxHelper smin (V.head vals) (V.tail vals)

    lmaximum :: BigList -> MathResult BigScalar
    lmaximum (BigList vals)
        | _containsNonreal vals = withError NoncomparableType
        | otherwise = _lminmaxHelper smax (V.head vals) (V.tail vals)

    _lminmaxHelper :: ErrableBinaryScalarOperation -> BigScalar -> V.Vector BigScalar -> MathResult BigScalar
    _lminmaxHelper func currentBest rest
        | isFailure newBest = withError NoncomparableType
        | otherwise = _lminmaxHelper func (value newBest) nextRest
        where headVal = V.head rest
              nextRest = V.tail rest
              newBest = func currentBest headVal

    _containsNonreal :: V.Vector BigScalar -> Bool
    _containsNonreal = V.any $ not . isReal

    lsum :: BigList -> BigScalar
    lsum (BigList vals) = V.foldl splus zero vals

    lcumsum :: BigList -> BigList
    lcumsum (BigList vals) = BigList $ V.scanl splus (V.head vals) (V.tail vals)

    lprod :: BigList -> BigScalar
    lprod (BigList vals) = V.foldl smult one vals

    lcumprod :: BigList -> BigList
    lcumprod (BigList vals) = BigList $ V.scanl smult (V.head vals) (V.tail vals)

    lmean :: BigList -> MathResult BigScalar
    lmean list@(BigList vals)
        | 0 == listSize = withError ZeroLength
        | otherwise = sdiv sumValue (integral listSize)
        where listSize = size list
              sumValue = lsum list

    lgmean :: BigList -> MathResult BigScalar
    lgmean list@(BigList vals)
        | 0 == listSize = withError ZeroLength
        | otherwise = spow prodValue (value $ sinv (integral listSize))
        where listSize = size list
              prodValue = lprod list

    lmedian :: BigList -> MathResult BigScalar
    lmedian list@(BigList vals)
        | 0 == listSize = withError ZeroLength
        | _containsNonreal vals = withError InvalidType
        | even listSize = sdiv (splus (_quickSelect (halfSize - 1) vals) (_quickSelect halfSize vals)) two
        | otherwise = withValue $ _quickSelect halfSize vals
        where listSize = size list
              halfSize = ceiling $ (fromIntegral listSize) / 2

    _quickSelect :: Int -> V.Vector BigScalar -> BigScalar
    _quickSelect index vals
        | index < leftSize = _quickSelect index left
        | index > leftSize = _quickSelect (index - leftSize - 1) right
        | otherwise = headVal
        where headVal = V.head vals
              rest = V.tail vals
              (left, right) = V.partition (\other -> LT == (value $ comparison other headVal)) rest
              leftSize = V.length left

    lrange :: BigList -> MathResult BigScalar
    lrange list
        | isFailure minValue = withError NoncomparableType
        | otherwise = binCombine maxValue minValue sminus
        where minValue = lminimum list
              maxValue = lmaximum list

    lmidrange :: BigList -> MathResult BigScalar
    lmidrange list
        | isFailure minValue = withError NoncomparableType
        | otherwise = errBinResolveLeft minmaxSum two sdiv
        where minValue = lminimum list
              maxValue = lmaximum list
              minmaxSum = binCombine minValue maxValue splus

    lmode :: BigList -> BigList
    lmode (BigList vals) = list $ HM.keys modeMap
        where counts = _count vals HM.empty
              maxCount = maximum $ HM.elems counts
              modeMap = HM.filter (==maxCount) counts

    _count :: V.Vector BigScalar -> HM.HashMap BigScalar Int -> HM.HashMap BigScalar Int
    _count vals counter
        | 0 == vecLength = counter
        | isNothing mappedVal = _count rest (HM.insert headVal 1 counter)
        | otherwise = _count rest (HM.adjust (+1) headVal counter)
        where vecLength = V.length vals
              headVal = V.head vals
              rest = V.tail vals
              mappedVal = HM.lookup headVal counter

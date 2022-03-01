{-# LANGUAGE DeriveGeneric #-}

module BigVector (
    BigVector,
    vlist,
    vvec,
    vsize,
    vequalSize,
    wPos,
    xPos,
    yPos,
    zPos,
    vget,
    vplus,
    vminus,
    smultv,
    vmults,
    vscale,
    vdot,
    vcross,
    vdivs,
    vneg,
    vabs,
    vnorm,
    vdist,
    vangle,
    asList,
    asVector,
    (==),
    (/=),
    show
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Maybe as M
    import qualified Data.Vector as V
    import qualified Data.Hashable as H
    import qualified MathInfo as MI
    import qualified BigScalar as BS

    newtype BigVector = BigVector {
        _pos :: [BS.BigScalar]
    } deriving (G.Generic, Eq)

    instance H.Hashable BigVector

    instance Show BigVector where
        show (BigVector pos) = TP.printf "Vector[%s]" (_str pos)

    _str :: [BS.BigScalar] -> String
    _str [] = ""
    _str [val] = show val
    _str (val:vals) = TP.printf "%s, %s" (show val) (_str vals)

    vlist :: [BS.BigScalar] -> MI.MathResult BigVector
    vlist pos
        | any BS.isExactQuaternion pos = MI.withError MI.InvalidType
        | otherwise = MI.withValue $ BigVector pos

    vvec :: V.Vector BS.BigScalar -> MI.MathResult BigVector
    vvec = vlist . V.toList

    vsize :: BigVector -> Int
    vsize (BigVector pos) = length pos

    vequalSize :: BigVector -> BigVector -> Bool
    vequalSize left right = (vsize left) == (vsize right)

    isNull :: BigVector -> Bool
    isNull (BigVector pos) = all (==BS.zero) pos

    wPos :: BigVector -> MI.MathResult BS.BigScalar
    wPos vec
        | 4 == (vsize vec) = MI.withValue $ head $ _pos vec
        | otherwise = MI.withError MI.InvalidLength

    xPos :: BigVector -> MI.MathResult BS.BigScalar
    xPos vec
        | vecLength <= 3 = MI.withValue $ head vecPos
        | 4 == vecLength = MI.withValue $ vecPos !! 1
        | otherwise = MI.withError MI.InvalidLength
        where vecLength = vsize vec
              vecPos = _pos vec

    yPos :: BigVector -> MI.MathResult BS.BigScalar
    yPos vec
        | vecLength <= 3 = MI.withValue $ vecPos !! 1
        | 4 == vecLength = MI.withValue $ vecPos !! 2
        | otherwise = MI.withError MI.InvalidLength
        where vecLength = vsize vec
              vecPos = _pos vec
    
    zPos :: BigVector -> MI.MathResult BS.BigScalar
    zPos vec
        | 3 == vecLength || 4 == vecLength = MI.withValue $ last $ _pos vec
        | otherwise = MI.withError MI.InvalidLength
        where vecLength = vsize vec

    vget :: BigVector -> Int -> MI.MathResult BS.BigScalar
    vget vec@(BigVector pos) index
        | index < 0 || index >= (vsize vec) = MI.withError MI.InvalidIndex
        | otherwise = MI.withValue $ pos !! index

    vplus :: BigVector -> BigVector -> MI.MathResult BigVector
    vplus = _binaryOperation BS.splus

    vminus :: BigVector -> BigVector -> MI.MathResult BigVector
    vminus = _binaryOperation BS.sminus

    vscale :: BigVector -> BigVector -> MI.MathResult BigVector
    vscale = _binaryOperation BS.smult

    _binaryOperation :: BS.BinaryScalarOperation -> BigVector -> BigVector -> MI.MathResult BigVector
    _binaryOperation operation left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ vequalSize left right = MI.withError MI.UnequalLength
        | otherwise = MI.withValue $ BigVector $ zipWith operation leftPos rightPos

    smultv :: BS.BigScalar -> BigVector -> MI.MathResult BigVector
    smultv left (BigVector rightPos)
        | BS.isQuaternion left = MI.withError MI.InvalidType
        | otherwise = MI.withValue $ BigVector $ map (BS.smult left) rightPos

    vmults :: BigVector -> BS.BigScalar -> MI.MathResult BigVector
    vmults = flip smultv

    vdot :: BigVector -> BigVector -> MI.MathResult BS.BigScalar
    vdot left right = MI.unResolve scaleResult _vsum
        where scaleResult = vscale left right

    vcross :: BigVector -> BigVector -> MI.MathResult BigVector
    vcross left right
        | 3 == (vsize left) && 3 == (vsize right) = vlist [resultXPos, resultYPos, resultZPos]
        | otherwise = MI.withError MI.InvalidLength
        where leftXPos = MI.value $ xPos left
              leftYPos = MI.value $ yPos left
              leftZPos = MI.value $ zPos left
              rightXPos = MI.value $ xPos right
              rightYPos = MI.value $ yPos right
              rightZPos = MI.value $ zPos right
              resultXPos = BS.sminus (BS.smult leftYPos rightZPos) (BS.smult leftZPos rightYPos)
              resultYPos = BS.sminus (BS.smult leftZPos rightXPos) (BS.smult leftXPos rightZPos)
              resultZPos = BS.sminus (BS.smult leftXPos rightYPos) (BS.smult leftYPos rightXPos)

    vdivs :: BigVector -> BS.BigScalar -> MI.MathResult BigVector
    vdivs left right
        | BS.zero == right = MI.withError MI.DivideByZero
        | otherwise = vmults left rightInv
        where rightInv = MI.value $ BS.sinv right

    vneg :: BigVector -> BigVector
    vneg = MI.value . (smultv BS.negOne)

    vabs :: BigVector -> BS.BigScalar
    vabs vec = BS.ssqrt $ MI.value $ vdot vec vec

    vnorm :: BigVector -> MI.MathResult BigVector
    vnorm vec
        | isNull vec = MI.withError MI.NullVector
        | otherwise = vdivs vec (vabs vec)

    vdist :: BigVector -> BigVector -> MI.MathResult BS.BigScalar
    vdist left right
        | MI.isFailure subtResult = MI.convert subtResult
        | otherwise = MI.withValue $ BS.ssqrt $ _sum $ map square (_pos subtValue)
        where subtResult = vminus left right
              subtValue = MI.value subtResult
              square = MI.value . (flip BS.spow) BS.two

    _vsum :: BigVector -> BS.BigScalar
    _vsum (BigVector pos) = _sum pos

    _sum :: [BS.BigScalar] -> BS.BigScalar
    _sum = foldr BS.splus BS.zero

    vangle :: BigVector -> BigVector -> MI.MathResult BS.BigScalar
    vangle left right
        | not $ vequalSize left right = MI.withError MI.UnequalLength
        | (isNull left) || (isNull right) = MI.withError MI.NullVector
        | otherwise = BS.sacos $ MI.value $ BS.sdiv (MI.value $ vdot left right) (BS.smult (vabs left) (vabs right))

    asList :: BigVector -> [BS.BigScalar]
    asList (BigVector pos) = pos
    
    asVector :: BigVector -> V.Vector BS.BigScalar
    asVector (BigVector pos) = V.fromList pos

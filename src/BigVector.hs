{-# LANGUAGE DeriveGeneric #-}

module BigVector (
    BigVector,
    vlist,
    vseq,
    vsize,
    vlength,
    vequalSize,
    vIsNull,
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
    asSeq
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.List as L
    import qualified Data.Sequence as S
    import qualified Data.Foldable as F
    import qualified Data.Hashable as H
    import qualified MathInfo as MI
    import qualified BigScalar as BS

    newtype BigVector = BigVector {
        _pos :: [BS.BigScalar]
    } deriving (G.Generic, Eq)

    instance H.Hashable BigVector where
        hashWithSalt salt (BigVector pos) = H.hashWithSalt salt pos

    instance Show BigVector where
        show (BigVector pos) = TP.printf "Vector[%s]" (_str pos)

    _str :: [BS.BigScalar] -> String
    _str [] = ""
    _str [val] = show val
    _str (val:vals) = TP.printf "%s, %s" (show val) (_str vals)

    vlist :: [BS.BigScalar] -> MI.Result BigVector
    vlist pos
        | any BS.isExactQuaternion pos = MI.withError MI.InvalidType
        | otherwise = MI.withValue $ BigVector pos

    vseq :: S.Seq BS.BigScalar -> MI.Result BigVector
    vseq = vlist . F.toList

    vsize :: BigVector -> BS.BigScalar
    vsize = BS.integral . vlength

    vlength :: BigVector -> Int
    vlength (BigVector pos) = length pos

    vequalSize :: BigVector -> BigVector -> Bool
    vequalSize left right = (vlength left) == (vlength right)

    vIsNull :: BigVector -> Bool
    vIsNull (BigVector pos) = all (==BS.zero) pos

    wPos :: BigVector -> MI.Result BS.BigScalar
    wPos vec
        | 4 == vecLength = MI.withValue $ head $ _pos vec
        | otherwise = MI.withError MI.InvalidLength
        where vecLength = vlength vec

    xPos :: BigVector -> MI.Result BS.BigScalar
    xPos vec
        | vecLength <= 3 = MI.withValue $ head vecPos
        | 4 == vecLength = MI.withValue $ vecPos !! 1
        | otherwise = MI.withError MI.InvalidLength
        where vecLength = vlength vec
              vecPos = _pos vec

    yPos :: BigVector -> MI.Result BS.BigScalar
    yPos vec
        | vecLength <= 3 = MI.withValue $ vecPos !! 1
        | 4 == vecLength = MI.withValue $ vecPos !! 2
        | otherwise = MI.withError MI.InvalidLength
        where vecLength = vlength vec
              vecPos = _pos vec
    
    zPos :: BigVector -> MI.Result BS.BigScalar
    zPos vec
        | 3 == vecLength || 4 == vecLength = MI.withValue $ last $ _pos vec
        | otherwise = MI.withError MI.InvalidLength
        where vecLength = vlength vec

    vget :: BigVector -> BS.BigScalar -> MI.Result BS.BigScalar
    vget vec@(BigVector pos) index
        | not $ BS.isExactInteger index = MI.withError MI.InvalidType
        | lessThanZero || greaterThanSize = MI.withError MI.InvalidIndex
        | otherwise = MI.withValue $ L.genericIndex pos integralIndex
        where vecSize = vsize vec
              integralIndex = BS.asBuiltInInteger index
              lessThanZero = MI.value $ BS.sLess index BS.zero
              greaterThanSize = MI.value $ BS.sGreater index vecSize

    vplus :: BigVector -> BigVector -> MI.Result BigVector
    vplus = _binaryOperation BS.splus

    vminus :: BigVector -> BigVector -> MI.Result BigVector
    vminus = _binaryOperation BS.sminus

    vscale :: BigVector -> BigVector -> MI.Result BigVector
    vscale = _binaryOperation BS.smult

    _binaryOperation :: BS.BinaryScalarOperation -> BigVector -> BigVector -> MI.Result BigVector
    _binaryOperation operation left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ vequalSize left right = MI.withError MI.UnequalLength
        | otherwise = MI.withValue $ BigVector $ zipWith operation leftPos rightPos

    smultv :: BS.BigScalar -> BigVector -> MI.Result BigVector
    smultv left (BigVector rightPos)
        | BS.isExactQuaternion left = MI.withError MI.InvalidType
        | otherwise = MI.withValue $ BigVector $ fmap (BS.smult left) rightPos

    vmults :: BigVector -> BS.BigScalar -> MI.Result BigVector
    vmults = flip smultv

    vdot :: BigVector -> BigVector -> MI.Result BS.BigScalar
    vdot left right = MI.unResolve scaleResult _vsum
        where scaleResult = vscale left right

    vcross :: BigVector -> BigVector -> MI.Result BigVector
    vcross left right
        | 3 == (vlength left) && 3 == (vlength right) = vlist [resultXPos, resultYPos, resultZPos]
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

    vdivs :: BigVector -> BS.BigScalar -> MI.Result BigVector
    vdivs left right
        | BS.zero == right = MI.withError MI.DivideByZero
        | otherwise = vmults left rightInv
        where rightInv = MI.value $ BS.sinv right

    vneg :: BigVector -> BigVector
    vneg = MI.value . (smultv BS.negOne)

    vabs :: BigVector -> BS.BigScalar
    vabs vec = BS.ssqrt $ MI.value $ vdot vec vec

    vnorm :: BigVector -> MI.Result BigVector
    vnorm vec
        | vIsNull vec = MI.withError MI.NullVector
        | otherwise = vdivs vec (vabs vec)

    vdist :: BigVector -> BigVector -> MI.Result BS.BigScalar
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

    vangle :: BigVector -> BigVector -> MI.Result BS.BigScalar
    vangle left right
        | not $ vequalSize left right = MI.withError MI.UnequalLength
        | (vIsNull left) || (vIsNull right) = MI.withError MI.NullVector
        | otherwise = BS.sacos $ MI.value $ BS.sdiv (MI.value $ vdot left right) (BS.smult (vabs left) (vabs right))

    asList :: BigVector -> [BS.BigScalar]
    asList (BigVector pos) = pos
    
    asSeq :: BigVector -> S.Seq BS.BigScalar
    asSeq (BigVector pos) = S.fromList pos

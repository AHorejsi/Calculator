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
    vangle
) where
    import Text.Printf
    import qualified Data.Vector as V
    import MathInfo
    import BigScalar

    newtype BigVector = BigVector {
        _pos :: V.Vector BigScalar
    } deriving (Eq)

    instance Show BigVector where
        show (BigVector pos) = printf "Vector[%s]" (_str pos)

    _str :: V.Vector BigScalar -> String
    _str vals = case V.length vals of 0 -> ""
                                      1 -> printf "%s" headVal
                                      _ -> printf "%s, %s" headVal (_str rest)
        where headVal = show $ V.head vals
              rest = V.tail vals

    vlist :: [BigScalar] -> MathResult BigVector
    vlist pos
        | any isQuaternion pos = withError InvalidType
        | otherwise = withValue $ BigVector $ V.fromList pos

    vvec :: V.Vector BigScalar -> MathResult BigVector
    vvec = vlist . V.toList

    vsize :: BigVector -> Int
    vsize (BigVector pos) = length pos

    vequalSize :: BigVector -> BigVector -> Bool
    vequalSize left right = (vsize left) == (vsize right)

    isNull :: BigVector -> Bool
    isNull (BigVector pos) = V.all (==zero) pos

    wPos :: BigVector -> MathResult BigScalar
    wPos vec
        | 4 == (vsize vec) = withValue $ V.head $ _pos vec
        | otherwise = withError InvalidLength

    xPos :: BigVector -> MathResult BigScalar
    xPos vec
        | vecLength <= 3 = withValue $ V.head vecPos
        | 4 == vecLength = withValue $ vecPos V.! 1
        | otherwise = withError InvalidLength
        where vecLength = vsize vec
              vecPos = _pos vec

    yPos :: BigVector -> MathResult BigScalar
    yPos vec
        | vecLength <= 3 = withValue $ vecPos V.! 1
        | 4 == vecLength = withValue $ vecPos V.! 2
        | otherwise = withError InvalidLength
        where vecLength = vsize vec
              vecPos = _pos vec
    
    zPos :: BigVector -> MathResult BigScalar
    zPos vec
        | 3 == vecLength || 4 == vecLength = withValue $ V.last $ _pos vec
        | otherwise = withError InvalidLength
        where vecLength = vsize vec

    vget :: BigVector -> Int -> BigScalar
    vget (BigVector pos) index = pos V.! index

    vplus :: BigVector -> BigVector -> MathResult BigVector
    vplus = _binaryOperation splus

    vminus :: BigVector -> BigVector -> MathResult BigVector
    vminus = _binaryOperation sminus

    vscale :: BigVector -> BigVector -> MathResult BigVector
    vscale = _binaryOperation smult

    _binaryOperation :: BinaryScalarOperation -> BigVector -> BigVector -> MathResult BigVector
    _binaryOperation operation left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ vequalSize left right = withError UnequalLength
        | otherwise = withValue $ BigVector $ V.zipWith operation leftPos rightPos

    smultv :: BigScalar -> BigVector -> MathResult BigVector
    smultv left (BigVector rightPos)
        | isQuaternion left = withError InvalidType
        | otherwise = withValue $ BigVector $ V.map (smult left) rightPos

    vmults :: BigVector -> BigScalar -> MathResult BigVector
    vmults = flip smultv

    vdot :: BigVector -> BigVector -> MathResult BigScalar
    vdot left right = unResolve scaleResult _vsum
        where scaleResult = vscale left right

    vcross :: BigVector -> BigVector -> MathResult BigVector
    vcross left right
        | 3 == (vsize left) && 3 == (vsize right) = withValue $ BigVector $ V.fromList [resultXPos, resultYPos, resultZPos]
        | otherwise = withError InvalidLength
        where leftXPos = value $ xPos left
              leftYPos = value $ yPos left
              leftZPos = value $ zPos left
              rightXPos = value $ xPos right
              rightYPos = value $ yPos right
              rightZPos = value $ zPos right
              resultXPos = sminus (smult leftYPos rightZPos) (smult leftZPos rightYPos)
              resultYPos = sminus (smult leftZPos rightXPos) (smult leftXPos rightZPos)
              resultZPos = sminus (smult leftXPos rightYPos) (smult leftYPos rightXPos)

    vdivs :: BigVector -> BigScalar -> MathResult BigVector
    vdivs left right
        | zero == right = withError DivideByZero
        | otherwise = vmults left rightInv
        where rightInv = value $ sinv right

    vneg :: BigVector -> BigVector
    vneg = value . (smultv negOne)

    vabs :: BigVector -> BigScalar
    vabs vec = ssqrt $ value $ vdot vec vec

    vnorm :: BigVector -> MathResult BigVector
    vnorm vec
        | isNull vec = withError NullVector
        | otherwise = vdivs vec (vabs vec)

    vdist :: BigVector -> BigVector -> MathResult BigScalar
    vdist left right
        | isFailure subtResult = convert subtResult
        | otherwise = withValue $ ssqrt $ _sum $ V.map (value . (flip spow) two) (_pos subtValue)
        where subtResult = vminus left right
              subtValue = value subtResult

    _vsum :: BigVector -> BigScalar
    _vsum (BigVector pos) = _sum pos

    _sum :: V.Vector BigScalar -> BigScalar
    _sum = V.foldr splus zero

    vangle :: BigVector -> BigVector -> MathResult BigScalar
    vangle left right
        | not $ vequalSize left right = withError UnequalLength
        | (isNull left) || (isNull right) = withError NullVector
        | otherwise = sacos $ value $ sdiv (value $ vdot left right) (smult (vabs left) (vabs right))

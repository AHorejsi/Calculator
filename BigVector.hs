module BigVector (
    BigVector,
    vector,
    dimensions,
    equalDimensions,
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
    import MathInfo
    import BigScalar

    newtype BigVector = BigVector {
        _pos :: [BigScalar]
    } deriving (Eq)

    instance Show BigVector where
        show (BigVector pos) = printf "Vector[%s]" (_str pos)

    _str :: [BigScalar] -> String
    _str [] = ""
    _str [val] = printf "%s" (show val)
    _str (val:vals) = printf "%s, %s" (show val) (_str vals)

    vector :: [BigScalar] -> MathResult BigVector
    vector pos
        | any isQuaternion pos = withError InvalidType
        | otherwise = withValue $ BigVector pos

    dimensions :: BigVector -> Int
    dimensions (BigVector pos) = length pos

    equalDimensions :: BigVector -> BigVector -> Bool
    equalDimensions left right = (dimensions left) == (dimensions right)

    isNull :: BigVector -> Bool
    isNull (BigVector pos) = all (==zero) pos

    wPos :: BigVector -> MathResult BigScalar
    wPos vec
        | 4 == (dimensions vec) = withValue $ head $ _pos vec
        | otherwise = withError InvalidLength

    xPos :: BigVector -> MathResult BigScalar
    xPos vec
        | vecLength <= 3 = withValue $ head vecPos
        | 4 == vecLength = withValue $ vecPos !! 1
        | otherwise = withError InvalidLength
        where vecLength = dimensions vec
              vecPos = _pos vec

    yPos :: BigVector -> MathResult BigScalar
    yPos vec
        | vecLength <= 3 = withValue $ vecPos !! 1
        | 4 == vecLength = withValue $ vecPos !! 2
        | otherwise = withError InvalidLength
        where vecLength = dimensions vec
              vecPos = _pos vec
    
    zPos :: BigVector -> MathResult BigScalar
    zPos vec
        | 3 == vecLength || 4 == vecLength = withValue $ last $ _pos vec
        | otherwise = withError InvalidLength
        where vecLength = dimensions vec

    vget :: BigVector -> Int -> BigScalar
    vget (BigVector pos) index = pos !! index

    vplus :: BigVector -> BigVector -> MathResult BigVector
    vplus = _binaryOperation splus

    vminus :: BigVector -> BigVector -> MathResult BigVector
    vminus = _binaryOperation sminus

    vscale :: BigVector -> BigVector -> MathResult BigVector
    vscale = _binaryOperation smult

    _binaryOperation :: BinaryScalarOperation -> BigVector -> BigVector -> MathResult BigVector
    _binaryOperation operation left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ equalDimensions left right = withError UnequalLength
        | otherwise = withValue $ BigVector $ zipWith operation leftPos rightPos

    smultv :: BigScalar -> BigVector -> MathResult BigVector
    smultv left (BigVector rightPos)
        | isQuaternion left = withError InvalidType
        | otherwise = withValue $ BigVector $ map (smult left) rightPos

    vmults :: BigVector -> BigScalar -> MathResult BigVector
    vmults = flip smultv

    vdot :: BigVector -> BigVector -> MathResult BigScalar
    vdot left right = unResolve scaleResult _vsum
        where scaleResult = vscale left right

    vcross :: BigVector -> BigVector -> MathResult BigVector
    vcross left right
        | 3 == (dimensions left) && 3 == (dimensions right) = withValue $ BigVector [resultXPos, resultYPos, resultZPos]
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
        | otherwise = withValue $ ssqrt $ _sum $ map (value . (flip spow) two) (_pos subtValue)
        where subtResult = vminus left right
              subtValue = value subtResult

    _vsum :: BigVector -> BigScalar
    _vsum (BigVector pos) = _sum pos

    _sum :: [BigScalar] -> BigScalar
    _sum pos = foldl splus zero pos

    vangle :: BigVector -> BigVector -> MathResult BigScalar
    vangle left right
        | not $ equalDimensions left right = withError UnequalLength
        | (isNull left) || (isNull right) = withError NullVector
        | otherwise = sacos $ value $ sdiv (value $ vdot left right) (smult (vabs left) (vabs right))

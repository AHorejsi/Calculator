module Vector (
    BigVector,
    vector,
    dimensions,
    equalDimensions,
    wPos,
    xPos,
    yPos,
    zPos,
    valPos,
    vplus,
    vminus,
    smultv,
    vmults,
    vscale,
    vdot
) where
    import MathInfo
    import Scalar

    newtype BigVector = BigVector {
        _pos :: [BigScalar]
    } deriving (Eq, Show)

    vector :: [BigScalar] -> MathResult BigVector
    vector pos
        | _containsQuaternion pos = withError InvalidType
        | otherwise = withValue $ BigVector pos

    _containsQuaternion :: [BigScalar] -> Bool
    _containsQuaternion [] = False
    _containsQuaternion (val:vals) = (isQuaternion val) || (_containsQuaternion vals)

    dimensions :: BigVector -> Int
    dimensions (BigVector pos) = length pos

    equalDimensions :: BigVector -> BigVector -> Bool
    equalDimensions left right = (dimensions left) == (dimensions right)

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

    valPos :: BigVector -> Int -> MathResult BigScalar
    valPos vec index
        | index >= (dimensions vec) = withError InvalidIndex
        | otherwise = withValue $ (_pos vec) !! index

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
    vdot left right = resolve scaleResult (\vec -> foldl splus zero (_pos vec))
        where scaleResult = vscale left right

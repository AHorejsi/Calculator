module Vector (
    Vector,
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
    vscale
) where
    import Scalar (Scalar, isQuaternion, zero, splus, sminus, smult)
    import MathInfo (MathResult, MathError(InvalidIndex, InvalidLength, InvalidType), mathValue, mathError, withValue, withError, withErrorSet, isSuccess)

    data Vector a = VectorND {
        _pos :: [Scalar a]
    }

    vector :: [Scalar a] -> MathResult (Vector a)
    vector pos
        | containsQuaternion pos = withError InvalidType
        | otherwise = withValue $ VectorND pos
        where containsQuaternion [] = False
              containsQuaternion (val:vals) = (isQuaternion val) || (containsQuaternion vals)

    dimensions :: Vector a -> Int
    dimensions (VectorND pos) = length pos

    equalDimensions :: Vector a -> Vector a -> Bool
    equalDimensions left right = (dimensions left) == (dimensions right)

    wPos :: Vector a -> MathResult (Scalar a)
    wPos vec
        | 4 == (dimensions vec) = withValue $ head $ _pos vec
        | otherwise = withError InvalidLength

    xPos :: Vector a -> MathResult (Scalar a)
    xPos vec
        | vecLength <= 3 = withValue $ head vecPos
        | 4 == vecLength = withValue $ vecPos !! 1
        | otherwise = withError InvalidLength
        where vecLength = dimensions vec
              vecPos = _pos vec

    yPos :: Vector a -> MathResult (Scalar a)
    yPos vec
        | vecLength <= 3 = withValue $ vecPos !! 1
        | 4 == vecLength = withValue $ vecPos !! 2
        where vecLength = dimensions vec
              vecPos = _pos vec
    
    zPos :: Vector a -> MathResult (Scalar a)
    zPos vec
        | vecLength <= 4 = withValue $ last $ _pos vec
        | otherwise = withError InvalidLength
        where vecLength = dimensions vec

    valPos :: Vector a -> Int -> MathResult (Scalar a)
    valPos vec index
        | index >= (dimensions vec) = withError InvalidIndex
        | otherwise = withValue $ (_pos vec) !! index

    vplus :: (RealFloat a) => Vector a -> Vector a -> MathResult (Vector a)
    vplus left@(VectorND leftPos) right@(VectorND rightPos)
        | not $ equalDimensions left right = withError InvalidLength
        | otherwise = withValue $ VectorND $ zipWith splus leftPos rightPos

    vminus :: (RealFloat a) => Vector a -> Vector a -> MathResult (Vector a)
    vminus left@(VectorND leftPos) right@(VectorND rightPos)
        | not $ equalDimensions left right = withError InvalidLength
        | otherwise = withValue $ VectorND $ zipWith sminus leftPos rightPos

    smultv :: (RealFloat a) => Scalar a -> Vector a -> MathResult (Vector a)
    smultv left (VectorND rightPos)
        | isQuaternion left = withError InvalidType
        | otherwise = withValue $ VectorND $ map (smult left) rightPos

    vmults :: (RealFloat a) => Vector a -> Scalar a -> MathResult (Vector a)
    vmults = flip smultv
    
    vscale :: (RealFloat a) => Vector a -> Vector a -> MathResult (Vector a)
    vscale left@(VectorND leftPos) right@(VectorND rightPos)
        | not $ equalDimensions left right = withError InvalidLength
        | otherwise = withValue $ VectorND $ zipWith smult leftPos rightPos

    vdot :: (RealFloat a) => Vector a -> Vector a -> MathResult (Scalar a)
    vdot left right
        | isSuccess scaleResult = withValue $ foldr splus zero (_pos $ mathValue scaleResult)
        | otherwise = withErrorSet $ mathError scaleResult
        where scaleResult = vscale left right

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
    import Scalar
    import MathInfo

    newtype Vector a = Vector {
        _pos :: [Scalar a]
    }

    vector :: [Scalar a] -> MathResult (Vector a)
    vector pos
        | containsQuaternion pos = withError InvalidType
        | otherwise = withValue $ Vector pos
        where containsQuaternion [] = False
              containsQuaternion (val:vals) = (isQuaternion val) || (containsQuaternion vals)

    dimensions :: Vector a -> Int
    dimensions (Vector pos) = length pos

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
        | otherwise = withError InvalidLength
        where vecLength = dimensions vec
              vecPos = _pos vec
    
    zPos :: Vector a -> MathResult (Scalar a)
    zPos vec
        | 3 == vecLength || 4 == vecLength = withValue $ last $ _pos vec
        | otherwise = withError InvalidLength
        where vecLength = dimensions vec

    valPos :: Vector a -> Int -> MathResult (Scalar a)
    valPos vec index
        | index >= (dimensions vec) = withError InvalidIndex
        | otherwise = withValue $ (_pos vec) !! index

    vplus :: (RealFloat a) => Vector a -> Vector a -> MathResult (Vector a)
    vplus left@(Vector leftPos) right@(Vector rightPos)
        | not $ equalDimensions left right = withError InvalidLength
        | otherwise = withValue $ Vector $ zipWith splus leftPos rightPos

    vminus :: (RealFloat a) => Vector a -> Vector a -> MathResult (Vector a)
    vminus left@(Vector leftPos) right@(Vector rightPos)
        | not $ equalDimensions left right = withError InvalidLength
        | otherwise = withValue $ Vector $ zipWith sminus leftPos rightPos

    smultv :: (RealFloat a) => Scalar a -> Vector a -> MathResult (Vector a)
    smultv left (Vector rightPos)
        | isQuaternion left = withError InvalidType
        | otherwise = withValue $ Vector $ map (smult left) rightPos

    vmults :: (RealFloat a) => Vector a -> Scalar a -> MathResult (Vector a)
    vmults = flip smultv
    
    vscale :: (RealFloat a) => Vector a -> Vector a -> MathResult (Vector a)
    vscale left@(Vector leftPos) right@(Vector rightPos)
        | not $ equalDimensions left right = withError InvalidLength
        | otherwise = withValue $ Vector $ zipWith smult leftPos rightPos

    vdot :: (RealFloat a) => Vector a -> Vector a -> MathResult (Scalar a)
    vdot left right
        | isSuccess scaleResult = withValue $ foldr splus zero (_pos $ value scaleResult)
        | otherwise = withErrorSet $ errorSet scaleResult
        where scaleResult = vscale left right

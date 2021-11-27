module Vector (
    Vector,
    vector,
    dimensions,
) where
    import Scalar (Scalar, isQuaternion)
    import MathInfo (MathResult, MathError(InvalidType), withValue, withError)

    data Vector a = Vector2D {
        _xPos2 :: Scalar a,
        _yPos2 :: Scalar a
    } | Vector3D {
        _xPos3 :: Scalar a,
        _yPos3 :: Scalar a,
        _zPos3 :: Scalar a
    } | Vector4D {
        _wPos4 :: Scalar a,
        _xPos4 :: Scalar a,
        _yPos4 :: Scalar a,
        _zPos4 :: Scalar a
    } | VectorND {
        _posN :: [Scalar a]
    }

    vector :: [Scalar a] -> MathResult (Vector a)
    vector pos
        | containsQuaternion pos = withError InvalidType
        | otherwise = withValue $ case (length pos) of 2 -> Vector2D first second
                                                       3 -> Vector3D first second third
                                                       4 -> Vector4D first second third fourth
                                                       _ -> VectorND pos
        where containsQuaternion [] = False
              containsQuaternion (val:vals) = (isQuaternion val) || (containsQuaternion vals)
              first = pos !! 0
              second = pos !! 1
              third = pos !! 2
              fourth = pos !! 3

    dimensions :: Vector a -> Int
    dimensions Vector2D{} = 2
    dimensions Vector3D{} = 3
    dimensions Vector4D{} = 4
    dimensions (VectorND pos) = length pos
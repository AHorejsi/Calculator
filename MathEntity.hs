module MathEntity (

) where
    import Scalar
    import Vector

    data MathEntity a = ScalarEntity {
        _scalar :: Scalar a
    } | VectorEntity {
        _vector :: Vector a
    }

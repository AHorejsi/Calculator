{-# LANGUAGE DeriveGeneric #-}

module MathEntity (
    MathEntity,
    (==),
    (/=),
    show
) where
    import GHC.Generics
    import BigScalar
    import BigVector
    import BigList
    import BigMatrix

    data MathEntity = ScalarEntity {
        _scalar :: BigScalar
    } | VectorEntity {
        _vector :: BigVector
    } | ListEntity {
        _list :: BigList
    } | MatrixEntity {
        _matrix :: BigMatrix
    } | BoolEntity {
        _bool :: Bool
    } deriving (Eq, Generic)

    instance Show MathEntity where
        show entity = "MathEntity:\n" ++ (show entity)
    
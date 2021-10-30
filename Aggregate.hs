module Aggregate (
    Aggregater(
        Vector,
        List,
        Matrix,
        AFail
    )
) where
    import Scalar

    data Aggregater a = Vector {
        pos :: [Scalar a]
    } | List {
        vals :: [Scalar a]
    } | Matrix {
        table :: [Scalar a],
        rows :: Int,
        cols :: Int
    } | AFail {
        message :: String
    }

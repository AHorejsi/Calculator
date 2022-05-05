module Actions (
    Addable,
    plus
) where
    import qualified MathInfo as MI

    class Addable a where
        plus :: a -> a -> MI.ComputationResult a

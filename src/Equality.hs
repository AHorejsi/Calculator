module Equality(
    Equality,
    eq,
    ne
) where
    import qualified Actions as A

    -- | Represents a type that can have an equality check fail
    -- | Failure is typically caused by equality being impossible to determine
    class Equality t where
        -- | Checks if the 2 input values are equal. Fails if equality cannot be determined
        eq :: t -> t -> A.Computation Bool
        -- | Checks if the 2 input values are NOT equal. Fails if equality cannot be determined
        ne :: t -> t -> A.Computation Bool
        ne left right = A.resolveUnary eqResult not
            where eqResult = eq left right
                  
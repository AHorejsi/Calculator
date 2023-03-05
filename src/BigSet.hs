{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module BigSet (
    BigSet,
    condition,
    union,
    intersection,
    symmetricDifference,
    absoluteComplement,
    relativeComplement,
    member
) where
    import qualified GHC.Generics as G
    import qualified Data.HashSet as HS
    import qualified Actions as A

    data BigSet a =
        Condition (A.UnaryPredicate a) |
        Union (BigSet a) (BigSet a) |
        Intersection (BigSet a) (BigSet a) |
        SymmetricDifference (BigSet a) (BigSet a) |
        AbsoluteComplement (BigSet a) |
        RelativeComplement (BigSet a) (BigSet a)
        deriving (G.Generic)

    condition :: A.UnaryPredicate a -> BigSet a
    condition = Condition

    union :: BigSet a -> BigSet a -> BigSet a
    union = Union

    intersection :: BigSet a -> BigSet a -> BigSet a
    intersection = Intersection

    symmetricDifference :: BigSet a -> BigSet a -> BigSet a
    symmetricDifference = SymmetricDifference

    absoluteComplement :: BigSet a -> BigSet a
    absoluteComplement = AbsoluteComplement

    relativeComplement :: BigSet a -> BigSet a -> BigSet a
    relativeComplement = RelativeComplement
    
    member :: a -> BigSet a -> Bool
    member val (Condition pred) = pred val
    member val (Union left right) = (member val left) || (member val right)
    member val (Intersection left right) = (member val left) && (member val right)
    member val (SymmetricDifference left right) = (member val left) /= (member val right)
    member val (AbsoluteComplement set) = not $ member val set
    member val (RelativeComplement left right) = (not $ member val left) && (member val right)
    
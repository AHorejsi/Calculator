{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Indexable (
    Indexable,
    count,
    empty,
    none,
    at,
    prepend,
    draw,
    skip,
    pair,
    pairOn,
    switch,
    front,
    back,
    trail,
    part,
    attach,
    sieve,
    only,
    link
) where
    import qualified Data.Maybe as M
    import qualified Data.Foldable as Fo
    import qualified Data.Functor as Fu
    import qualified Data.Hashable as H
    import qualified Data.Sequence as S
    import qualified Data.Vector as V
    import qualified Actions as A
    
    -- | Represents a linear container that has elements that can be accessed by index
    class (Fo.Foldable f, Fu.Functor f) => Indexable f where
        -- | Returns the number of elements in the given 'Indexable'
        count :: f a -> Int
        -- | Returns an empty version of the given 'Indexable'
        empty :: f a
        -- | Checks if the given 'Indexable' is empty
        none :: f a -> Bool
        -- | Accesses the element at the given index from the given 'Indexable'
        at :: f a -> Int -> a
        -- | Inserts a new element at the beginning of the given 'Indexable'
        prepend :: a -> f a -> f a
        -- | Retrieves the first n elements of the given 'Indexable'
        draw :: Int -> f a -> f a
        -- | Removes the first n elements of the given 'Indexable'
        skip :: Int -> f a -> f a
        -- | Creates a new 'Indexable' of 2-tuples where each element from each 'Indexable' is matched together in a tuple based on index
        pair :: f a -> f b -> f (a, b)
        -- | Converts the given 'Foldable' to an 'Indexable'
        switch :: (Fo.Foldable g) => g a -> f a
        -- | Creates two separate 'Indexable' instances that are splits from the given 'Indexable' at the given index
        part :: Int -> f a -> (f a, f a)
        -- | Concatenates the two given 'Indexable' instances together
        attach :: f a -> f a -> f a
        -- | Filters out the elements from the given 'Indexable' that match the given 'Predicate'
        sieve :: A.UnaryPredicate a -> f a -> f a
        -- | Creates a new 'Indexable' containing only the given element
        only :: a -> f a
        only = (`prepend` empty)
        -- | Accesses the first element of the 'Indexable'
        front :: f a -> a
        front = (`at` 0)
        -- | Access the last element of the 'Indexable'
        back :: f a -> a
        back container = at container ((Fo.length container) - 1)
        -- | Removes the first element of the 'Indexable' and returns the rest
        trail :: f a -> f a
        trail = skip 1
        -- | Pairs elements together based on index and combines them via some 'Action'
        pairOn :: A.BinaryAction a b c -> f a -> f b -> f c
        pairOn action left right = Fu.fmap (uncurry action) (pair left right)
        -- | Concatenates all 'Indexable' instances in the list together
        link :: [f a] -> f a
        link = Fo.foldl' attach empty

    instance Indexable S.Seq where
        count = S.length
        empty = S.empty
        none = S.null
        at = M.fromJust A..: (S.!?)
        prepend = (S.<|)
        draw = S.take
        skip = S.drop
        pair = S.zip
        switch = Fo.foldr (S.<|) S.empty
        part = S.splitAt
        attach = (S.><)
        sieve = S.filter

    instance Indexable V.Vector where
        count = V.length
        empty = V.empty
        none = V.null
        at = (V.!)
        prepend = V.cons
        draw = V.take
        skip = V.drop
        pair = V.zip
        switch = V.fromList . Fo.toList
        part = V.splitAt
        attach = (V.++)
        sieve = V.filter

    instance Indexable [] where
        count = length
        empty = []
        none = null
        at = (!!)
        prepend = (:)
        draw = take
        skip = drop
        pair = zip
        switch = Fo.toList
        part = splitAt
        attach = (++)
        sieve = filter

    instance (Indexable f, H.Hashable a) => H.Hashable (f a) where
        hashWithSalt salt vals = Fo.foldr H.hashWithSalt salt hashed
            where hashed = Fu.fmap (H.hashWithSalt salt) vals

    instance (Indexable f, Eq a) => Eq (f a) where
        (==) left right = equalSize && sameElements
            where equalSize = (Fo.length left) == (Fo.length right)
                  sameElements = Fo.and $ pairOn (==) left right

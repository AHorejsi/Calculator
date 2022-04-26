module Debug (
    DebugString,
    stringify
) where

    class (Show a) => DebugString a where
        stringify :: a -> String
        stringify = show

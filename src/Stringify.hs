module Stringify (
    Stringifier,
    stringify
) where

    class Stringifier a where
        stringify :: a -> String

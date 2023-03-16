module Stringify (
    Stringifier,
    stringify
) where

    class Stringifier t where
        stringify :: t -> String

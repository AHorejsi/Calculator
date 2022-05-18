module Stringify (
    Stringifier,
    stringify
) where
    import qualified CalcSettings as CS

    class Stringifier a where
        stringify :: CS.PrintSettings -> a -> String

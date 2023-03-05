module Underlying (
    Underlying,
    stringify
) where
    import qualified Data.Number.CReal as NCR
    
    -- | Real number type to be used by all math entities
    -- | Should always be mapped to some arbitrary-precision real number
    type Underlying = NCR.CReal

    -- | Represents the maximum number of decimal places to be used when converting
    -- an 'Underlying' to a 'String' 
    _decimals :: Int
    _decimals = 20
    
    -- | 'String' representation of the given 'Underlying'
    stringify :: Underlying -> String
    stringify = NCR.showCReal _decimals
        
    
{-# LANGUAGE LambdaCase #-}
    
module Parse (
    satisfy
) where
    import qualified Text.ParserCombinators.Parsec as TPPC
    import qualified Text.ParserCombinators.Parsec.Token as TPPCT
    import qualified Text.ParserCombinators.Parsec.Language as TPPCL
    import qualified MathEntity as ME

    data ParseError i e =
        EndOfInput |
        Unexpected i |
        Custom e |
        Empty
        deriving (Show)

    newtype Parser i e a = Parser {
        runParser :: [i] -> Either [ParseError i e] (a, [i])
    }

    instance Functor (Parser i e) where
        fmap func (Parser p) = Parser (\input ->
            case p input of
                Left err -> Left err
                Right (output, rest) -> Right (func output, rest)
            )

    satisfy :: (i -> Bool) -> Parser i e i
    satisfy predicate = Parser (\case
            [] -> Left [EndOfInput]
            (hd:rest)
                | predicate hd -> Right (hd, rest)
                | otherwise -> Left [Unexpected hd]    
        )

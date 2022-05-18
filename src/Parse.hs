{-# LANGUAGE LambdaCase #-}

module Parse (
    
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
        deriving (Eq, Show)

    newtype Parser i e a = Parser {
        runParser :: [i] -> Either [ParseError i e] (a, [i])
    }

    satisfy :: (i -> Bool) -> Parser i e i
    satisfy predicate = Parser (\case
            [] -> Left [EndOfInput]
            (hd:rest)
                | predicate hd -> Right (hd, rest)
                | otherwise -> Left [Unexpected hd]    
        )

    data SyntaxError =
        InputEnd |
        UnexpectedInput |
        InvalidSyntax |
        Null
        deriving (Enum, Eq, Show)

    data ParseResult = SuccessfulParse {
        _parsed :: ME.MathEntity,
        _rest :: String
    } | FailedParse {
        _err :: SyntaxError
    } deriving (Eq)

    instance Functor (Parser i e) where
        fmap func (Parser p) = Parser (\input ->
            case p input of
                Left err -> Left err
                Right (output, rest) -> Right (func output, rest)
            )

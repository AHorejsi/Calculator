module Parse (
    
) where
    import qualified Text.ParserCombinators.Parsec as TPP
    import qualified Text.ParserCombinators.Parsec.Token as TPPT
    import qualified Text.ParserCombinators.Parsec.Language as TPPL
    import qualified MathEntity as ME

    data ParseError i e =
        EndOfInput |
        Unexpected i |
        Custom e |
        Empty
        deriving (Eq, Show)

    data ParserError =
        InputEnd |
        UnexpectedInput |
        InvalidSyntax |
        Null
        deriving (Enum, Eq, Show)

    data ParseResult = OkParse {
        parsed :: ME.MathEntity,
        rest :: String
    } | BadParse {
        err :: ParserError
    }

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
    satisfy predicate = Parser (\input -> 
        case input of
            [] -> Left [EndOfInput]
            (hd:rest)
                | predicate hd -> Right (hd, rest)
                | otherwise -> Left [Unexpected hd]    
        )

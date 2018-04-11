module FinRec.Parser(
    parse
) where

import FinRec.Exp
import FinRec.Type
import FinRec.Val
import FinRec.Runtime

import Data.Char ( isDigit, isAlpha, isAlphaNum, isSpace )

newtype Parser t a = Parser ([t] -> ([t], Maybe a))

instance Functor (Parser t) where
    fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative (Parser t) where
    pure a = Parser $ \ ts -> (ts, Just a)
    Parser pf <*> Parser px = Parser (\ ts -> (
        let (ts0, f) = pf ts
            (ts1, x) = px ts0
        in (ts1, f <*> x)
        ))

parse :: String -> [Program]
parse = runParser parseProgramList

runParser :: (Show a, Show t) => Parser t a -> [t] -> a
runParser (Parser p) ts =
    case p ts of
        ([], Just x) -> x 
        (ts0, r) -> runtimeError $ "No parse: " ++ show ts0 ++ " , " ++ show r 

infixl 0 +++

(+++) :: Parser t a -> Parser t a -> Parser t a
Parser p1 +++ Parser p2 =
    Parser $ \ ts ->
        let (ts0, x) = p1 ts
        in case x of
            Nothing -> p2 ts
            Just _ -> (ts0, x)

token :: Eq t => t -> Parser t t
token t =
    Parser $ \ ts ->
        case ts of
            (t0:ts0) | t == t0 -> (ts0, Just t)
            _ -> (ts, Nothing)

ident1 :: (a -> Bool) -> Parser a [a]
ident1 p = 
    Parser $ \ts ->
        if null ts || not (p (head ts))
            then (ts, Nothing)
            else
                ( dropWhile p ts
                , Just (takeWhile p ts)
                )

ident :: (a -> Bool) -> Parser a [a]
ident p = ident1 p +++ pure []
                    
many :: Parser t a -> Parser t [a]
many (Parser p) = Parser go 
    where 
        go ts = 
            let (ts0, x) = p ts
            in case x of
                Nothing -> (ts, Just [])
                Just x0 ->
                    let (ts1, xs) = go ts0
                    in (ts1, (x0:) <$> xs)

sepby1 :: Parser t a -> Parser t b -> Parser t [b]
sepby1 sep p = (:) <$> p <*> many (sep *> p)

sepby :: Parser t a -> Parser t b -> Parser t [b]
sepby sep p = sepby1 sep p +++ pure []

keyword :: Eq a => [a] -> Parser a [a]
keyword k = 
    case k of
        [] -> pure []
        c:cs -> (:) <$> token c <*> keyword cs

parseInt :: Parser Char Int
parseInt = read <$> ident1 isDigit

parseString :: Parser Char String
parseString = keyword "\"" *> ident (/= '"') <* keyword "\""

parseBool :: Parser Char Bool
parseBool = const True <$> keyword "true" +++ 
            const False <$> keyword "false"
 
parseFloat :: Parser Char Float
parseFloat = (\a b c -> read (a ++ b ++ c)) <$> ident1 isDigit <*> keyword "." <*> ident1 isDigit

space :: Parser Char [Char]
space = ident isSpace

parseVal :: Parser Char Exp
parseVal = 
    Literal <$>
    (   Number <$> parseFloat
    +++ Number . fromIntegral <$> parseInt
    +++ Bool <$> parseBool
    +++ String <$> parseString
    )
    <* space

isAlpha' :: Char -> Bool
isAlpha' c = isAlpha c || c == '_'

isAlphaNum' :: Char -> Bool
isAlphaNum' c = isAlphaNum c || c == '_'

parseIdent :: Parser Char String
parseIdent = (++) <$> ident1 isAlpha' <*> ident isAlphaNum'

parseVar :: Parser Char Exp
parseVar = Var <$> (token 'x' *> parseInt) <* space

parseIndex :: Parser Char Exp
parseIndex = Index <$> (token 'i' *> parseInt) <* space

parseApp :: Parser Char Exp
parseApp = Apply <$> parseIdent <* keyword "(" <* space <*> parseExpList <* keyword ")" <* space

parseTuple :: Parser Char Exp
parseTuple = TupleExp <$> (keyword "(" *> space *> parseExpList) <* keyword ")" <* space

parseExpList :: Parser Char [Exp]
parseExpList = sepby (keyword "," <* space) parseExp

parseExpAtom :: Parser Char Exp
parseExpAtom = parseVal +++ parseApp +++ parseVar +++ parseIndex +++ parseTuple

chainl :: (a -> b -> a) -> Parser t a -> Parser t b -> Parser t a
chainl f p1 p2 = foldl f <$> p1 <*> many p2

parseExp :: Parser Char Exp
parseExp = chainl Project parseExpAtom (keyword "." *> parseInt <* space)

parseInputExp :: Parser Char Exp
parseInputExp = const Input <$> keyword "input" <* space +++ parseExp

parseInput :: Parser Char Type
parseInput = parseType <* space

parseType :: Parser Char Type
parseType = 
    const numType <$> keyword "number" +++
    const boolType <$> keyword "bool" +++
    const stringType <$> keyword "string"

parseProgram :: Parser Char Program
parseProgram = Program <$> parseIdent <* space <* keyword ":" <* space <*> many parseInputExp

parseProgramList :: Parser Char [Program]
parseProgramList = space *> many parseProgram

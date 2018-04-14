module FinRec.Parser(
    parse
) where

import FinRec.Exp
import FinRec.Infix
import FinRec.Type
import FinRec.Val
import FinRec.Runtime

import Data.Char ( isDigit, isAlpha, isAlphaNum, isSpace )

updateInfixTableFromProgram :: Program -> InfixTable -> InfixTable
updateInfixTableFromProgram p =
    case programFixity p of
        Nothing -> id
        Just (op, f, prec) -> updateInfixTable op f prec

getInfixTable :: (InfixTable -> Parser t a) -> Parser t a
getInfixTable f = Parser $ \ it ts -> 
    let Parser q = f it
    in q it ts

newtype Parser t a = Parser (InfixTable -> [t] -> ([t], Maybe a))

instance Functor (Parser t) where
    fmap f (Parser p) = Parser $ \it -> fmap (fmap f) . p it

instance Applicative (Parser t) where
    pure a = Parser $ \ _ ts -> (ts, Just a)
    Parser pf <*> Parser px = Parser (\ it ts -> (
        let (ts0, f) = pf it ts
            (ts1, x) = px it ts0
        in (ts1, f <*> x)
        ))

parse :: InfixTable -> String -> [Program]
parse = runParser parseProgramList

runParser :: (Show a, Show t) => Parser t a -> InfixTable -> [t] -> a
runParser (Parser p) it ts =
    case p it ts of
        ([], Just x) -> x 
        (ts0, r) -> runtimeError $ "No parse: " ++ show ts0 ++ " , " ++ show r 

infixl 0 +++

(+++) :: Parser t a -> Parser t a -> Parser t a
Parser p1 +++ Parser p2 =
    Parser $ \ it ts ->
        let (ts0, x) = p1 it ts
        in case x of
            Nothing -> p2 it ts
            Just _ -> (ts0, x)

token :: Eq t => t -> Parser t t
token t =
    Parser $ \ _ ts ->
        case ts of
            (t0:ts0) | t == t0 -> (ts0, Just t)
            _ -> (ts, Nothing)

ident1 :: (a -> Bool) -> Parser a [a]
ident1 p = 
    Parser $ \ _ ts ->
        if null ts || not (p (head ts))
            then (ts, Nothing)
            else
                ( dropWhile p ts
                , Just (takeWhile p ts)
                )

ident :: (a -> Bool) -> Parser a [a]
ident p = ident1 p +++ pure []

manyWith :: (a -> InfixTable -> InfixTable) -> Parser t a -> Parser t [a]
manyWith f (Parser p) = Parser go 
    where 
        go it ts = 
            let (ts0, x) = p it ts
            in case x of
                Nothing -> (ts, Just [])
                Just x0 ->
                    let (ts1, xs) = go (f x0 it) ts0
                    in (ts1, (x0:) <$> xs)

many :: Parser t a -> Parser t [a]
many = manyWith (const id)

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

space :: Parser Char ()
space = const () <$> ident isSpace <* (comment +++ pure ())

comment :: Parser Char ()
comment = const () <$> keyword "#" <* ident (\c -> c /= '\n') <* space

parseVal :: Parser Char Exp
parseVal = 
    Literal <$>
    (   Number <$> parseFloat
    +++ Number . fromIntegral <$> parseInt
    +++ Bool <$> parseBool
    +++ String <$> parseString
    +++ TypeVal <$> parseType
    )
    <* space

isAlpha' :: Char -> Bool
isAlpha' c = isAlpha c || c == '_'

isAlphaNum' :: Char -> Bool
isAlphaNum' c = isAlphaNum c || c == '_'

parseIdent :: Parser Char String
parseIdent = (++) <$> ident1 isAlpha' <*> ident isAlphaNum'

parseInfixOp :: Parser Char String
parseInfixOp = ident1 (`elem` ['?', ':', ';', '.', '\'', '=', '*', '+', '/', '<', '>', '-'])

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

chainr :: (a -> b -> b) -> Parser t a -> Parser t b -> Parser t b
chainr f p1 p2 = flip (foldr f) <$> many p1 <*> p2

parseBinopL :: [(a -> b -> a, String)] -> Parser Char a -> Parser Char b -> Parser Char a
parseBinopL ops p p' = 
    let ps = map (\ (f, op) -> flip f <$> (keyword op *> space *> p' <* space)) ops
    in chainl (flip ($)) p (foldr1 (+++) ps)

parseBinopR :: [(a -> b -> b, String)] -> Parser Char a -> Parser Char b -> Parser Char b
parseBinopR ops p p' = 
    let ps = map (\ (f, op) -> f <$> (p <* keyword op <* space)) ops
    in chainr id (foldr1 (+++) ps) p'

parseBinop :: [(a -> b -> c, String)] -> Parser Char a -> Parser Char b -> Parser Char c
parseBinop ops p p' = 
    let ps = map (\ (f, op) -> (keyword op *> space *> pure f)) ops
    in (\ x f y -> f x y) <$> p <*> (foldr1 (+++) ps) <*> p'

parseBinopL' :: [String] -> Parser Char Exp -> Parser Char Exp -> Parser Char Exp
parseBinopL' ops = parseBinopL (map (\ op -> (\ e0 e1 -> Apply op [e0, e1], op)) ops)

parseBinopR' :: [String] -> Parser Char Exp -> Parser Char Exp -> Parser Char Exp
parseBinopR' ops = parseBinopR (map (\ op -> (\ e0 e1 -> Apply op [e0, e1], op)) ops)

parseBinop' :: [String] -> Parser Char Exp -> Parser Char Exp -> Parser Char Exp
parseBinop' ops = parseBinop (map (\ op -> (\ e0 e1 -> Apply op [e0, e1], op)) ops)

parseProject :: Parser Char Exp
parseProject = parseBinopL [(Project, ".")] parseExpAtom parseInt

parsePrec :: InfixTable -> Parser Char Exp
parsePrec t =
    case t of
        [] -> parseProject
        (_, (Infixl, ops)):t0 -> parseBinopL' ops (parsePrec t0) (parsePrec t0)
        (_, (Infixr, ops)):t0 -> parseBinopR' ops (parsePrec t0) (parsePrec t0)
        (_, (Infix, ops)):t0  -> parseBinop' ops (parsePrec t0) (parsePrec t0)

parseExp :: Parser Char Exp
parseExp = getInfixTable parsePrec

parseInputExp :: Parser Char Exp
parseInputExp = const Input <$> keyword "input" <* space +++ parseExp

parseType :: Parser Char Type
parseType = 
    (
    const numType <$> keyword "number" +++
    const boolType <$> keyword "bool" +++
    const stringType <$> keyword "string" +++
    tupleType <$> (keyword "(" *> space *> many parseType) <* keyword ")"
    ) <* space

parseFixity :: Parser Char Fixity
parseFixity = 
    const Infixl <$> keyword "infixl" +++
    const Infixr <$> keyword "infixr" +++
    const Infix  <$> keyword "infix"

parseBinopDecleration :: Parser Char (String, Maybe (String, Fixity, Int))
parseBinopDecleration = 
    (\ n f p -> (n, Just (n, f, p))) <$> 
    (keyword "binop" *> space *> keyword "(" *> space *> parseInfixOp <* space <* keyword "," <* space) <*>
    parseFixity <* space <* keyword "," <* space <*>
    parseInt <* space <* keyword ")" <* space

parseProgramDecleration :: Parser Char (String, Maybe (String, Fixity, Int))
parseProgramDecleration = 
    parseBinopDecleration +++
    (\ n -> (n ,Nothing)) <$> parseIdent

parseProgram :: Parser Char Program
parseProgram = (\ (n, fi) es -> Program n es fi) <$> parseProgramDecleration <* space <* keyword ":" <* space <*> many parseInputExp

parseProgramList :: Parser Char [Program]
parseProgramList = space *> manyWith updateInfixTableFromProgram parseProgram

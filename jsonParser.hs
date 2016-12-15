

--http://stackoverflow.com/questions/17826616/json-parser-incorrectly-parsing-string-as-a-number

import Data.Char
import Control.Monad
import Control.Applicative
import Control.Monad (liftM, ap)

newtype Parser a = Parser (String -> [(String, a)])

parse :: Parser a -> (String -> [(String, a)])
parse (Parser p) = p

item :: Parser Char
item = Parser (\s ->
    case s of
        []     -> []
        (x:xs) -> [(xs,x)])

failure :: Parser a
failure = Parser (\ts -> [])

produce :: a -> Parser a                                  --parse (item >>= produce) "hello"
produce x = Parser (\ts -> [(ts, x)])

instance Applicative Parser where
    pure x = produce x
    Parser pf <*> Parser px = Parser (\ts -> [ (ts'', f x )| (ts', f) <- pf ts,
                                                            (ts'', x) <- px ts'] )

instance Functor Parser where
    fmap f (Parser px) = Parser (\ts -> [ (ts', f x)  | (ts', x) <- px ts])

instance Monad Parser where
    --return :: a -> Parser a
    return = produce
    --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser px) >>= f = Parser (\ts ->
        concat [parse (f x) ts' | (ts', x) <- px ts])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= (\c ->
    if p c then
    produce c
    else failure)

char :: Char -> Parser Char
char c = satisfy (c == )

string :: String -> Parser String                       --parse (string "hello") "hello"
string [] = produce []
string (c:cs) = char c >>= (\c' ->
                string cs >>= (\cs' ->
                produce (c:cs)))

instance Alternative Parser where
    empty = failure
    (<|>) = orElse
    many p = some p <|> produce []
    some p = (:) <$> p <*> many p

orElse :: Parser a -> Parser a -> Parser a
orElse (Parser px) (Parser py) = Parser (\ts ->
    case px ts of
        [] -> py ts
        xs -> xs)


---------------Parsec bits---------------------------

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (\c -> not (elem c cs))

sepBy ::  Parser a -> Parser String -> Parser [a]
sepBy p sep         = sepBy1 p sep <|> return []

sepBy1 :: Parser a -> Parser String -> Parser [a]
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }

-------------------------------------------------------

data Value = StrJson String
            | IntJson Int
            | BoolJson Bool
            | ObjectJson [Pair]
            | ArrayJson [Value]
            | NullJson
              deriving (Eq, Ord, Show)
--need parens expression

type Pair = (String, Value)

type NullJson = String

tok :: String -> Parser String
tok t = string t <* whitespace

whitespace :: Parser ()
whitespace = many (oneOf " \t") *> pure ()

var :: Parser Char
var = oneOf ['A' .. 'Z'] <* whitespace


val :: Parser Value
val = IntJson <$> jIntParser
    <|> NullJson <$ tok "null"
    <|> BoolJson <$> jBoolParser
    <|> StrJson <$> jStrParser
    <|> ArrayJson <$> jArrParser
    <|> ObjectJson <$> jObjParser


jStrParser :: Parser String
jStrParser = some (noneOf ("\n\r\"=[]{},:")) <* whitespace

jIntParser :: Parser Int
jIntParser = (some (oneOf ['0' .. '9']) >>= produce . read) <* whitespace

jBoolParser :: Parser Bool
jBoolParser = ((string "False" *> produce False) <|> (string "True" *> produce True))

jObjParser :: Parser [Pair]
jObjParser = do
    char '{'
    jp <- jPairParser `sepBy1` (tok ",")
    char '}'
    produce jp

jPairParser :: Parser (String, Value)
jPairParser = do
        jStr <- jStrParser
        tok ":"
        jVal <- val
        produce (jStr, jVal)


jArrParser :: Parser [Value]
jArrParser = do
    char '['
    jArr <- val `sepBy1` (tok ",")
    char ']'
    produce jArr



runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [([], res)] -> res
    [(xs, res)] -> error xs


--run :: String -> Value
--run = runParser val



--main1 = case parse (string "hello") "hello" of
--            [([], res)] -> res
--            _   -> error "Parse error on input."







--end

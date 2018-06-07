module ApplicativeParser where
import Language
import Data.Char (isDigit, isAlpha, isSpace)
import Control.Applicative
import Debug.Trace

{----------------------------------------------------
  PARSER
  ----------------------------------------------------}

-- Nice to have: attach a line number with `type Token = (num, String)`
type Token = String

-- Core lexical analyzer
clex :: String -> [Token]
clex [] = []
clex ('-':'-':cs) = clex (dropWhile (\x -> x /= '\n') cs)                     -- Ignore comments "|| comment"
clex (c:cs)
  | isSpace c = clex cs
  | isDigit c = (token isDigit) : clex (restcs isDigit)                       -- Number token
  | isAlpha c = (token isIdChar) : clex (restcs isIdChar)                     -- Variable token
  | not (null cs) && (c:[y]) `elem` twoCharOps = (c:[y]) : (clex (drop 1 cs)) -- Two charaters operator
  | otherwise = [c] : (clex cs)
  where
    token f = c : takeWhile f cs
    restcs f = dropWhile f cs
    isIdChar a = isAlpha a || isDigit a || (a == '_')                         -- valid variable char
    y = head cs

newtype Parser a = P { parse:: ([Token] -> [(a, [Token])]) }

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inToks -> case parse p inToks of
    [] -> []
    [(v, outToks)] -> [(g v, outToks)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inToks -> [(v, inToks)])

  -- <*> :: Parser (a->b) -> Parser a -> Parser b
  pg <*> px = P (\inToks -> case parse pg inToks of
    [] -> []
    [(g, outToks)] -> parse (fmap g px) outToks)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inToks -> case parse p inToks of
    [] -> []
    [(v, outToks)] -> parse (f v) outToks)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inToks -> case parse p inToks of
    [] -> parse q inToks
    [(v, outToks)] -> [(v, outToks)])

item :: Parser Token
item = P (\inToks -> case inToks of
  [] -> []
  (x:xs) -> [(x,xs)])

pSomeWithSep :: Parser a -> Parser b -> Parser [a]
pSomeWithSep p1 p2 = do
  a <- p1
  do
    p2
    as <- pSomeWithSep p1 p2
    return (a:as)
    <|> return [a]

pSat :: (String -> Bool) -> Parser String
pSat predicate = do
  x <- item
  if predicate x then return x else empty

-- Literal parser
pLit :: String -> Parser String
pLit s = pSat (==s)

pVar :: Parser String
pVar = pSat (\tok -> ((not $ elem tok keywords) && (isAlpha . head) tok))

pNum :: Parser Int
pNum = fmap read (pSat (all isDigit))

pDefns :: Parser [(String, CoreExpr)]
pDefns = pSomeWithSep pDefn (pLit ";")

pDefn :: Parser (String, CoreExpr)
pDefn = pure mk_def <*> pVar <*> (pLit "=") <*> pExpr
  where
    mk_def var _ expr = (var, expr)

pAlters :: Parser [CoreAlt]
pAlters = pSomeWithSep pAlter (pLit ";")

pAlter :: Parser CoreAlt
pAlter = pure mk_Alter <*> pTag <*> (many pVar) <*> (pLit "->") <*> pExpr
  where
    mk_Alter tag args _ expr = (tag, args, expr)
    pTag = pure mk_tag <*> (pLit "<") <*> pNum <*> (pLit ">")
    mk_tag _ tag _ = tag

pLet :: Parser CoreExpr
pLet = pure mk_ELet <*> (pLit "let") <*> pDefns <*> (pLit "in") <*> pExpr
  where
    mk_ELet _ defns _ body = ELet False defns body

pLetrec :: Parser CoreExpr
pLetrec = pure mk_ELetRec <*> (pLit "letrec") <*> pDefns <*> (pLit "in") <*> pExpr
  where
    mk_ELetRec _ defns _ body = ELet True defns body

pCase :: Parser CoreExpr
pCase = pure mk_ECase <*> (pLit "case") <*> pExpr <*> (pLit "of") <*> pAlters
  where
    mk_ECase _ expr _ alters = ECase expr alters

pLam :: Parser CoreExpr
pLam = pure mk_ELam <*> (pLit "\\") <*> (some pVar) <*> (pLit "->") <*> pExpr
  where
    mk_ELam _ args _ expr = ELam args expr

pAExpr :: Parser CoreExpr
pAExpr = pEVar <|> pENum <|> pEContr <|> pPExpr
  where
    pEVar = fmap EVar pVar
    pENum = fmap ENum pNum

    pEContr = pure mk_EConstr <*> pIgnored <*> pNumPair <*> (pLit "}")
    mk_EConstr _ (tag, arity) _ = EConstr tag arity
    pIgnored = pure (\a _ -> a) <*> (pLit "Pack") <*> (pLit "{")
    pNumPair = pure (\x _ y -> (x, y)) <*> pNum <*> (pLit ",") <*> pNum

    pPExpr = pure mk_parens <*> (pLit "(") <*> pExpr <*> (pLit ")") -- Parenthesised expression
    
    mk_parens _ expr _ = expr

pExpr :: Parser CoreExpr
pExpr = pLet <|> pLetrec <|> pCase <|> pLam <|> pAExpr

pSc :: Parser CoreScDefn
pSc = pure mk_sc <*> pVar <*> (many pVar) <*> (pLit "=") <*> pExpr
  where mk_sc name args _ body = (name, args, body)

pProgram :: Parser CoreProgram
pProgram = pSomeWithSep pSc (pLit ";")

syntax :: [Token] -> CoreProgram
syntax = take_first_parse . (parse pProgram)
  where
    take_first_parse [(prog, [])] = prog
    take_first_parse [(_, _)] = error "Unused input"
    take_first_parse [] = error "Syntax error"

parser :: String -> CoreProgram
parser input = syntax toks
  where toks = clex input

{- Examples of usage -}

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") <|> (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pure mk_greeting <*> pHelloOrGoodbye <*> pVar <*> (pLit "!")
  where mk_greeting hg name _ = (hg, name)

pGreetings :: Parser [(String, String)]
pGreetings = many pGreeting

pGreetingsN :: Parser Int
pGreetingsN = fmap length (many pGreeting)

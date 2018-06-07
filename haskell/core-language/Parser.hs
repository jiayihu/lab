module Parser where
import Language
import Data.Char (isDigit, isAlpha, isSpace)
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

type Parser a = [Token] -> [(a, [Token])]


-- Corresponds to `|` symbol in the grammar
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

-- Corresponds to sequencing of symbols in the grammar
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [(combine v1 v2, toks2)| (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks = [(combine v1 v2 v3, toks3)| (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks = [(combine v1 v2 v3 v4, toks4)| (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2, (v4, toks4) <- p4 toks3]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f v1, toks1) | (v1, toks1) <- p toks]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pZeroOrMore $ pThen ignoreA p2 p1)
    where ignoreA _ b = b

pSat :: (String -> Bool) -> Parser String
pSat _ [] = []
pSat predicate (tok:toks) = if predicate tok then [(tok, toks)] else []

-- Literal parser
pLit :: String -> Parser String
pLit s = pSat (==s)

pVar :: Parser String
pVar = pSat (\tok -> ((not $ elem tok keywords) && (isAlpha . head) tok))

pNum :: Parser Int
pNum = (pSat (all isDigit)) `pApply` read

pDefns :: Parser [(String, CoreExpr)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

pDefn :: Parser (String, CoreExpr)
pDefn = pThen3 mk_def pVar (pLit "=") pExpr
  where
    mk_def var _ expr = (var, expr)

pAlters :: Parser [CoreAlt]
pAlters = pOneOrMoreWithSep pAlter (pLit ";")

pAlter :: Parser CoreAlt
pAlter = pThen4 mk_Alter pTag (pZeroOrMore pVar) (pLit "->") pExpr
  where
    mk_Alter tag args _ expr = (tag, args, expr)
    pTag = pThen3 mk_tag (pLit "<") pNum (pLit ">")
    mk_tag _ tag _ = tag

pLet :: Parser CoreExpr
pLet = pThen4 mk_ELet (pLit "let") pDefns (pLit "in") pExpr
  where
    mk_ELet _ defns _ body = ELet False defns body

pLetrec :: Parser CoreExpr
pLetrec = pThen4 mk_ELetRec (pLit "letrec") pDefns (pLit "in") pExpr
  where
    mk_ELetRec _ defns _ body = ELet True defns body

pCase :: Parser CoreExpr
pCase = pThen4 mk_ECase (pLit "case") pExpr (pLit "of") pAlters
  where
    mk_ECase _ expr _ alters = ECase expr alters

pLam :: Parser CoreExpr
pLam = pThen4 mk_ELam (pLit "\\") (pOneOrMore pVar) (pLit "->") pExpr
  where
    mk_ELam _ args _ expr = ELam args expr

pAExpr :: Parser CoreExpr
pAExpr = pEVar `pAlt` pENum `pAlt` pEContr `pAlt` pPExpr
  where
    pEVar = pVar `pApply` EVar
    pENum = pNum `pApply` ENum

    pEContr = pThen3 mk_EConstr pIgnored pNumPair (pLit "}")
    mk_EConstr _ (tag, arity) _ = EConstr tag arity
    pIgnored = pThen (\a _ -> a) (pLit "Pack") (pLit "{")
    pNumPair = pThen3 (\x _ y -> (x, y)) pNum (pLit ",") pNum

    pPExpr = pThen3 mk_parens (pLit "(") pExpr (pLit ")") -- Parenthesised expression
    
    mk_parens _ expr _ = expr

pExpr :: Parser CoreExpr
pExpr = pLet `pAlt` pLetrec `pAlt` pCase `pAlt` pLam `pAlt` pAExpr

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where mk_sc name args _ body = (name, args, body)

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
  where
    take_first_parse ((prog, []) : _) = prog
    take_first_parse (_ : others) = take_first_parse others
    take_first_parse _ = error "Syntax error"

parser :: String -> CoreProgram
parser input = syntax toks
  where toks = clex input

{- Examples of usage -}

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!")
  where mk_greeting hg name _ = (hg, name)

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

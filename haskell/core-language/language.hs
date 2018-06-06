module Language where
import Prelude hiding (seq) 

data Expr a
  = EVar Name                         -- Variables
  | ENum Int                          -- Numbers
  | EConstr Int Int                   -- Constructor tag arity (Branch = Pack{2, 2})
  | EAp (Expr a) (Expr a)             -- Applications
  | ELet IsRec [(a, Expr a)] (Expr a) -- Let isRecursive definitions body
  | ECase (Expr a) [Alter a]          -- Case expression alternatives
  | ELam [a] (Expr a)                 -- Lamba arguments expression
  deriving Show

type Name = String
type IsRec = Bool

type CoreExpr = Expr Name

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

-- Get list of variables from definitions
bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

-- Get list of right-hand sides from definitions
rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]


type Alter a = (Int, [a], Expr a) -- <2> a b = expression
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

-- Supercombinator definitions, consisting of a name, a list of arguments and its body.
-- They seems like just a function with a name, some arguments and a body, with some restrictions ¯\_(ツ)_/¯
-- Info https://wiki.haskell.org/Super_combinator
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram
preludeDefs = [
    ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
    ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
    ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]

programEx :: CoreProgram
programEx = [
    ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
    ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
    ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")),
    ("Branch", [], EConstr 2 2),
    ("three", [], ELet False [("x", ENum 1), ("y", ENum 2)] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))),
    ("isBranch", ["x"], ECase (EAp (EAp (EVar ">") (EVar "x")) (ENum 0)) [
      (1, ["a"], EVar "False"), -- False
      (2, ["l", "r"], EVar "True") -- True
    ]),
    ("isGreater", [], ELam ["x", "y"] (EAp (EAp (EVar ">") (EVar "x")) (EVar "y"))),
    ("infixOperators", [], ELam ["x", "y"] (EAp (EAp (EVar ">") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EAp (EVar "length") (EVar "xs"))))
  ]


{----------------------------------------------------
  PRETTY PRINTER (an example of compiler transformer)
  ----------------------------------------------------}

-- ADT which represents an operation in the printer
data Iseq
  = INil
  | IStr String
  | IAppend Iseq Iseq
  | IIndent Iseq
  | INewLine
  deriving Show

{- Operations on Iseq -}

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr str = IStr str

iNum :: Int -> Iseq
iNum n = iStr (show n)

-- Left-pad the number to given width
-- iFWNum 3 12 = " 12"
iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (spaces (width - length digits) ++ digits)
  where 
    digits = show n
    spaces x = replicate x ' '

iAppend :: Iseq -> Iseq -> Iseq
iAppend INil seq2 = seq2
iAppend seq1 INil = seq1
iAppend seq1 seq2 = IAppend seq1 seq2

iConcat :: [Iseq] -> Iseq
iConcat [] = INil
iConcat (seq:seqs) = seq `iAppend` (iConcat seqs)

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = INil
iInterleave _ (seq:[]) = seq
iInterleave s (seq1:seq2:seqs) = iConcat [seq1, s, iInterleave s (seq2:seqs)]

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map layItem (zip [1..] seqs))
    where layItem (n, seq) = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

iNewline :: Iseq
iNewline = INewLine

iIndent :: Iseq -> Iseq
iIndent seq = IIndent seq

-- Converts from Iseq to String
-- flatten (current column) [(seq, indentation for seq)]
-- Linear in the size of iseq
flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten col ((INil, _):seqs) = flatten col seqs 
flatten col ((IStr s, _) : seqs) = s ++ (flatten col seqs)
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs) -- pushes more work to the list
flatten _ ((INewLine, indent) : seqs) = '\n' : (spaces indent) ++ (flatten indent seqs)
  where spaces n = replicate n ' '
flatten col ((IIndent seq, _) : seqs) = flatten col ((seq, col + 2) : seqs)

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]


{- PRINTERS -}

-- Definition printer
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [iStr ";", iNewline]

pprAlter :: CoreAlt -> Iseq
pprAlter (tag, vars, expr) = iConcat [
    iStr "<",
    iNum tag,
    iStr "> ",
    iInterleave (IStr " ") (map iStr vars),
    iStr " -> ",
    pprExpr expr
  ]

-- Expression printer
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iNum n
pprExpr (EConstr tag arity) = iConcat [iStr "Pack{", pprExpr (ENum tag), iStr ", ", pprExpr (ENum arity), iStr "}"]
pprExpr (EAp (EAp (EVar op) e1) e2) = iConcat [pprAExpr e1, iStr " ", iStr op, iStr " ", pprAExpr e2]
pprExpr (EAp e1 e2) = iConcat [pprExpr e1, iStr " ", pprAExpr e2]
pprExpr (ELet isrec defns expr) =
  iConcat [ 
      iStr keyword, iNewline,
      iStr "  ", iIndent (pprDefns defns), iNewline,
      iStr "in ", pprExpr expr
    ]
  where keyword = if not isrec then "let" else "letrec"
pprExpr (ECase expr alters) = iConcat [
    iStr "case ", pprExpr expr, iStr " of ", iNewline,
    iStr "  ", iIndent (iInterleave sep (map pprAlter alters))
  ]
  where sep = iConcat [iStr ";", iNewline]
pprExpr (ELam args expr) = iConcat [iStr "\\", iInterleave (iStr " ") (map iStr args), iStr " -> ", pprExpr expr]

-- Atomic or composed expression printer
pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise = iConcat [iStr "(", pprExpr e, iStr ")"]

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, args, expr) = iConcat [iStr name, sep, iInterleave (iStr " ") (map iStr args), iStr " = ", pprExpr expr]
  where sep = if null args then iStr "" else iStr " "

pprProgram :: [ScDefn Name] -> Iseq
pprProgram scDefns = iInterleave (iNewline `iAppend` iNewline) (map pprScDefn scDefns)

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)





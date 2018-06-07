module Language where

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
isAtomicExpr (EConstr _ _) = True
isAtomicExpr _ = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

-- Supercombinator definitions, consisting of a name, a list of arguments and its body.
-- They seem like just a function with a name, some arguments and a body, with some restrictions ¯\_(ツ)_/¯
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

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

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

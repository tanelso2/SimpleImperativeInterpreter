module ParseWhile.Grammar where

import qualified Data.Map.Strict as Map

data Const = BoolConst Bool
           | IntConst Integer
           | StringConst String
instance Show Const where
    show c =
        case c of
        BoolConst b -> show b
        IntConst i -> show i
        StringConst s -> show s

data Expr = Var String
          | Monary MonOp Expr
          | Binary BinOp Expr Expr
          | ConstExpr Const
          | ListExpr [Expr]
instance Show Expr where
    show e =
        case e of
        Var s -> s
        Monary op e -> show op ++ show e
        Binary op e1 e2 -> show e1 ++ show op ++ show e2
        ConstExpr c -> show c
        ListExpr l -> show l

data MonOp = Not
           | Neg
           | Abs
           deriving (Show)

data BinOp = And
           | Or
           | Add
           | Subtract
           | Multiply
           | Divide
           | Greater
           | Less
           | Equals
           | GEQ
           | LEQ
           | NotEquals
           | In
           deriving (Show)

data Stmt = Seq [Stmt]
          | Assign String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Skip
          | Print Expr
          | Assert Expr
          deriving (Show)

data Func = Fun String Stmt deriving (Show)

type FuncMap = Map.Map String Func
data Prog = Program FuncMap deriving (Show)

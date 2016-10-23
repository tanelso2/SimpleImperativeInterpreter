module ParseWhile.Grammar where

import qualified Data.Map.Strict as Map

data Const = BoolConst Bool
           | IntConst Integer
           | StringConst String
           deriving (Show)

data Expr = Var String
          | Monary MonOp Expr
          | Binary BinOp Expr Expr
          | ConstExpr Const
          deriving (Show)

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

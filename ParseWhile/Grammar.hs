module ParseWhile.Grammar where

data BExpr = BoolConst Bool
           | BMonary BMonOp BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp IExpr IExpr
           deriving (Show)

data BMonOp = Not deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater
            | Less
            | Equals
            | GEQ
            | LEQ
            | NotEquals
            deriving (Show)

data IExpr = Var String
           | IntConst Integer
           | IMonary IMonOp IExpr
           | IBinary IBinOp IExpr IExpr
           deriving (Show)

data IMonOp = Neg
            | Abs
            deriving (Show)

data IBinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

data Stmt = Seq [Stmt]
          | Assign String IExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          | Print String
          deriving (Show)

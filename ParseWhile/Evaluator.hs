module ParseWhile.Evaluator where

import ParseWhile.Grammar
import qualified Data.Map.Strict as Map

data Val = BoolVal Bool
         | IntVal Integer
         | StringVal String
         deriving (Show)

type Memory = Map.Map String Val

eval :: Prog -> IO Memory
eval (Program funcMap) =
    case Map.lookup "main" funcMap of
    Just (Fun _ stmt) -> evalStmt stmt Map.empty
    Nothing -> error "No main function found in program"

evalStmt :: Stmt -> Memory -> IO Memory
evalStmt stmt m =
    case stmt of
    Seq (x:xs) -> do
        m' <- evalStmt x m
        evalStmt (Seq xs) m'
    Seq [] -> return m
    Assign var exp -> return $ Map.insert var (evalExpr exp m) m
    If exp stmt1 stmt2 ->
        case evalExpr exp m of
        BoolVal True -> evalStmt stmt1 m
        BoolVal False -> evalStmt stmt2 m
        _ -> error $ "Failure to evaluate " ++ show exp ++ " as boolean"
    While exp s ->
        let trueBranch = Seq [s, stmt] in
        let falseBranch = Skip in
        evalStmt (If exp trueBranch falseBranch) m
    Print exp ->
        case evalExpr exp m of
        StringVal s ->
            do putStr s
               return m
        _ -> error "Typechecking should have failed"
    PrintLn exp ->
        case evalExpr exp m of
        StringVal s ->
            do putStr s
               putChar '\n'
               return m
        _ -> error "Typechecking should have failed"
    Skip -> return m
    Assert exp ->
        case evalExpr exp m of
        BoolVal True -> return m
        --TODO: modify so it doesn't print AST
        BoolVal False -> error $ "Assertion error: " ++ show exp ++ " is false"
        _ -> error $ "Failure to evaluate " ++ show exp ++ " as boolean"

evalExpr :: Expr -> Memory -> Val
evalExpr exp m =
    case exp of
    ConstExpr c -> constToVal c
    Var s ->
        case Map.lookup s m of
        Just x -> x
        Nothing -> error $ "Variable " ++ show s ++ " has not been initialized"
    Monary op exp ->
        let v = evalExpr exp m in
        applyMonOp op v
    Binary op exp1 exp2 ->
        let v1 = evalExpr exp1 m in
        let v2 = evalExpr exp2 m in
        applyBinOp op v1 v2

applyMonOp :: MonOp -> Val -> Val
applyMonOp op v =
    case (op,v) of
    (Not,BoolVal b) -> BoolVal $ not b
    (Neg,IntVal i) -> IntVal $ -i
    (Abs,IntVal i) -> IntVal $ abs i
    _ -> error "Typechecking failed. God help us all"

applyBinOp :: BinOp -> Val -> Val -> Val
applyBinOp op v1 v2 =
    case (op,v1,v2) of
    (And,BoolVal b1, BoolVal b2) -> BoolVal $ b1 && b2
    (Or,BoolVal b1, BoolVal b2) -> BoolVal $ b1 || b2
    (Add,IntVal i1, IntVal i2) -> IntVal $ i1 + i2
    (Subtract,IntVal i1, IntVal i2) -> IntVal $ i1 - i2
    (Multiply,IntVal i1, IntVal i2) -> IntVal $ i1 * i2
    (Divide,IntVal i1, IntVal i2) -> IntVal $ i1 `div` i2
    (Greater,IntVal i1, IntVal i2) -> BoolVal $ i1 > i2
    (Less,IntVal i1, IntVal i2) -> BoolVal $ i1 < i2
    (GEQ,IntVal i1, IntVal i2) -> BoolVal $ i1 >= i2
    (LEQ,IntVal i1, IntVal i2) -> BoolVal $ i1 <= i2
    (Equals,IntVal i1, IntVal i2) -> BoolVal $ i1 == i2
    (NotEquals,IntVal i1, IntVal i2) -> BoolVal $ i1 /= i2
    _ -> error "Typechecking failed. God help us all"

constToVal :: Const -> Val
constToVal c =
    case c of
    BoolConst b -> BoolVal b
    IntConst i -> IntVal i
    StringConst s -> StringVal s

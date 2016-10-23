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
    Just (Fun _ stmt) -> eval_stmt stmt Map.empty
    Nothing -> error $ "No main function found in program"

eval_stmt :: Stmt -> Memory -> IO Memory
eval_stmt stmt m =
    case stmt of
    Seq (x:xs) -> do
        m' <- eval_stmt x m
        eval_stmt (Seq xs) m'
    Seq [] -> return m
    Assign var exp -> return $ Map.insert var (eval_exp exp m) m
    If exp stmt1 stmt2 ->
        case eval_exp exp m of
        BoolVal True -> eval_stmt stmt1 m
        BoolVal False -> eval_stmt stmt2 m
        _ -> error $ "Failure to evaluate " ++ show exp ++ " as boolean"
    While exp s ->
        let while_true = Seq [s, stmt] in
        let while_false = Skip in
        eval_stmt (If exp while_true while_false) m
    Print exp ->
        case eval_exp exp m of
        StringVal s ->
            do putStr s
               return m
        _ -> error $ "Typechecking should have failed"
    PrintLn exp ->
        case eval_exp exp m of
        StringVal s ->
            do putStr s
               putChar '\n'
               return m
        _ -> error $ "Typechecking should have failed"
    Skip -> return m
    Assert exp ->
        case eval_exp exp m of
        BoolVal True -> return m
        --TODO: modify so it doesn't print AST
        BoolVal False -> error $ "Assertion error: " ++ show exp ++ " is false"
        _ -> error $ "Failure to evaluate " ++ show exp ++ " as boolean"

eval_exp :: Expr -> Memory -> Val
eval_exp exp m =
    case exp of
    ConstExpr c -> constToVal c
    Var s ->
        case Map.lookup s m of
        Just x -> x
        Nothing -> error $ "Variable " ++ show s ++ " has not been initialized"
    Monary op exp ->
        let v = eval_exp exp m in
        apply_mon_op op v
    Binary op exp1 exp2 ->
        let v1 = eval_exp exp1 m in
        let v2 = eval_exp exp2 m in
        apply_bin_op op v1 v2

apply_mon_op :: MonOp -> Val -> Val
apply_mon_op op v =
    case (op,v) of
    (Not,BoolVal b) -> BoolVal $ not b
    (Neg,IntVal i) -> IntVal $ -i
    (Abs,IntVal i) -> IntVal $ abs i
    _ -> error $ "Typechecking failed. God help us all"

apply_bin_op :: BinOp -> Val -> Val -> Val
apply_bin_op op v1 v2 =
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
    _ -> error $ "Typechecking failed. God help us all"

constToVal :: Const -> Val
constToVal c =
    case c of
    BoolConst b -> BoolVal b
    IntConst i -> IntVal i
    StringConst s -> StringVal s

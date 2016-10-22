module ParseWhile.Evaluator where

import ParseWhile.Grammar
import qualified Data.Map.Strict as Map

data Val = BoolVal Bool
         | IntVal Integer
         deriving (Show)

type Memory = Map.Map String Val

eval :: Stmt -> IO Memory
eval s = eval_stmt s Map.empty

eval_stmt :: Stmt -> Memory -> IO Memory
eval_stmt stmt m =
    case stmt of
    Seq (x:xs) -> do
        m' <- eval_stmt x m
        eval_stmt (Seq xs) m'
    Seq [] -> return m
    Assign var iexp -> return $ Map.insert var (eval_iexp iexp m) m
    If bexp stmt1 stmt2 ->
        case eval_bexp bexp m of
        BoolVal True -> eval_stmt stmt1 m
        BoolVal False -> eval_stmt stmt2 m
        _ -> error $ "Failure to evaluate " ++ show bexp ++ " as boolean"
    While bexp s ->
        let while_true = Seq [s, stmt] in
        let while_false = Skip in
        eval_stmt (If bexp while_true while_false) m
    Print s -> do
        putStr s
        return m
    Skip -> return m

eval_iexp :: IExpr -> Memory -> Val
eval_iexp exp m =
    case exp of
    IntConst i -> IntVal i
    Var s ->
        case Map.lookup s m of
        Just x -> x
        Nothing -> error $ "Variable " ++ show s ++ " has not been initialized"
    IMonary op iexp ->
        let v = eval_iexp iexp m in
        apply_imon_op op v
    IBinary op exp1 exp2 ->
        let v1 = eval_iexp exp1 m in
        let v2 = eval_iexp exp2 m in
        apply_ibin_op op v1 v2

apply_imon_op :: IMonOp -> Val -> Val
apply_imon_op op (IntVal i) =
    IntVal $ case op of
             Neg -> -i
             Abs -> abs i
apply_imon_op op _ = error $ "Cannot apply " ++ show op ++ " to non integer value"

apply_ibin_op :: IBinOp -> Val -> Val -> Val
apply_ibin_op op (IntVal i1) (IntVal i2) =
    IntVal $ case op of
             Add -> i1 + i2
             Subtract -> i1 - i2
             Multiply -> i1 * i2
             Divide -> i1 `div` i2
apply_ibin_op op _ _ = error $ "Cannot apply " ++ show op ++ " to non integer values"

eval_bexp :: BExpr -> Memory -> Val
eval_bexp exp m =
    case exp of
    BoolConst b -> BoolVal b
    BMonary op bexp ->
        let v = eval_bexp bexp m in
        apply_bmon_op op v
    BBinary op exp1 exp2 ->
        let v1 = eval_bexp exp1 m in
        let v2 = eval_bexp exp2 m in
        apply_bbin_op op v1 v2
    RBinary op exp1 exp2 ->
        let v1 = eval_iexp exp1 m in
        let v2 = eval_iexp exp2 m in
        apply_rbin_op op v1 v2

apply_bmon_op :: BMonOp -> Val -> Val
apply_bmon_op op (BoolVal b) =
    BoolVal $ case op of
              Not -> not b
apply_bmon_op op _ = error $ "Cannot apply " ++ show op ++ " to non boolean value"

apply_bbin_op :: BBinOp -> Val -> Val -> Val
apply_bbin_op op (BoolVal b1) (BoolVal b2) =
    BoolVal $ case op of
              And -> b1 && b2
              Or -> b1 || b2
apply_bbin_op op _ _ = error $ "Cannot apply " ++ show op ++ " to non boolean values"

apply_rbin_op :: RBinOp -> Val -> Val -> Val
apply_rbin_op op (IntVal i1) (IntVal i2) =
    BoolVal $ case op of
              Greater -> i1 > i2
              Less -> i1 < i2
              Equals -> i1 == i2
              GEQ -> i1 >= i2
              LEQ -> i1 <= i2
              NotEquals -> i1 /= i2
apply_rbin_op op _ _ = error $ "Cannot apply " ++ show op ++ " to non integer values"

module ParseWhile.TypeChecker where

import ParseWhile.Grammar

import qualified Data.Map.Strict as Map

data Type = BoolType | IntType | StringType deriving (Eq, Show)
type TypeMemory = Map.Map String Type

typecheck :: Prog -> Map.Map String TypeMemory
typecheck (Program funcMap) = Map.map typecheckFunc funcMap

typecheckFunc :: Func -> TypeMemory
typecheckFunc (Fun _ stmt) = typecheckStmt stmt Map.empty

typecheckStmt :: Stmt -> TypeMemory -> TypeMemory
typecheckStmt stmt m =
    case stmt of
    Seq [] -> m
    Seq (x:xs) ->
        let m' = typecheckStmt x m in
        typecheckStmt (Seq xs) m'
    Assign var expr -> Map.insert var (typecheckExpr expr m) m
    If expr stmt1 stmt2 ->
        case typecheckExpr expr m of
        BoolType ->
            let m1 = typecheckStmt stmt1 m in
            let m2 = typecheckStmt stmt2 m in
            if m1 == m2 then m1 else error "Branches in if do not match on assignments and types"
        _ -> error $ "Expression " ++ show expr ++ " is not a boolean"
    While expr stmt ->
        case typecheckExpr expr m of
        BoolType ->
            let m' = typecheckStmt stmt m in
            if m == m' then m else error "Branches in while do not match on assignments and types"
        _ -> error $ "Expression " ++ show expr ++ " is not a boolean"
    Skip -> m
    Print expr ->
        case typecheckExpr expr m of
        StringType -> m
        _ -> error $ "Expression " ++ show expr ++ " is not a string"
    PrintLn expr ->
        case typecheckExpr expr m of
        StringType -> m
        _ -> error $ "Expression " ++ show expr ++ " is not a string"
    Assert expr ->
        case typecheckExpr expr m of
        BoolType -> m
        _ -> error $ "Expression " ++ show expr ++ " is not a boolean"

typecheckExpr :: Expr -> TypeMemory -> Type
typecheckExpr expr m =
    case expr of
    ConstExpr c -> constToType c
    Var var ->
        case Map.lookup var m of
        Just t -> t
        Nothing -> error $ "Variable " ++ show var ++ " has not been initialized"
    Binary op expr1 expr2 ->
        let t1 = typecheckExpr expr1 m in
        let t2 = typecheckExpr expr2 m in
        typecheckBinOp op t1 t2
    Monary op expr ->
        let t = typecheckExpr expr m in
        typecheckMonOp op t

typecheckBinOp :: BinOp -> Type -> Type -> Type
typecheckBinOp op t1 t2 =
    case (op,t1,t2) of
    (Add, IntType, IntType) -> IntType
    (Subtract, IntType, IntType) -> IntType
    (Multiply, IntType, IntType) -> IntType
    (Divide, IntType, IntType) -> IntType
    (Greater, IntType, IntType) -> BoolType
    (Less, IntType, IntType) -> BoolType
    (GEQ, IntType, IntType) -> BoolType
    (LEQ, IntType, IntType) -> BoolType
    (Equals, IntType, IntType) -> BoolType
    (NotEquals, IntType, IntType) -> BoolType
    (And, BoolType, BoolType) -> BoolType
    (Or, BoolType, BoolType) -> BoolType
    _ -> error $ show op ++ " can not be applied to types " ++ show t1 ++ " and " ++ show t2

typecheckMonOp :: MonOp -> Type -> Type
typecheckMonOp op t =
    case (op,t) of
    (Not, BoolType) -> BoolType
    (Neg, IntType) -> IntType
    (Abs, IntType) -> IntType
    _ -> error $ show op ++ " can not be applied to type " ++ show t

constToType :: Const -> Type
constToType c =
    case c of
    BoolConst _ -> BoolType
    IntConst _ -> IntType
    StringConst _ -> StringType

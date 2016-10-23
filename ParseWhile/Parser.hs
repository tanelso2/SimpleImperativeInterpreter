module ParseWhile.Parser where

import ParseWhile.Grammar
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map.Strict as Map

languageDef =
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "not"
                                      , "and"
                                      , "or"
                                      , "abs"
                                      , "print"
                                      , "println"
                                      , "def"
                                      , "assert"
                                      ]
             , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                       , "<", ">", "and", "or", "not"
                                       , "==", "<=", ">=", "!=", "abs"
                                       ]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
stringLiteral = Token.stringLiteral lexer
braces = Token.braces lexer
commaSep = Token.commaSep lexer
semiSep1 = Token.semiSep1 lexer


whileParser :: Parser Prog
whileParser = whiteSpace >> program

program :: Parser Prog
program =
    do list <- sepBy1 function whiteSpace
       return $ Program $ foldl funcNameFolder Map.empty list
    where funcNameFolder m f@(Fun name _) = Map.insert name f m

function :: Parser Func
function =
    do reserved "def"
       name <- identifier
       parens $ commaSep identifier
       body <- braces statement
       return $ Fun name body

statement :: Parser Stmt
statement = parens statement
            <|> sequenceOfStmt

sequenceOfStmt =
    do list <- endBy1 statement' semi
       return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = ifStmt
            <|> whileStmt
            <|> skipStmt
            <|> assignStmt
            <|> printStmt
            <|> assertStmt

printStmt :: Parser Stmt
printStmt =
    (do reserved "print"
        expr <- expression
        return $ Print expr)
    <|>
    (do reserved "println"
        expr <- expression
        return $ PrintLn expr)

ifStmt :: Parser Stmt
ifStmt =
    do reserved "if"
       cond <- expression
       reserved "then"
       stmt1 <- statement
       reserved "else"
       stmt2 <- statement
       return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
    do reserved "while"
       cond <- expression
       reserved "do"
       stmt <- braces statement
       return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
    do var <- identifier
       reservedOp "="
       expr <- expression
       return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt =
    do reserved "skip"
       return Skip

assertStmt :: Parser Stmt
assertStmt =
    do reserved "assert"
       cond <- expression
       return $ Assert cond

expression :: Parser Expr
expression = buildExpressionParser operators term

operators = [
                [
                    Prefix (reservedOp "-" >> return (Monary Neg)),
                    Prefix (reservedOp "abs" >> return (Monary Abs)),
                    Prefix (reservedOp "not" >> return (Monary Not))
                ],
                [
                    Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft,
                    Infix (reservedOp "/" >> return (Binary Divide)) AssocLeft
                ],
                [
                    Infix (reservedOp "+" >> return (Binary Add)) AssocLeft,
                    Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft
                ],
                [
                    Infix (reservedOp "and" >> return (Binary Add)) AssocLeft,
                    Infix (reservedOp "or" >> return (Binary Subtract)) AssocLeft
                ],
                [
                    Infix (reservedOp "<" >> return (Binary Less)) AssocLeft,
                    Infix (reservedOp ">" >> return (Binary Greater)) AssocLeft,
                    Infix (reservedOp "<=" >> return (Binary LEQ)) AssocLeft,
                    Infix (reservedOp ">=" >> return (Binary GEQ)) AssocLeft,
                    Infix (reservedOp "==" >> return (Binary Equals)) AssocLeft,
                    Infix (reservedOp "!=" >> return (Binary NotEquals)) AssocLeft
                ]
            ]

term = parens expression
        <|> liftM Var identifier
        <|> (do i <- integer
                return $ ConstExpr $ IntConst i)
        <|> (do reserved "true"
                return $ ConstExpr $ BoolConst True)
        <|> (do reserved "false"
                return $ ConstExpr $ BoolConst False)
        <|> (do str <- stringLiteral
                return $ ConstExpr $ StringConst str)

parseString :: String -> Prog
parseString str =
    case parse whileParser "" str of
        Left e -> error $ show e
        Right r -> r

parseFile :: String -> IO Prog
parseFile file =
    do result <- parseFromFile whileParser file
       case result of
           Left e -> print e >> fail "parse error"
           Right r -> return r

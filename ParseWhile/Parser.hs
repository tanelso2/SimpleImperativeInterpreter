module ParseWhile.Parser where

import ParseWhile.Grammar
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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


whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
            <|> sequenceOfStmt

sequenceOfStmt =
    do list <- (sepBy1 statement' semi)
       return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = ifStmt
            <|> whileStmt
            <|> skipStmt
            <|> assignStmt
            <|> printStmt

printStmt :: Parser Stmt
printStmt =
    do reserved "print"
       str <- stringLiteral
       return $ Print str

ifStmt :: Parser Stmt
ifStmt =
    do reserved "if"
       cond <- parens bExpression
       reserved "then"
       stmt1 <- statement
       reserved "else"
       stmt2 <- statement
       return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
    do reserved "while"
       cond <- parens bExpression
       reserved "do"
       stmt <- braces statement
       return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
    do var <- identifier
       reservedOp "="
       expr <- iExpression
       return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt =
    do reserved "skip"
       return Skip

iExpression :: Parser IExpr
iExpression = buildExpressionParser iOperators iTerm

iOperators = [
                [
                    Prefix (reservedOp "-" >> return (IMonary Neg)),
                    Prefix (reservedOp "abs" >> return (IMonary Abs))
                ],
                [
                    Infix (reservedOp "*" >> return (IBinary Multiply)) AssocLeft,
                    Infix (reservedOp "/" >> return (IBinary Divide)) AssocLeft
                ],
                [
                    Infix (reservedOp "+" >> return (IBinary Add)) AssocLeft,
                    Infix (reservedOp "-" >> return (IBinary Subtract)) AssocLeft
                ]
             ]

iTerm = parens iExpression
        <|> liftM Var identifier
        <|> liftM IntConst integer

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

bOperators = [
                [
                    Prefix (reservedOp "not" >> return (BMonary Not))
                ],
                [
                    Infix (reservedOp "and" >> return (BBinary And)) AssocLeft,
                    Infix (reservedOp "or" >> return (BBinary Or)) AssocLeft
                ]
             ]

bTerm = parens bExpression
        <|> (reserved "true" >> return (BoolConst True))
        <|> (reserved "false" >> return (BoolConst False))
        <|> rExpression

rExpression =
    do a1 <- iExpression
       op <- relation
       a2 <- iExpression
       return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater)
           <|> (reservedOp "<" >> return Less)
           <|> (reservedOp "==" >> return Equals)
           <|> (reservedOp "<=" >> return LEQ)
           <|> (reservedOp ">=" >> return GEQ)
           <|> (reservedOp "!=" >> return NotEquals)

parseString :: String -> Stmt
parseString str =
    case parse whileParser "" str of
        Left e -> error $ show e
        Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
    do program <- readFile file
       case parse whileParser "" program of
           Left e -> print e >> fail "parse error"
           Right r -> return r
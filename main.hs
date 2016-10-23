import ParseWhile.Evaluator
import ParseWhile.Parser
import ParseWhile.TypeChecker

import Control.Exception (evaluate)

import System.Environment

main = do
    (filename:_) <- getArgs
    adt <- parseFile filename
    evaluate $ typecheck adt
    eval adt

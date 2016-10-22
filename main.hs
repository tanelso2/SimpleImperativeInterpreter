import ParseWhile.Evaluator
import ParseWhile.Parser

import System.Environment

main = do
    (filename:_) <- getArgs
    adt <- parseFile filename
    eval adt

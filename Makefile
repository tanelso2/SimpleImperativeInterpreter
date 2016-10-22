COMPILER=ghc
FLAGS=-Werror -fwarn-incomplete-patterns

main: main.hs ParseWhile/Evaluator.hs ParseWhile/Parser.hs ParseWhile/Grammar.hs
	$(COMPILER) $(FLAGS) main.hs

check:
	ls examples/*.wh | xargs -l --verbose ./main

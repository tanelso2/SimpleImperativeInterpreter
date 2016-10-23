COMPILER=ghc
FLAGS=-Werror -fwarn-incomplete-patterns

main: main.hs ParseWhile/Evaluator.hs ParseWhile/Parser.hs ParseWhile/Grammar.hs ParseWhile/TypeChecker.hs
	$(COMPILER) $(FLAGS) main.hs

check:
	ls examples/*.wh | xargs -l --verbose ./main

clean:
	rm -f *.o *.hi ParseWhile/*.o ParseWhile/*.hi main

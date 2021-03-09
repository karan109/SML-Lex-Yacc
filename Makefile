all:
	mlyacc formula.yacc
	mllex formula.lex
	mlton a2.mlb
run:
	./a2 input.txt
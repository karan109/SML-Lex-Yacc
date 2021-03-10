all:
	mlyacc formula.yacc
	mllex formula.lex
	mlton a2.mlb
run:
	./a2 input.txt
clean:
	rm formula.lex.sml
	rm formula.yacc.desc
	rm formula.yacc.sig
	rm formula.yacc.sml
	rm a2

all: lexer.x parser.hs 
		alex lexer.x
		ghc -o ./a2 parser.hs
run:
	.\a2.exe $(filename) 

clean: 
	rm -rf ./Lexer.hi ./Lexer.hs ./Lexer.o ./parser.exe ./parser.hi ./parser.o ./a2.exe
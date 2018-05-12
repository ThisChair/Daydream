all: daydream

Lexer.hs : Lexer.x
	alex Lexer.x

daydream : Lexer.hs
	ghc -o daydream --make -main-is Daydream Daydream.hs

clean :
	rm -f daydream Lexer.hs *.o *.hi *~

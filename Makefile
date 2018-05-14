all: daydream

Lexer.hs : Lexer.x
	alex Lexer.x

Parser.hs : Parser.y
	happy Parser.y

daydream : Lexer.hs Parser.hs
	ghc -o daydream --make -main-is Daydream Daydream.hs

clean :
	rm -f daydream Lexer.hs Parser.hs  *.o *.hi *~

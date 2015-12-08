main : configure kvstore

configure :
	cabal update && cabal install network aeson-0.9.0.1 utf8-string random

kvstore :
	ghc RawServer.hs -o 3700kvstore && mv 3700kvstore project5

clean :
	rm Server.hi Server.o RawServer.hi RawServer.o 3700kvstore Message.hi Message.o

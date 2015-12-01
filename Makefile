main : configure kvstore

configure :
	cabal update && cabal install network aeson utf8-string

kvstore :
	ghc RawServer.hs -o 3700kvstore

clean :
	rm Server.hi Server.o RawServer.hi RawServer.o 3700kvstore Message.hi Message.o

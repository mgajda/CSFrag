
all: test

clean:
	find . '(' -iname '*.o' -or -iname '*.hi' ')' -exec rm -f '{}' ';'
	rm -f Test Test2 Test3 MakeDB

test: MakeDB
	bash testit.sh

MakeDB: MakeDB.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded MakeDB
#	ghc --make -rtsopts -threaded MakeDB
#	ghc --make -rtsopts -with-rtsopts=-H2G\ -A2M -threaded MakeDB

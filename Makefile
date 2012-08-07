
all: test

clean:
	find . '(' -iname '*.o' -or -iname '*.hi' -or -iname '*.p_o' -or -iname '*.p_hi' ')' -exec rm -f '{}' ';'
	rm -f Test Test2 Test3 MakeDB

test: MakeDB SearchDB
	bash testit.sh

SearchDB: SearchDB.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded SearchDB

MakeDB: MakeDB.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded MakeDB
#	ghc --make -rtsopts -threaded MakeDB
#	ghc --make -rtsopts -with-rtsopts=-H2G\ -A2M -threaded MakeDB

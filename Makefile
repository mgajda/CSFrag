EXECUTABLES=MakeDB SearchDB TestOuter

all: ${EXECUTABLES} test

clean:
	find . '(' -iname '*.o' -or -iname '*.hi' -or -iname '*.p_o' -or -iname '*.p_hi' ')' -exec rm -f '{}' ';'
	rm -f ${EXECUTABLES} Test Test2 Test3 

test: MakeDB SearchDB
	bash testit.sh

test.prof: SearchDB.prof
	./SearchDB output.db K18_alone_shifty.csv +RTS -xc

SearchDB.prof: SearchDB.hs Util.hs Outer.hs Database.hs Util.hs SeqSim.hs
	ghc --make SearchDB.hs -prof -auto-all -threaded -rtsopts -hisuf p_hi -osuf p_o -o SearchDB.prof

SearchDB: SearchDB.hs Util.hs Outer.hs Database.hs Util.hs SeqSim.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded SearchDB

MakeDB: MakeDB.hs Util.hs Database.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded MakeDB
#	ghc --make -rtsopts -threaded MakeDB
#	ghc --make -rtsopts -with-rtsopts=-H2G\ -A2M -threaded MakeDB
TestOuter: Outer.hs TestOuter.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded TestOuter

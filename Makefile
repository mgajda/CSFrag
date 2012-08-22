EXECUTABLES=MakeDB SearchDB TestOuter MakeDBMR CheckSeq

all: ${EXECUTABLES} test

clean:
	find . '(' -iname '*.o' -or -iname '*.hi' -or -iname '*.p_o' -or -iname '*.p_hi' ')' -exec rm -f '{}' ';'
	rm -f ${EXECUTABLES} Test Test2 Test3 

test: MakeDB SearchDB
	bash testit.sh

test.prof: SearchDB.prof
	./SearchDB output.db K18_alone_shifty.csv +RTS -xc

SearchDB.prof: SearchDB.hs Util.hs Outer.hs Database.hs SeqSim.hs
	ghc --make SearchDB.hs -prof -auto-all -threaded -rtsopts -hisuf p_hi -osuf p_o -o SearchDB.prof

SearchDB: SearchDB.hs Util.hs Outer.hs Database.hs SeqSim.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded SearchDB

MakeDB: MakeDB.hs Util.hs Database.hs DatabaseCreation.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded MakeDB
#	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M\ -k16 -threaded MakeDB
#	ghc --make -rtsopts -with-rtsopts=-H2G\ -A2M -threaded MakeDB

MakeDBMR: MakeDBMR.hs Util.hs Database.hs DatabaseCreation.hs

TestOuter: Outer.hs TestOuter.hs
	ghc --make -rtsopts -with-rtsopts=-H64M\ -A2M -threaded TestOuter

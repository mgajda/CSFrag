
clean:
	find . '(' -iname '*.o' -or -iname '*.hi' ')' -exec rm -f '{}' ';'
	rm -f Test Test2 Test3 MakeDB

test:
	bash testit.sh
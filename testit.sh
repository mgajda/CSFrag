#!/bin/bash

#time ./Test1
#time ./Test2 K18_alone_shifty.csv
#time ./Test3 data/seqsim.txt
#time ./MakeDB smallest.str smallest_gap.str output.db
#time ./MakeDB smallest.str largest.str output.db
#time ./SearchDB output.db K18_alone_shifty.csv #+RTS -N1
time ./SearchDB small.db K18_alone_shifty.csv #+RTS -N1

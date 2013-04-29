#!/bin/bash

#time ./Test1
#time ./Test2 K18_alone_shifty.csv
#time ./Test3 data/seqsim.txt
#time ./MakeDB smallest.str smallest_gap.str output.db
#time ./MakeDB smallest.str largest.str output.db
#time ./SearchDB small.db K18_alone_shifty.csv
#time ./MakeDB ubq.str ubq.db
#time ./SearchDB ubq.db ubq.csv
time ./MakeDB ubqs/*.str ubqs.db
time ./SearchDB ubqs.db ubq.csv

cut -f-3   -d, ubq.csv > ubq_HA.csv
cut -f-2,4 -d, ubq.csv > ubq_CA.csv
cut -f-2,5 -d, ubq.csv > ubq_CB.csv
cut -f-2,6 -d, ubq.csv > ubq_CO.csv
cut -f-2,7 -d, ubq.csv > ubq_N.csv
cut -f-2,8 -d, ubq.csv > ubq_HN.csv

# For seeing all the components
for AT in HA CA CB CO N HN; do
  dist/build/SearchDB/SearchDB ubq_2k25.db ubq_${AT}.csv &&\
  gnuplot -e 'plot "matrix.out" matrix w image' --persist;
done

#cabal build
for AT in HA CA HA_CA; do
  dist/build/SearchDB/SearchDB ubq_2k25.db ubqStart_${AT}.csv ; mv matrix.out matrix_${AT}.out;
done


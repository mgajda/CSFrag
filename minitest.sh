cabal build &&
dist/build/SearchDB/SearchDB ubq_2k25.db ubqStart.csv &&
dist/build/CheckSeq/CheckSeq ubq_2k25.db &&
gnuplot -e 'plot "matrix.out" matrix w image' --persist

#!/bin/bash

N=10000000
COMPILE_FLAGS="-O2 -threaded"

echo "* Creating tmp file"
tmpfile=$(mktemp /tmp/haskell_survival_kit_benchmark.XXXXXXXXX)

echo "* Generating $N numbers"
./generate_numbers $N > $tmpfile

echo "* Compiling run_regular.hs"
ghc $COMPILE_FLAGS run_regular.hs

echo "* Compiling run_io.hs"
ghc $COMPILE_FLAGS ../io.hs ../io.c run_io.hs

echo "* Compiling run_parser.hs"
ghc $COMPILE_FLAGS ../parser.hs run_parser.hs

echo "* Running run_regular"
time ./run_regular < $tmpfile

echo "* Running run_io"
time ./run_io < $tmpfile

echo "* Running run_parser"
time ./run_parser < $tmpfile

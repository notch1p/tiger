#!/bin/bash
PREFIX="tiger-src/testcases"
dune exec tigerlex ${PREFIX}/*
# for i in "${PREFIX}"/*; do
#   echo -e "\033[1m${i}\033[0m"
#   _build/default/bin/lextest.exe <"${i}"
# done

#!/bin/bash

SAT='../../minisat/simp/minisat'
SAT='../../glucose/glucose_2.1/glucose_static'

./encoder.py $1 > temp.dimacs
${SAT} temp.dimacs temp2.dimacs > /dev/null
./decoder.py $1 temp2.dimacs 
rm temp.dimacs temp2.dimacs

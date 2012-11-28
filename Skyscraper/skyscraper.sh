#!/bin/bash

SAT='../../minisat/simp/minisat'
SAT='../../glucose/glucose_2.1/glucose_static'

skyscraper/encoder.py $1 > temp.dimacs
${SAT} temp.dimacs temp2.dimacs > /dev/null
skyscraper/decoder.py $1 temp2.dimacs 
#rm temp.dimacs temp2.dimacs

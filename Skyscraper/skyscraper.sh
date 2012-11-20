#!/bin/bash

MINISAT='../../minisat/simp/minisat'

skyscraper/encoder.py $1 > temp.dimacs
${MINISAT} temp.dimacs temp2.dimacs > /dev/null
skyscraper/decoder.py $1 temp2.dimacs 
rm temp.dimacs temp2.dimacs

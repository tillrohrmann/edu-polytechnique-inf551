#!/bin/bash
MINISAT=../../minisat/simp/minisat
./encoder.py $1 $2 > temp.dimacs
${MINISAT} temp.dimacs temp2.dimacs >/dev/null
./decoder.py $1 temp2.dimacs
C=$?
VAR=$2
while [ $C -ne 0 -a ${VAR} -lt $3 ]; do
    VAR=$((${VAR}+1))
    ./encoder.py $1 ${VAR} > temp.dimacs
    ${MINISAT} temp.dimacs temp2.dimacs >/dev/null
    ./decoder.py $1 temp2.dimacs
    C=$?
done
#rm temp.dimacs temp2.dimacs

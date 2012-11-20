#!/bin/bash
MINISAT=../../minisat/simp/minisat
echo $2
./encoder.py $1 $2 > temp.dimacs
${MINISAT} temp.dimacs temp2.dimacs >/dev/null
./decoder.py $1 temp2.dimacs
C=$?
VAR=$2
while [ $C -ne 0 -a ${VAR} -lt $3 ]; do
    VAR=$((${VAR}+1))
    echo ${VAR}
    ./encoder.py $1 ${VAR} > temp.dimacs
    ${MINISAT} temp.dimacs temp2.dimacs >/dev/null
    ./decoder.py $1 temp2.dimacs
    C=$?
done
#rm temp.dimacs temp2.dimacs

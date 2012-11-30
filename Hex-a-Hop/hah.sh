#!/bin/bash
#SAT=../../minisat/simp/minisat
SAT=../../glucose/glucose_2.1/glucose_static
echo $2
./encoder.py $1 $2 > temp.dimacs
${SAT} temp.dimacs temp2.dimacs >/dev/null
./decoder.py $1 temp2.dimacs $2
C=$?
VAR=$2
while [ $C -ne 0 ] && [ ${VAR} -lt $3 ]; do
    VAR=$((${VAR}+1))
    echo ${VAR}
    ./encoder.py $1 ${VAR} > temp.dimacs
    ${SAT} temp.dimacs temp2.dimacs >/dev/null
    ./decoder.py $1 temp2.dimacs ${VAR}
    C=$?
done
rm temp.dimacs temp2.dimacs

#!/bin/bash
#SAT=minisat2
SAT=../../../glucose/glucose_2.1/glucose_static
echo $2
echo "Encoding..."
time ./encoder $1 $2 temp.dimacs
echo "Solving..."
time ${SAT} temp.dimacs temp2.dimacs >/dev/null
../decoder.py $1 temp2.dimacs $2
C=$?
VAR=$2
while [ $C -ne 0 ] && [ ${VAR} -lt $3 ]; do
    VAR=$((${VAR}+1))
    echo ${VAR}
    echo "Encoding..."
    time ./encoder $1 ${VAR} temp.dimacs
    echo "Solving..."
    time ${SAT} temp.dimacs temp2.dimacs >/dev/null
    ../decoder.py $1 temp2.dimacs ${VAR}
    C=$?
done
rm temp.dimacs temp2.dimacs

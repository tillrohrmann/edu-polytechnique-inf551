#! /bin/bash

if [ $# -ne 1 ]
then
    echo "Usage : $0 <instance_file>"
else
    if ! [ -r $1 ]
    then
	echo "Error : $1 does not exist or is not readable."
    else
	echo "Encoding formula..."
	./skyscraper_encoder $1 formula.dimacs

	echo -e "\nExecuting the SAT solver..."
	minisat formula.dimacs solution.dimacs

	echo -e "\nSolution :"
	./skyscraper_decoder $1 solution.dimacs
    fi
fi

#!/usr/bin/python

'''
Created on Nov 14, 2012

@author: Till Rohrmann
'''

import sys;
import os;
from common import *;

'''
On each field there has to be at least one skyscraper:
clause(x,y) = var(x,y,0) || var(x,y,1) || ....

@param int size
	field size

@return list list int
	list of clauses for each field
'''
def getAtEachFieldAtLeastOneSkyscraper(size):
    clauses = [];
    
#on each field
    for i in range(size):
        for j in range(size):
            clause = [];
#there has to be at least one skyscraper
            for h in range(size):
                clause.append(var(i,j,h,size));
            clauses.append(clause)
            
    return clauses;

'''
On each field there has to be at most one skyscraper:
formula(x,y) = (-var(x,y,0) || -var(x,y,1)) && (-var(x,y,0)||-var(x,y,2) ....

@param int size
	field size

@return list list int
	list of clauses for each field
'''
def getAtEachFieldAtMostOneSkyscraper(size):
    clauses =[];
#on each field
    for i in range(size):
        for j in range(size):
#there can be at most one skyscraper
            for h in range(size-1):
                for k in range(h+1,size):
                    clause = [-var(i,j,h,size),-var(i,j,k,size)];
                    clauses.append(clause);
    return clauses

'''
In each line there can only be one skyscraper of the same type
clause(x,h): (-var(x,y,h) || -var(x,z,h))

@param int size
	field size

@return list list int
	list of clauses
'''
def getInEachLineAtMostOneSimilarSkyscraper(size):
    clauses = [];
    #in each line
    for i in range(size):
        for h in range(size):
#there can only be one skyscraper of the same type
            for j in range(size-1):
                for k in range(j+1,size):
                    clause = [-var(i,j,h,size),-var(i,k,h,size)];
                    clauses.append(clause);
    
    return clauses;
'''
In each column there can only be one skyscraper of the same type
clause(y,h):  (-var(x,y,h) || -var(z,y,h))

@param int size
	field size

@return list list int
	list of clauses
'''
def getInEachColumnAtMostOneSimilarSkyscraper(size):
    clauses = [];
    #in each column
    for j in range(size):
        for h in range(size):
#there can only be one skyscraper of the same type
            for i in range(size-1):
                for k in range(i+1,size):
                    clause = [-var(i,j,h,size),-var(k,j,h,size)];
                    clauses.append(clause)
    return clauses;

def getMaxSkyscraperClauses(size):
    clauses = [];
    
    #maxSkyscraper can only takae one value one each field
    for dir in range(4):
        for x in range(size):
            for y in range(size):
                for u in range(size):
                    for v in range(u+1, size+1):
                        clauses.append([-maxSkyscraper(x, y, u, dir, size), -maxSkyscraper(x, y, v, dir, size)]);
    
    #initialization: initially the maximumSkyscraper takes the value 0 at the borders
    for x in range(size):
        clauses.append([maxSkyscraper(x, 0, 0, WEST, size)])
        clauses.append([maxSkyscraper(x, size-1, 0, EAST, size)])
        clauses.append([maxSkyscraper(0, x,0,  NORTH, size)])
        clauses.append([maxSkyscraper(size-1, x, 0, SOUTH, size)])
    
    #value propagation if the current skyscraper is smaller than maxSkyscraper for this field
    # if(heightSkyscraper(x,y) < maxSkyscraper(x,y)) => maxSkyscraper(x+1,y) = maxSkyscraper(x,y)
    for x in range(size):
        for y in range(size-1):
            for u in range(size):
                for v in range(u+1, size+1):
                    clauses.append([-var(x, y, u, size), -maxSkyscraper(x, y, v, WEST, size), maxSkyscraper(x, y+1, v, WEST, size)])
                    clauses.append([-var(x, y+1, u, size), -maxSkyscraper(x, y+1, v, EAST, size), maxSkyscraper(x, y, v, EAST, size)])
                    clauses.append([-var(y, x, u, size), -maxSkyscraper(y, x, v, NORTH, size), maxSkyscraper(y+1, x, v, NORTH, size)])
                    clauses.append([-var(y+1, x, u, size), -maxSkyscraper(y+1, x, v, SOUTH, size), maxSkyscraper(y, x, v, SOUTH, size)])
    #value propagation if the current skyscraper is greater or equal to maxkSkyscraper for this field
   # if(heightSkyscraper(x,y) >= maxSkyscraper(x,y) ) => maxSkyscraper(x+,1,y) = heightSkyscraper(x,y)+1
    for x in range(size):
        for y in range(size-1):
            for u in range(size+1):
                for v in range(u, size):
                    clauses.append([-var(x, y, v, size), -maxSkyscraper(x, y, u, WEST, size), maxSkyscraper(x, y+1, v+1, WEST, size)])
                    clauses.append([-var(x, y+1, v, size), -maxSkyscraper(x, y+1, u, EAST, size), maxSkyscraper(x, y, v+1, EAST, size)])
                    clauses.append([-var(y, x, v, size), -maxSkyscraper(y, x, u, NORTH, size), maxSkyscraper(y+1, x, v+1, NORTH, size)])
                    clauses.append([-var(y+1, x, v, size), -maxSkyscraper(y+1,x, u, SOUTH, size), maxSkyscraper(y, x, v+1, SOUTH, size)])

    return clauses;
    
def getNumberSkyscraperClauses(size):
    clauses = [];
    
    #numberSkyscraper can only take one value
    for dir in range(4):
        for x in range(size):
            for y in range(size):
                for u in range(size):
                    for v in range(u+1, size+1):
                        clauses.append([-numberSkyscraper(x, y, u, dir, size), -numberSkyscraper(x, y, v, dir, size)]);
    
    #initialization: one at the borders (in the direction of the opposing border) one sees initially 1 skyscraper
    for x in range(size):
        clauses.append([numberSkyscraper(x, size-1, 1, WEST, size)])
        clauses.append([numberSkyscraper(x, 0, 1, EAST, size)])
        clauses.append([numberSkyscraper(size-1, x,1,  NORTH, size)])
        clauses.append([numberSkyscraper(0, x, 1, SOUTH, size)])
    
    #value propagation if no new skyscraper was seen
    #if( heightSkyscraper(x,y) < maxSkyscraper(x,y) ) => numberSkyscraper(x,y) = numberSkyscraper(x-1,y)
    for x in range(size):
        for y in range(1, size):
            for u in range(size):
                for v in range(u+1, size+1):
                    for w in range(size+1):
                        clauses.append([-var(x, y, u, size), -maxSkyscraper(x, y, v, WEST, size), -numberSkyscraper(x, y-1, w, EAST, size), numberSkyscraper(x, y, w, EAST, size)])
                        clauses.append([-var(x, y-1, u, size), -maxSkyscraper(x, y-1, v, EAST, size), -numberSkyscraper(x, y, w, WEST, size), numberSkyscraper(x, y-1, w, WEST, size)])
                        clauses.append([-var(y, x, u, size), -maxSkyscraper(y, x, v, NORTH, size),-numberSkyscraper(y-1, x, w, SOUTH, size),  numberSkyscraper(y, x, w, SOUTH, size)])
                        clauses.append([-var(y-1, x, u, size), -maxSkyscraper(y-1, x, v, SOUTH, size),-numberSkyscraper(y, x, w, NORTH, size),  numberSkyscraper(y-1, x, w, NORTH, size)])
    
    #value propagation if a new skyscraper was seen
    # if(heightSkyscraper(x,y) >= maxSkyscraper(x,y)) => numberSkyscraper(x,y) = numberSkyscraper(x-1,y)+1
    for x in range(size):
        for y in range(1, size):
            for u in range(size):
                for v in range(u, size):
                    for w in range(size):
                        clauses.append([-var(x, y, v, size), -maxSkyscraper(x, y, u, WEST, size), -numberSkyscraper(x, y-1, w, EAST, size),  numberSkyscraper(x, y, w+1, EAST, size)])
                        clauses.append([-var(x, y-1, v, size), -maxSkyscraper(x, y-1, u, EAST, size),-numberSkyscraper(x, y, w, WEST, size) , numberSkyscraper(x, y-1,  w+1, WEST, size)])
                        clauses.append([-var(y, x, v, size), -maxSkyscraper(y, x, u, NORTH, size),-numberSkyscraper(y-1, x, w, SOUTH, size),  numberSkyscraper(y, x, w+1, SOUTH, size)])
                        clauses.append([-var(y-1, x, v, size), -maxSkyscraper(y-1,x, u, SOUTH, size),-numberSkyscraper(y, x, w, NORTH, size),  numberSkyscraper(y-1, x, w+1, NORTH, size)])
    
    return clauses;
    

'''
This function creates the clauses which guarantee the constraints

@param int size
	field size

@param list (int,int)
	constraints: first half of the list represents the row constraints and the second half the column constraints. A 0 indicates that there is no constraint

@return list list int
	list of clauses
'''
def getConstraintFormulas(size, constraints):
    clauses = [];
    
#get clauses for maxSkyscraper predicate
    clauses += getMaxSkyscraperClauses(size);
#get clauses for numberSkyscraper predicate
    clauses += getNumberSkyscraperClauses(size);
    
#set the constraints
    for i in range(0, size):
        lineConstraint = constraints[i];
        columnConstraint = constraints[i+size];
        #if there is a constraint specified for the ith column looking southwards, then the predicate numberSkyscraper(size-1,i,dir)=constraint has to be true
        if(columnConstraint[0] >0):
            clauses.append([numberSkyscraper(size-1, i, columnConstraint[0], SOUTH, size)]);
        #if there is a constraint specified for the ith line looking eastwards, then the predicate numberSkyscraper(i,size-1,dir)=constraint has to be true
        if(lineConstraint[0] >0):
            clauses.append([numberSkyscraper(i, size-1, lineConstraint[0], EAST, size)]);
        #if there is a constraint specified for the ith line looking westwards, then the predicate numberSkyscraper(i,0,dir)=constraint has to be true
        if(lineConstraint[1] > 0):
            clauses.append([numberSkyscraper(i, 0, lineConstraint[1], WEST, size)]);
        #if there is a constraint specified for the ith column looking northwards, then the predicate numberSkyscraper(0,i,dir)=constraint has to be true
        if(columnConstraint[1] >0):
            clauses.append([numberSkyscraper(0, i, columnConstraint[1], NORTH, size)]);

    return clauses;
    
'''
Print a clause in the dimacs format with a trailing 0 to stdout

@param list int clause
'''
def printClause(clause):
    print(" ".join([str(x) for x in clause])+" 0");
    
'''
Print formulas in dimacs format on stdout

@param list list int
	list of clauses. A clause is a list int which contains the variables as int encoded

'''
def printFormulas(clauses):
    numVar =0;
    for clause in clauses:
        temp = [abs(x) for x in clause];
        maxVar = max(temp)
        if(maxVar > numVar):
            numVar = maxVar
    print("p cnf "+str(numVar)+" " + str(len(clauses)));
    for clause in clauses:
        printClause(clause)
    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: <input file>");
        exit(1);
    else:
        inputFile = sys.argv[1];
        size, constraints  = readFile(os.getcwd()+"/"+inputFile)
        atEachFieldAtLeastOneSkyscraper = getAtEachFieldAtLeastOneSkyscraper(size);
        atEachFieldAtMostOneSkyscraper = getAtEachFieldAtMostOneSkyscraper(size);
        inEachLineAtMostOneSimilarSkyscraper = getInEachLineAtMostOneSimilarSkyscraper(size);
        inEachColumnAtMostOneSimilarSkyscraper = getInEachColumnAtMostOneSimilarSkyscraper(size);
        constraintFormulas = getConstraintFormulas(size,constraints);
        
        clauses = atEachFieldAtLeastOneSkyscraper + atEachFieldAtMostOneSkyscraper + inEachColumnAtMostOneSimilarSkyscraper + inEachLineAtMostOneSimilarSkyscraper + constraintFormulas;
        
        printFormulas(clauses)
        
        
    

#!/usr/bin/python

'''
Created on Nov 14, 2012

@author: Till Rohrmann
'''

import sys;
import os;
from common import *;

def getAtEachFieldAtLeastOneSkyscraper(size):
    clauses = [];
    
    for i in range(size):
        for j in range(size):
            clause = [];
            for h in range(size):
                clause.append(var(i,j,h,size));
            clauses.append(clause)
            
    return clauses;

def getAtEachFieldAtMostOneSkyscraper(size):
    clauses =[];
    for i in range(size):
        for j in range(size):
            for h in range(size-1):
                for k in range(h+1,size):
                    clause = [-var(i,j,h,size),-var(i,j,k,size)];
                    clauses.append(clause);
    return clauses

def getInEachLineAtMostOneSimilarSkyscraper(size):
    clauses = [];
    
    for i in range(size):
        for h in range(size):
            for j in range(size-1):
                for k in range(j+1,size):
                    clause = [-var(i,j,h,size),-var(i,k,h,size)];
                    clauses.append(clause);
    
    return clauses;

def getInEachColumnAtMostOneSimilarSkyscraper(size):
    clauses = [];
    
    for j in range(size):
        for h in range(size):
            for i in range(size-1):
                for k in range(i+1,size):
                    clause = [-var(i,j,h,size),-var(k,j,h,size)];
                    clauses.append(clause)
    return clauses;

    
def getMaxSkyscraperClauses(size):
    clauses = [];
    
    for dir in range(4):
        for x in range(size):
            for y in range(size):
                for u in range(size):
                    for v in range(u+1, size+1):
                        clauses.append([-maxSkyscraper(x, y, u, dir, size), -maxSkyscraper(x, y, v, dir, size)]);
    
    #initialization
    for x in range(size):
        clauses.append([maxSkyscraper(x, 0, 0, WEST, size)])
        clauses.append([maxSkyscraper(x, size-1, 0, EAST, size)])
        clauses.append([maxSkyscraper(0, x,0,  NORTH, size)])
        clauses.append([maxSkyscraper(size-1, x, 0, SOUTH, size)])
    
    #value propagation
    for x in range(size):
        for y in range(size-1):
            for u in range(size):
                for v in range(u+1, size+1):
                    clauses.append([-var(x, y, u, size), -maxSkyscraper(x, y, v, WEST, size), maxSkyscraper(x, y+1, v, WEST, size)])
                    clauses.append([-var(x, y+1, u, size), -maxSkyscraper(x, y+1, v, EAST, size), maxSkyscraper(x, y, v, EAST, size)])
                    clauses.append([-var(y, x, u, size), -maxSkyscraper(y, x, v, NORTH, size), maxSkyscraper(y+1, x, v, NORTH, size)])
                    clauses.append([-var(y+1, x, u, size), -maxSkyscraper(y+1, x, v, SOUTH, size), maxSkyscraper(y, x, v, SOUTH, size)])
                    
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
    
    #initialization
    for x in range(size):
        clauses.append([numberSkyscraper(x, size-1, 1, WEST, size)])
        clauses.append([numberSkyscraper(x, 0, 1, EAST, size)])
        clauses.append([numberSkyscraper(size-1, x,1,  NORTH, size)])
        clauses.append([numberSkyscraper(0, x, 1, SOUTH, size)])
    
    #value propagation
    for x in range(size):
        for y in range(1, size):
            for u in range(size):
                for v in range(u+1, size+1):
                    for w in range(size+1):
                        clauses.append([-var(x, y, u, size), -maxSkyscraper(x, y, v, WEST, size), -numberSkyscraper(x, y-1, w, EAST, size), numberSkyscraper(x, y, w, EAST, size)])
                        clauses.append([-var(x, y-1, u, size), -maxSkyscraper(x, y-1, v, EAST, size), -numberSkyscraper(x, y, w, WEST, size), numberSkyscraper(x, y-1, w, WEST, size)])
                        clauses.append([-var(y, x, u, size), -maxSkyscraper(y, x, v, NORTH, size),-numberSkyscraper(y-1, x, w, SOUTH, size),  numberSkyscraper(y, x, w, SOUTH, size)])
                        clauses.append([-var(y-1, x, u, size), -maxSkyscraper(y-1, x, v, SOUTH, size),-numberSkyscraper(y, x, w, NORTH, size),  numberSkyscraper(y-1, x, w, NORTH, size)])
                    
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
    
    
def getConstraintFormulas(size, constraints):
    clauses = [];
    
    clauses += getMaxSkyscraperClauses(size);
    clauses += getNumberSkyscraperClauses(size);
    
    for i in range(0, size):
        lineConstraint = constraints[i];
        columnConstraint = constraints[i+size];
        
        if(columnConstraint[0] >0):
            clauses.append([numberSkyscraper(size-1, i, columnConstraint[0], SOUTH, size)]);
        if(lineConstraint[0] >0):
            clauses.append([numberSkyscraper(i, size-1, lineConstraint[0], EAST, size)]);
        if(lineConstraint[1] > 0):
            clauses.append([numberSkyscraper(i, 0, lineConstraint[1], WEST, size)]);
        if(columnConstraint[1] >0):
            clauses.append([numberSkyscraper(0, i, columnConstraint[1], NORTH, size)]);

    return clauses;
    

def printClause(clause):
    print(" ".join([str(x) for x in clause])+" 0");
    
def printFormulas(size, clauses):
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
        
        printFormulas(size,clauses)
        
        
    

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

def getAllPermutationsHelper(elements):
    if(not elements):
        return [[]];
    else:
        result = [];
        newElements = elements.copy();
        for elem in elements:
            newElements.remove(elem);
            temp = getAllPermutationsHelper(newElements);
            newElements.add(elem);
            t = [[elem] + x for x in temp];
            result = result +t
        return result;

def getAllPermutations(size):
    return getAllPermutationsHelper(set(range(0,size)));
    
def calcVisibility(setting):
    left =0;
    right =0;
    
    llast = -1;
    rlast = -1;
    
    for i in range(len(setting)):
        if(llast < setting[i]):
            llast = setting[i];
            left +=1
        if(rlast < setting[len(setting)-1-i]):
            rlast = setting[len(setting)-1-i];
            right+=1
    
    return ((left,right),setting);

def constructClause(elem,successors,size):
    result = []
    
    for i in range(0,len(elem)):
        result.append(-var(0,i,elem[i],size))
        
    for successor in successors:
        result.append(var(0,len(elem),successor,size))
        
    return result;

def findSuccessor(elem,constraints):
    result = set()
    
    for constraint in constraints:
        if elem == constraint[0:len(elem)]:
            result.add(constraint[len(elem)])
    
    return list(result);
    
def cnfConstraints(selection):
    if not selection:
        return []
    else:
        result = [];
        size = len(selection[0]);
    
        #which elements are not allowed
        for i in range(size):
            temp = set()
            for constraint in selection:
                temp.add(constraint[i]);
            
            diff = set(range(size)) - temp;
            
            for elem in diff:
                result.append([-var(0,i,elem,size)]);
                
        stack = [[x] for x in range(size)];
        
        while not not stack:
            elem = stack.pop();
            if len(elem) < size:
                successors = findSuccessor(elem,selection)
                clause = constructClause(elem,successors,size) 
                result.append(clause)
                
                for succ in successors:
                    nelem = elem + [succ];
                    stack.append(nelem)
                    
        return result
    
def transformClause(clauses, index,isLine,size):
    result = [];
    
    for clause in clauses:
        transformedClause = []
        for term in clause:
            sign,line,column,height = decodeVar(term,size)
            if(isLine):
                line = index
            else:
                line = column;
                column = index;
            transformedClause.append(sign*var(line,column,height,size));
        
        result.append(transformedClause);
        
    return result;

def areEqual(a,b):
    if((a[0] != 0 and b[0]!=0 and a[0] != b[0]) or (a[1] != 0 and b[1] != 0 and a[1]!= b[1]) ):
        return False
    return True

def selectConstraints(constraint,db):
    if(constraint==(0,0)):
        return []
    else:
        result = []
        for entry in db:
            if(areEqual(constraint,entry[0])):
                result.append(entry[1])
            
        return result;
        
def getConstraintFormulas(size,constraints):
    clauses = [];
    permutations = getAllPermutations(size);
    db = [calcVisibility(x) for x in permutations];
    
    for i in range(0,size):
        lineSelection = selectConstraints(constraints[i],db);
        columnSelection = selectConstraints(constraints[i+size],db);
        
        lineClauses = cnfConstraints(lineSelection);
        columnClauses = cnfConstraints(columnSelection);
        
        clauses = clauses+transformClause(lineClauses,i,True,size)+transformClause(columnClauses,i,False,size);
    
    return clauses;
    
    

def printClause(clause):
    print(" ".join([str(x) for x in clause])+" 0");
    
def printFormulas(size, clauses):
    numVar = size**3;
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
        
        
    

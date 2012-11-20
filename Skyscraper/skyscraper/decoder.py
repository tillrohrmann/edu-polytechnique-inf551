#!/usr/bin/python

'''
Created on Nov 14, 2012

@author: Till Rohrmann
'''

import sys;
import common;

def printResult(size, constraints,positives):
    field = [[0 for j in range(size)] for i in range(size)];
    
    for positive in positives:
        sign,line,column,height = common.decodeVar(positive,size)
        field[line][column] = str(height+1)
    
    topLine = []
    bottomLine = []
    
    for i in range(size,2*size):
        topLine.append(str(constraints[i][0]));
        bottomLine.append(str(constraints[i][1]));
        
    print("  "+"|".join(topLine))
    print("  "+"-"*(2*size-1));
    
    for i in range(size):
        print(str(constraints[i][0])+">" +"|".join(field[i])+"<"+str(constraints[i][1]));
    
    print("  "+ "-"*(2*size-1));    
        
    print("  "+ "|".join(bottomLine))
        
    

if __name__ == '__main__':
    if(len(sys.argv) < 3):
        print("Usage: <field> <assignment>")
        exit(1)
    else:
        size,constraints = common.readFile(sys.argv[1])
        variables = common.readResultFile(sys.argv[2])        
        positives = [x for x in variables if x >0];
        printResult(size,constraints,positives)
        
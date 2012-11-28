#!/usr/bin/python

'''
Created on Nov 14, 2012

@author: Till Rohrmann
'''

NORTH = 0
SOUTH=1
WEST=2
EAST=3

counter = 0

def readResultFile(filename):
    file = open(filename)
    result = []
    
    line = file.readline()
    for line in file:
        result = result + [int(x) for x in line.split() if int(x) != 0];
    
    return result;

def readFile(filename):
    file = open(filename);
    size = int(file.readline());
    constraints = [];
    for line in file:
        constraints.append(tuple([int(x) for x in line.split()]));
        
    return (size,constraints)

def var(x,y,h,maxValue):
    return h + maxValue*y + maxValue*maxValue*x +1;
    
def varHelper(x, y, v, size, maxValue):
    return v + maxValue*y + maxValue*size*x+1;
    
def maxSkyscraper(x, y, v, dir, size):
    offset = 1*numberFieldVariables(size) + dir*numberHelperVariables(size, size+1)
    
    return varHelper(x, y, v, size, size+1) + offset
    
def numberSkyscraper(x, y, v, dir, size):
    offset = numberFieldVariables(size) + (4+dir)*numberHelperVariables(size, size+1)
    
    return varHelper(x, y, v, size, size+1) + offset
    
def numberFieldVariables(size):
    return size**3;
    
def numberHelperVariables(size, maxValue):
    return maxValue + maxValue*(size-1) + maxValue*size*(size-1)
    
def helperVariable(size):
    global counter
    offset = numberFieldVariables(size) + 8*numberHelperVariables(size, size+1);
    counter += 1;
    return offset+counter

def decodeVar(value,size):
    sign = 1;
    if(value < 0):
        sign = -1
        value *= -1
    
    value -=1
    
    height = value%size;
    value = int(value/size)
    column = value%size;
    line = int(value/size);
    
    return (sign,line,column,height)

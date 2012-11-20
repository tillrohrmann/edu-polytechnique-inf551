#!/usr/bin/python

'''
Created on Nov 14, 2012

@author: Till Rohrmann
'''


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
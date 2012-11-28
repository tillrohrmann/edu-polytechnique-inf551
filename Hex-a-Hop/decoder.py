#!/usr/bin/python3
'''
Created on Nov 15, 2012

@author: till
'''

import sys
import common

def calcPositions(model,size):
    result = {};
    
    for entry in model:
        (x,y,t) = common.decodeVar(entry,size);
        if t in result:
            print(result)
            print("double entries for time " + str(t)+":(",x,y,t,") (",result[t],t,")");
            sys.exit(1)
        result[t] = (x,y)
    
    keys = list(result.keys());
    
    list.sort(keys)
    
    prev = -1;
    
    for key in keys:
        if key != prev+1:
            print("Temporal interuption between " + str(prev) + " and " + str(key));
            sys.exit(1)
        prev = key
    
    return result;

def separateLine(line):
    firstLine = [];
    secondLine= [];
    
    for i in range(len(line)):
        if(i%2 ==0):
            firstLine.append(line[i]);
            secondLine.append(" ");
        else:
            secondLine.append(line[i]);
            firstLine.append(" ");
            
    return firstLine, secondLine
    
    

def printHistory(field,size,playerPositions):
    for t in range(len(playerPositions)):
        position = playerPositions[t]
        
        for x in range(size[0]):
            line = list(field[x])
            if(x == position[0]):
                line[position[1]] = '*';
            firstLine,secondLine = separateLine(line);
            print(" ".join(firstLine));
            print(" ".join(secondLine));
            
            if(x == position[0] and common.isDestroyable(common.getElem(position, field, size))):
                field[position[0]][position[1]]=common.destroy(common.getElem(position, field, size))
                
        input()

if __name__ == '__main__':
    if(len(sys.argv) < 3):
        print("Usage: <map file> <result file> <steps>")
        exit(1)
    else:
        (size,field,pos) = common.readMap(sys.argv[1]);
        model = common.readResultFile(sys.argv[2]);
        model = [x for x in model if x > 0];
        steps = int(sys.argv[3]);
        
        model = model[0:steps+1];
        
        playerPositions = calcPositions(model,size);
        
        printHistory(field,size,playerPositions)
        
        exit(0)

#!/usr/bin/python
'''
Created on Nov 15, 2012

@author: till
'''

import sys;
from common import *;

def verifyStart(start,field):
    return field[start[0]][start[1]] != CONST_WATER

def getStateClauses(field,size,timeSteps):
    result = [];
    for t in range(timeSteps+1):
        clause = [];
        #at every timestep, the player has to be somewhere
        for x in range(size[0]):
            for y in range(size[1]):
                if(isAccessible(field[x][y])):
                    clause.append(encodeVar(x,y,t,size));
        
        result.append(clause);
        
        #but he cannot be at two positions at the same time
        for x in range(size[0]):
            for y in range(size[1]):
                if(isAccessible(field[x][y])):
                    for j in range(y+1,size[1]):
                        if(isAccessible(field[x][j])):
                            result.append([-encodeVar(x,y,t,size),-encodeVar(x,j,t,size)]);
                            
                    for i in range(x+1,size[0]):
                        for j in range(size[1]):
                            if(isAccessible(field[i][j])):
                                result.append([-encodeVar(x,y,t,size),-encodeVar(i,j,t,size)]);
        
    return result;

def getAccessibleNeighbours(position,field,size):
    result = [];
        
    directions = [CONST_NORTH,CONST_SOUTH,CONST_NORTHWEST,CONST_SOUTHWEST,CONST_NORTHEAST,CONST_SOUTHEAST];
    
    for direction in directions:
        distance =1
        (elem,_) =getRelElem(position,direction,distance,field,size)
        while(isTrampoline(elem)):
            distance +=2
            (elem,_) =getRelElem(position,direction,distance,field,size)
        (_,pos) = getRelElem(position,direction,distance,field,size);
        
        if(isAccessible(getElem(pos,field,size))):
            result.append(pos)
 
    return result;

def getMovementClauses(field,size,timesteps):
    result = []
    for t in range(timesteps):
        for x in range(size[0]):
            for y in range(size[1]):
                neighbours = getAccessibleNeighbours((x,y),field,size);
                clause = [-encodeVar(x,y,t,size)];
                
                for neighbour in neighbours:
                    clause.append(encodeVar(neighbour[0],neighbour[1],t+1,size));
                    
                result.append(clause);
    return result

def getBehavioralClauses(field, size, timesteps):
    result = []
    for x in range(size[0]):
        for y in range(size[1]):
            #a destroyable (green) field cannot be accessed twice
            if(isDestroyable(field[x][y])):
                for t in range(timesteps-1):
                    for u in range(t+1,timesteps):
                        result.append([-encodeVar(x,y,t,size),-encodeVar(x,y,u,size)])
                    
    return result;

def getStartClauses(field, size,start):
    variable = encodeVar(start[0], start[1], 0, size);
    return [[variable]];

def getEndClauses(field,size,timesteps):
    result = []
    clause = []
    #the player has to be on a non-destroyable field at the last timestep
    for x in range(size[0]):
        for y in range(size[1]):
            if(isAccessible(field[x][y]) and not isDestroyable(field[x][y])):
                clause.append(encodeVar(x, y, timesteps, size));
                
    result.append(clause);
    
    #all green tiles have to be destroyed
    for x in range(size[0]):
        for y in range(size[1]):
            if(isDestroyable(field[x][y])):
                clause = [];
                for t in range(timesteps):
                    clause.append(encodeVar(x,y,t,size));
                    
                result.append(clause);
    return result;
    
    

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: <map file> <number steps>")
        exit(1);
    else:
        size,field,start = readMap(sys.argv[1]);
        if not verifyStart(start,field):
            print("Invalid map. The player has to start on a non-waterfield")
            exit(1)
            
        timeSteps = int(sys.argv[2])
        clauses = [];
        stateClauses = getStateClauses(field,size,timeSteps);
        movementClauses = getMovementClauses(field,size,timeSteps);
        behavioralClauses = getBehavioralClauses(field,size,timeSteps);
        startClauses= getStartClauses(field,size,start);
        endClauses = getEndClauses(field,size,timeSteps)
        clauses = stateClauses + movementClauses + behavioralClauses + startClauses + endClauses;
        printFormulas(clauses)

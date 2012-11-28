#!/usr/bin/python3
'''
Created on Nov 15, 2012

@author: till
'''

import sys;
import gc;
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
    
def getNeighbours(position, field, size):
    result = [];

    directions = [CONST_NORTH,CONST_SOUTH,CONST_NORTHWEST,CONST_SOUTHWEST,CONST_NORTHEAST,CONST_SOUTHEAST];
    
    for direction in directions:
        (elem, pos)=getRelElem(position, direction, 1, field, size)
        if(isAccessible(elem) and not isHigh(elem)):
            result.append(pos)
            
    return result

def getAccessibleNeighbours(position,field,size):
    result = [];

    directions = [CONST_NORTH,CONST_SOUTH,CONST_NORTHWEST,CONST_SOUTHWEST,CONST_NORTHEAST,CONST_SOUTHEAST];

    for direction in directions:
        distance =1
        (elem,_) =getRelElem(position,direction,distance,field,size)
        (elem2, _) =getRelElem(position, direction, distance+1, field, size)
        while(isTrampoline(elem) and not isHigh(elem2)):
            distance +=2
            (elem,_) =getRelElem(position,direction,distance,field,size)
            (elem2, _) = getRelElem(position, direction+1, distance, field, size)
        (_,pos) = getRelElem(position,direction,distance,field,size);

        if(isAccessible(getElem(pos,field,size)) and not isHigh(getElem(pos,field,size))):
            result.append(pos)

    return result;

def getMovementClauses(field,size,timesteps):
    result = []
    for t in range(timesteps):
        for x in range(size[0]):
            for y in range(size[1]):
                if(isAccessible(getElem((x, y), field, size))):
                    neighbours = getAccessibleNeighbours((x,y),field,size);
                    clause = [-encodeVar(x,y,t,size)];

                    for neighbour in neighbours:
                        clause.append(encodeVar(neighbour[0],neighbour[1],t+1,size));

                    result.append(clause);
    return result
    
def getBigClauses(big, size, timesteps):
    result = []
    
    for elem in big:
        for t in range(timesteps):
            clause = [-encodeVar(elem[0], elem[1], t, size), encodeState(elem[0], elem[1], t, size)]
        result.append(clause)
    
    return result
    
def getSmallBigClauses(small, big, size, timesteps):
    result =[]
    for elem in big:
        for t in range(2, timesteps):
            for smallElem in small:
                clause = [-encodeVar(elem[0], elem[1], t, size)];
                for u in range(t-1):
                    clause.append(encodeVar(smallElem[0], smallElem[1], u, size))
                result.append(clause);
                
    return result;
    
def getSmallBigClausesDifference1(small, big, size, timesteps):
    result = []
    for elem in big:
        for t in range(2, timesteps):
            for smallElem in small:
                for u in range(t-1):
                    clause = [-encodeVar(elem[0], elem[1], t, size), -encodeVar(smallElem[0], smallElem[1], u, size)];
                    for v in range(t-1):
                        if(v != u):
                            clause.append(encodeVar(smallElem[0], smallElem[1], v, size))
                    result.append(clause);
                    
    return result;
    
def getHighClauses(smallGreen, bigGreen, smallTurq, bigTurq, size, timesteps):
    result =[];
    
    for elem in bigTurq:
            prevVar = encodeState(elem[0], elem[1], 0, size, timesteps)
            result.append([-prevVar])
            for t in range(timesteps):
                var = encodeState(elem[0], elem[1], t+1, size, timesteps)
                
                allDestroyed = [];
                if(t ==0):
                    allDestroyed = [[prevVar]]
                else:
                    for sElem in smallTurq:
                        clause = [prevVar]
                        for u in range(t+1):
                            clause.append(encodeVar(sElem[0], sElem[1], u, size));
                        allDestroyed.append(clause)
                    
                for clause in allDestroyed:
                    temp = [-var] + clause;
                    result.append(temp);
            
                temp = [var];
                for clause in allDestroyed:
                    tempVar = helperVariable(size, timesteps)
                    temp.append(-tempVar);
                    result.append(clause+[-tempVar])
                
                    for c in clause:
                        result.append([tempVar, -c])
                        
                gc.collect()
                result.append(temp);

            
                prevVar = var
    for elem in bigGreen:
            prevVar = encodeState(elem[0], elem[1], 0, size, timesteps)
            result.append([-prevVar])
            for t in range(timesteps):
                var = encodeState(elem[0], elem[1], t+1, size, timesteps)
                
                allDestroyed = [];
                if(t ==0):
                    allDestroyed = [[prevVar]]
                else:
                    for sElem in smallGreen:
                        clause = [prevVar]
                        for u in range(t+1):
                            clause.append(encodeVar(sElem[0], sElem[1], u, size));
                        allDestroyed.append(clause)
                    for sElem in smallTurq:
                        for u in range(t+1):
                            clause = [prevVar, -encodeVar(sElem[0], sElem[1], u, size)]
                            for v in range(t+1):
                                if(u != v):
                                    clause.append(encodeVar(sElem[0], sElem[1], v, size))
                            allDestroyed.append(clause)
                    
                for clause in allDestroyed:
                    temp = [-var] + clause;
                    result.append(temp);
            
                temp = [var];
                for clause in allDestroyed:
                    tempVar = helperVariable(size, timesteps)
                    temp.append(-tempVar);
                    result.append(clause+[-tempVar])
                
                    for c in clause:
                        result.append([tempVar, -c])
                gc.collect()
                result.append(temp);
                
            
                prevVar = var
    
    return result;
    
def getHighMovementClauses(bigGreen,  bigTurq, size, timesteps):
    result = []
    
    for elem in bigGreen:
        for t in range(1, timesteps+1):
            neighbours = getNeighbours(elem, field, size)
            result.append([-encodeVar(elem[0], elem[1], t, size), encodeState(elem[0], elem[1], t-1, size, timesteps), encodeState(elem[0], elem[1], t, size, timesteps)])
            if(len(neighbours) > 0):
                for neighbour in neighbours:
                    result.append([-encodeVar(elem[0], elem[1], t, size), encodeState(elem[0], elem[1], t-1, size, timesteps), -encodeVar(neighbour[0], neighbour[1], t-1, size)]);
            else:
                result.append([-encodeVar(elem[0], elem[1], t, size), encodeState(elem[0], elem[1], t-1, size, timesteps)]);
            
    
    for elem in bigTurq:
        for t in range(1, timesteps+1):
            neighbours = getNeighbours(elem, field, size)
            result.append([-encodeVar(elem[0], elem[1], t, size), encodeState(elem[0], elem[1], t-1, size, timesteps), encodeState(elem[0], elem[1], t, size, timesteps)])
            if(len(neighbours) > 0):
                for neighbour in neighbours:
                    result.append([-encodeVar(elem[0], elem[1], t, size), encodeState(elem[0], elem[1], t-1, size, timesteps), -encodeVar(neighbour[0], neighbour[1], t-1, size)]);
            else:
                result.append([-encodeVar(elem[0], elem[1], t, size), encodeState(elem[0], elem[1], t-1, size, timesteps)]);
    
    return result

def getBehavioralClauses(field, size, timesteps):
    result = []
    for x in range(size[0]):
        for y in range(size[1]):
            #a destroyable (green) field cannot be accessed twice
            if(isGreen(field[x][y])):
                for t in range(timesteps-1):
                    for u in range(t+1,timesteps):
                        result.append([-encodeVar(x,y,t,size),-encodeVar(x,y,u,size)])
            
            if(isTurquoise(getElem((x,y),field,size))):
                #if once visited it has to be visited a second time
                for t in range(timesteps):
                    clause = [-encodeVar(x,y,t,size)];
                    for u in range(timesteps):
                        if(u != t):
                            clause.append(encodeVar(x,y,u,size));
                    result.append(clause);
                
                #turquoise fields can at most be accessed twice
                for t in range(timesteps-1):
                    for u in range(t+1, timesteps):
                        for v in range(u+1, timesteps+1):
                            result.append([-encodeVar(x, y, t, size), -encodeVar(x, y, u, size), -encodeVar(x, y, v, size)])
                            
    #model that high stones sink down if all flat stones of the same kind have been destroyed
    smallGreens = findSmallElem(CONST_GREEN, field, size);
    bigGreens = findBigElem(CONST_GREEN, field, size);
    
    smallTurq = findSmallElem(CONST_TURQUOISE, field, size);
    bigTurq = findBigElem(CONST_TURQUOISE, field, size);
    
    clauses = getHighClauses(smallGreens, bigGreens, smallTurq, bigTurq, size, timesteps);
    result += clauses;
    
    clauses = getHighMovementClauses(bigGreens, bigTurq, size, timesteps)
    result += clauses;
    
    #clauses = getBigClauses(bigGreens, size, timesteps);
    #result += clauses;
    #clauses = getBigClauses( bigTurq, size, timesteps);
    #result += clauses;
    
    #clauses = getSmallBigClausesDifference1(smallTurq, bigGreens, size, timesteps);
    #result += clauses
    
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
            if(isAccessible(field[x][y]) and not isGreen(field[x][y])):
                clause.append(encodeVar(x, y, timesteps, size));

    result.append(clause);

    #all green tiles have to be destroyed
    for x in range(size[0]):
        for y in range(size[1]):
            if(isGreen(field[x][y])):
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
        clauses = stateClauses + movementClauses + startClauses + endClauses + behavioralClauses;
        
        printFormulas(clauses)

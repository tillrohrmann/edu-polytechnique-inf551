'''
Created on Nov 15, 2012

@author: till
'''

CONST_WATER='W'
CONST_GREEN='g'
CONST_TRAMPOLINE='j'
CONST_TURQUOISE='t'
CONST_STONE='s'
CONST_NORTH=1
CONST_SOUTH=2
CONST_NORTHWEST=3
CONST_SOUTHWEST=4
CONST_NORTHEAST=5
CONST_SOUTHEAST=6

counter = 0;
#highMapping = {};

import sys;

def getMaxMovementVariables(size, timesteps):
    return size[1]-1 + size[1]*(size[0]-1) + size[0]*size[1]*timesteps + 2

def encodeVar(x,y,t,size):
    return y + size[1]*x + size[0]*size[1]*t + 1;
    
def encodeState(x, y, t, size, timesteps):
    #global counter
   # global highMapping
    nombreMovementVariables = getMaxMovementVariables(size, timesteps)
    
    return y + size[1]*x + size[0]*size[1]*t + 1 + nombreMovementVariables;
    
   # if((x, y, t) in highMapping):
    #    return highMapping[(x, y, t)]
   # else:
    #    highMapping[(x, y, t)] = counter + nombreMovementVariables;
    #    counter += 1;
     #   return counter + nombreMovementVariables-1;
        
def helperVariable(size, timesteps):
    global counter
    nombreMovementVariables = 2*getMaxMovementVariables(size, timesteps)
    counter += 1
    
    return nombreMovementVariables + counter -1

def decodeVar(value,size):
    value -= 1;
    y = value % size[1];
    value = int(value/size[1]);
    x = value % size[0];
    t = int(value/size[0])
    
    return (x,y,t)

def isAccessible(element):
    return element != None and element != CONST_WATER
    
def isGreen(element):
    return str(element).lower()==CONST_GREEN

def isDestroyable(element):
    return isGreen(element) or isTurquoise(element)

def isTrampoline(element):
    return str(element).lower() == CONST_TRAMPOLINE
    
def isHigh(element):
    return element == CONST_STONE.upper()
    
def isHighMoveable(element):
    return element == element.upper();

def isTurquoise(element):
    return str(element).lower() == CONST_TURQUOISE

def getElem(elem,field,size):
    if(elem[0] < 0 or elem[0] >= size[0] or elem[1] < 0 or elem[1] >= size[1]):
        return None
    else:
        return field[elem[0]][elem[1]];
    
def getDirectionVector(elem,direction):
    if(direction==CONST_NORTH):
        vec = (-1,0);
    elif(direction == CONST_SOUTH):
        vec = (1,0);
    elif(elem[1]%2==1):
        if(direction == CONST_NORTHWEST):
            vec = (0,-1);
        elif(direction == CONST_SOUTHWEST):
            vec = (1,-1);
        elif(direction == CONST_NORTHEAST):
            vec = (0,+1)
        else:
            vec = (+1,+1)
    else:
        if(direction == CONST_NORTHWEST):
            vec = (-1,-1)
        elif(direction == CONST_SOUTHWEST):
            vec = (0,-1)
        elif(direction==CONST_NORTHEAST):
            vec = (-1,+1)
        else:
            vec= (0,+1)
            
    return vec
    
def getRelElem(elem,direction,distance,field,size):
    pos = (elem[0],elem[1])
    
    while(distance >0):
        vec = getDirectionVector(pos,direction)
        pos = (pos[0]+vec[0],pos[1]+vec[1])
        distance -= 1
                
    return getElem(pos,field,size),pos
    
def findElem(elem, field, size):
    result = [];
    
    for x in range(size[0]):
        for y in range(size[1]):
            if(field[x][y] == elem):
                result.append((x, y));
                
    return result;
    
def findSmallElem(elem, field, size):
    return findElem(elem.lower(), field, size)
    
def findBigElem(elem, field, size):
    return findElem(elem.upper(), field, size)
    
def destroy(elem):
    result = CONST_WATER
    if(isGreen(elem)):
        result = CONST_WATER
    elif(isTurquoise(elem)):
        result = CONST_GREEN
        
    if(elem.islower()):
        result= result.lower()
    else:
        result = result.upper()
        
    return result;

def readMap(filename):
    file = open(filename);
    
    linePosition = file.readline();
    startRow,startColumn = [int(x) for x in linePosition.split()];
    
    startRow = int(startRow/2);
        
    firstLine = list(file.readline().rstrip());
    firstSign = -1;
    
    for i in range(len(firstLine)):
        if(firstLine[i] != " "):
            firstSign = i;
            break;
    
    expansion = []
    if(firstSign % 2 == 1):
        expansion = [CONST_WATER];
        startColumn += 1;
    
    firstLine = expansion + firstLine;
    firstLineRead = True;
    
    maxColumns = -1;
    field = []
    
    for line in file:
        line = list(line.rstrip())
        if(firstLineRead):
            line = expansion + line;
            newLine = []
            
            for i in range(max(len(firstLine),len(line))):
                if(i%2 ==0 and i < len(firstLine)):
                    newLine.append(firstLine[i]);
                elif(i < len(line)):
                    newLine.append(line[i]);
                else:
                    newLine.append(CONST_WATER)
            
            if(len(newLine) >maxColumns):
                maxColumns = len(newLine)
                
            field.append(newLine);
            firstLineRead = False;
        else:
            firstLine = expansion + line;
            firstLineRead = True;
    
    if(firstLineRead):
        field.append(firstLine)
            
    for row in field:
        for i in range(len(row)):
            if(row[i] == " "):
                row[i] = CONST_WATER;
                
        if(len(row) < maxColumns):
            row.extend([CONST_WATER for x in range(maxColumns-len(row))]);
            
    file.close()
        
    return ((len(field),maxColumns),field,(startRow,startColumn))

def readResultFile(filename):
    file = open(filename)
    result = []
    
    line = file.readline()
    if line.strip() == "UNSAT":
        sys.exit(1)
    for line in file:
        result = result + [int(x) for x in line.split() if int(x) != 0];
    
    file.close()
    
    return result;

def printClause(clause):
    print(" ".join([str(x) for x in clause])+" 0");
    
def printFormulas(clauses):
    numVar = 0
    
    for clause in clauses:
        temp = [abs(x) for x in clause];
        maxVar = max(temp)
        if(maxVar > numVar):
            numVar = maxVar
    
    print("p cnf "+str(numVar)+" " + str(len(clauses)));
    for clause in clauses:
        printClause(clause)

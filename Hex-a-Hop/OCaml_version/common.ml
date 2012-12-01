let const_water = 'W';;
let const_green = 'g';;
let const_trampoline = 'j';;
let const_turquoise = 't';;
let const_stone = 's';;

(** The number of different tiles types. *)
let nb_types = 2;;
(** The type number of green. *)
let green_type = 1;;
(** The type number of turquoise. *)
let turquoise_type = 2;;

let const_north = 1;;
let const_south = 2;;
let const_northwest = 3;;
let const_southwest = 4;;
let const_northeast = 5;;
let const_southeast = 6;;

let counter = ref 0;;

(** Stores the maximum value of a used (encoded variable). *)
let maxVar = ref 0;;

let getMaxMovementVariables size timesteps =
  size.(1) - 1 + size.(1) * (size.(0) - 1) + size.(0) * size.(1) * timesteps + 2
;;

(**
 * Generate a variable indicating that the player is at a given position at a
 * given timestep.
 *
 * @param x The abscissa of the position.
 * @param y The ordinate of the position.
 * @param t The timestep.
 * @param size The size of the map.
 * @return The value of the encoded variable.
 *)
let encodeVar x y t size =
  let var = y + size.(1) * x + size.(0) * size.(1) * t + 1 in
  if var > !maxVar then maxVar := var;
  var
;;

(**
 * Generate a variable indicating if all the low tiles of a given type have been
 * destroyed, and therefore all the high tiles have sunk down.
 * 
 * @param tileType The type of the tile.
 * @param t The timestep.
 * @param size The size of the field (one-dimensional array).
 * @param timesteps The number of timesteps.
 * @return The value of the encoded variable.
 *)
let encodeTypeHeightVar tileType t size timesteps =
  let var = size.(0) * size.(1) * (timesteps + 1) + tileType + t * nb_types in
  if var > !maxVar then maxVar := var;
  var
;;
    
let encodeState x y t size timesteps =
  y + size.(1) * x + size.(0) * size.(1) * t + 1 +
  (getMaxMovementVariables size timesteps)
;;
        
let helperVariable size timesteps =
  let numberMovementVariables = 2 * (getMaxMovementVariables size timesteps) in
  counter := !counter + 1;
  numberMovementVariables + !counter - 1
;;

(**
 * Decode a position and timestep variable.
 *
 * @param value The value of the variable.
 * @param size The size of the field (one-dimensional array).
 * @return A triplet giving the abscissa, the ordinate and the timestep
 * represented corresponding to this variable.
 *)
let decodeVar value size =
  let v = ref (value - 1) in
  let y = !v mod size.(1) in
  v := !v / size.(1);
  let x = !v mod size.(0) in
  let t = !v / size.(0) in
  
  (x,y,t)
;;

(**
 * Check if a given element is accessible.
 *
 * @param element An option giving, if defined, a character indicating the
 * element type.
 * @return A boolean indicating if the element is accessible or not.
 *)
let isAccessible element =
  match element with
      None -> false
    | Some e -> e <> const_water;;
    
(**
 * Check if a given element is green.
 *
 * @param element An option giving, if defined, a character indicating the
 * element type.
 * @return A boolean indicating if the element is green or not.
 *)
let isGreen element =
  match element with
      None -> false
    | Some e -> Char.lowercase e = const_green;;

(**
 * Check if a given element is turquoise.
 *
 * @param element An option giving, if defined, a character indicating the
 * element type.
 * @return A boolean indicating if the element is turquoise or not.
 *)
let isTurquoise element = 
  match element with
      None -> false
    | Some e -> Char.lowercase e = const_turquoise;;

(**
 * Check if a given element is destroyable.
 *
 * @param element An option giving, if defined, a character indicating the
 * element type.
 * @return A boolean indicating if the element is destroyable or not.
 *)
let isDestroyable element = isGreen element || isTurquoise element;;

(**
 * Check if a given element is a trampoline.
 *
 * @param element An option giving, if defined, a character indicating the
 * element type.
 * @return A boolean indicating if the element is a trampoline or not.
 *)
let isTrampoline element = 
  match element with
      None -> false
    | Some e -> Char.lowercase e = const_trampoline;;
    
(**
 * Check if a given element is high.
 *
 * @param element An option giving, if defined, a character indicating the
 * element type.
 * @return A boolean indicating if the element is high or not.
 *)
let isHigh element = 
  match element with
      None -> false
    | Some e -> e = Char.uppercase const_stone;;
    
(**
 * Check if a given element is high and moveable.
 *
 * @param element An option giving, if defined, a character indicating the
 * element type.
 * @return A boolean indicating if the element is high and moveable or not.
 *)
let isHighMoveable element = 
  match element with
      None -> false
    | Some e -> e = Char.uppercase e;;

(**
 * Get the element type of a given element.
 *
 * @param elem An one-dimensional array giving the abscissa and the ordinate of
 * the point to consider.
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @return An option giving, if defined, the type of the element, otherwise if
 * it is undefined it means that the given position is out of bounds.
 *)
let getElem elem field size =
  if elem.(0) < 0 || elem.(0) >= size.(0) || elem.(1) < 0 ||
     elem.(1) >= size.(1)
  then
    None
  else
    Some field.(elem.(0)).(elem.(1))
;;
    
(**
 * Get the direction movement vector for a given position and direction.
 *
 * @param elem An one-dimensional array giving the abscissa and the ordinate of
 * the point to consider.
 * @param direction The direction in which to move.
 * @return A one dimensional array giving the movements to do in terms of
 * abscissa and ordinate to actually move in the given direction.
 *)
let getDirectionVector elem direction =
  if direction = const_north
  then
    [|-1; 0|]
  else if direction = const_south
  then
    [|1; 0|]
  else if elem.(1) mod 2 = 1
  then
    if direction = const_northwest
    then
      [|0; -1|]
    else if direction = const_southwest
    then
      [|1; -1|]
    else if direction = const_northeast
    then
      [|0; 1|]
    else
      [|1; 1|]
  else
    if direction = const_northwest
    then
      [|-1; -1|]
    else if direction = const_southwest
    then
      [|0; -1|]
    else if direction = const_northeast
    then
      [|-1; 1|]
    else
      [|0; 1|]
;;
    
(**
 * Get the element type and position of the point reached after a movement.
 *
 * @param elem An one-dimensional array giving the abscissa and the ordinate of
 * the point to consider.
 * @param direction The direction in which to move.
 * @param distance The distance of the move.
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @return A couple with the element type (as an option) and the position (as an
 * one-dimensional array) of the reached point.
 *)
let getRelElem elem direction distance field size =
  let pos = ref [|elem.(0); elem.(1)|] in
  
  let rec getRelElem_aux distance =
    if distance > 0
    then
      begin
        let vec = getDirectionVector !pos direction in
        pos := [|!pos.(0) + vec.(0); !pos.(1) + vec.(1)|];
        getRelElem_aux (distance - 1)
      end
  in

  getRelElem_aux distance;
  (getElem !pos field size, !pos);
;;
    
(*
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
 *)

(**
 * Read the map (field) of the level from a given file.
 *
 * @param filename The name of the file from which to read the map.
 * @return A triplet containing the size of the field (one-dimensional array),
 * the field itself (two-dimensional array) and the coordinates of the starting
 * point (one-dimensional array).
 *)
let readMap filename =
  try
    let file = open_in filename in

    let (r, c) = Scanf.fscanf file "%d %d\n" (fun r c -> (r, c)) in
    let startRow = (r / 2) in
    let startColumn = ref c in

    let readLine file = Scanf.fscanf file "%s@\n" (fun s -> s) in

    let firstLine = ref (readLine file) in
    
    let rec getFirstSign i =
      if i < String.length !firstLine
      then
        if (String.get !firstLine i) <> ' '
        then
          i
        else
          getFirstSign (i + 1)
      else
        -1
    in
    let firstSign = getFirstSign 0 in
    
    let expansion = ref "" in
    if firstSign mod 2 = 1
    then
      begin
        expansion := String.make 1 const_water;
        startColumn := !startColumn + 1
      end;
    
    firstLine := !expansion ^ !firstLine;
    let firstLineRead = ref true in
    
    let maxColumns = ref (-1) in
    let field = ref [||] in
    let line = ref (readLine file) in
    
    while !line <> ""  do
      if !firstLineRead
      then
        begin
          line := !expansion ^ !line;
          let newLine = ref [||] in
          
          let bound = (max (String.length !firstLine) (String.length !line)) - 1
          in
          for i = 0 to bound do
            if i mod 2 = 0 && i < (String.length !firstLine)
            then
              newLine := Array.append !newLine [|String.get !firstLine i|]
            else if i < (String.length !line)
            then
              newLine := Array.append !newLine [|String.get !line i|]
            else
              newLine := Array.append !newLine ([|const_water|])
          done;
          
          let length = Array.length !newLine in
          if length > !maxColumns
          then
            maxColumns := length;

          field := Array.append !field [|!newLine|];
          firstLineRead := false;
        end

      else
        begin
          firstLine := !expansion ^ !line;
          firstLineRead := true
        end;

      line := readLine file;
    done;
    
    if !firstLineRead
    then
      begin
        let length = String.length !firstLine in
        let tmp = Array.make length ' ' in
        for i = 0 to length - 1 do
          tmp.(i) <- String.get !firstLine i
        done;
        field := Array.append !field [|tmp|]
      end;

    let field_height = Array.length !field in
    for i = 0 to field_height - 1 do
      let row_length = Array.length !field.(i) in
      for j = 0 to row_length - 1 do
        if !field.(i).(j) = ' '
        then
          !field.(i).(j) <- const_water
      done;
      
      if row_length < !maxColumns
      then
        !field.(i) <- Array.append !field.(i)
                                   (Array.make (!maxColumns - row_length)
                                               const_water)
    done;

    close_in file;
    ([|field_height; !maxColumns|], field, [|startRow; !startColumn|])
  with
      Scanf.Scan_failure s -> failwith (Printf.sprintf
                                          "Bad file structure : %s." s)
    | End_of_file -> failwith "Bad file structure : end-of-file encountered \
                               before it should have been the case."
    | Sys_error e -> failwith (Printf.sprintf "Error while opening file : %s."
                                              e);
;;

(*
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
 *)

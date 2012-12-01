open Common

(**
 * Check if the start is not placed on a water field.
 *
 * @param start An one-dimensional array giving the abscissa and the ordinate of
 * the starting point.
 * @param field The field of the level (two-dimensional array).
 * @return True if the start is placed correctly, false if not.
 *) 
let verifyStart start field = field.(start.(0)).(start.(1)) != const_water;;

(**
 * Generate the clauses related to the position of the player and add them to
 * the formula.
 *
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @param timesteps The total number of timesteps.
 * @param formula A reference to the propositional logic formula in CNF
 * associated to the level (list of clauses, which are lists of integers).
 *)
let getStateClauses field size timesteps formula =
  for t = 0 to timesteps do
    let clause = ref [] in
    (* at every timestep, the player has to be somewhere *)
    for x = 0 to size.(0) - 1 do
      for y = 0 to size.(1) - 1 do
        if isAccessible (getElem [|x; y|] field size)
        then
          clause := (encodeVar x y t size) :: !clause
      done
    done;
    formula := !clause :: !formula;
    
    (* but he cannot be at two positions at the same time *)
    for x = 0 to size.(0) - 1 do
      for y = 0 to size.(1) - 1 do
        if isAccessible (getElem [|x; y|] field size)
        then
          begin
            for j = y + 1 to size.(1) - 1 do
              if isAccessible (getElem [|x; j|] field size)
              then
                formula := [-(encodeVar x y t size); -(encodeVar x j t size)] ::
                           !formula
            done;

            for i = x + 1 to size.(0) - 1 do
              for j = 0 to size.(1) - 1 do
                if isAccessible (getElem [|i; j|] field size)
                then
                  formula := [-(encodeVar x y t size); -(encodeVar i j t size)]
                             :: !formula
              done
            done
          end
      done
    done
  done
;;

(**
 * Get the list of directly accessible neighbours from a given position.
 *
 * @param position An one-dimensional array giving the abscissa and the ordinate
 * of the point to consider.
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @return The list of positions (one-dimensional arrays) of the directly
 * accessible neighbours.
 *)
let getNeighbours position field size =
  let result = ref [] in
  let directions = [const_north; const_south; const_northwest; const_southwest;
                    const_northeast; const_southeast] in

  List.iter (fun direction ->
               let (elem, pos) = getRelElem position direction 1 field size in
               if isAccessible elem && not (isHigh elem)
               then
                 result := pos :: !result) directions;
  !result
;;

(**
 * Get the list of all accessible neighbours from a given position (especially
 * thanks to trampolines).
 *
 * @param position An one-dimensional array giving the abscissa and the ordinate
 * of the point to consider.
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @return The list of positions (one-dimensional arrays) of all the accessible
 * neighbours.
 *)
let getAccessibleNeighbours position field size =
  let result = ref [] in
  let directions = [const_north; const_south; const_northwest; const_southwest;
                    const_northeast; const_southeast] in

  let iteration_function direction =
    let distance = ref 1 in
    let (elem, _) = getRelElem position direction !distance field size in
    let (elem2, _) = getRelElem position direction (!distance + 1) field size in

    let rec iteration_function_aux elem elem2 =
      if isTrampoline elem && not (isHigh elem2)
      then
        begin
          distance := !distance + 2;
          let (elem_new, _) = getRelElem position direction !distance field size
          in
          let (elem2_new, _) = getRelElem position direction (!distance + 1)
                                          field size in
          iteration_function_aux elem_new elem2_new
        end
    in

    iteration_function_aux elem elem2;
    let (_, pos) = getRelElem position direction !distance field size in
    if isAccessible (getElem pos field size) &&
       not (isHigh (getElem pos field size))
    then
      result := pos :: !result
  in

  List.iter iteration_function directions;
  !result
;;

(**
 * Generate the clauses related to the movements of the player and add them to
 * the formula.
 *
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @param timesteps The total number of timesteps.
 * @param formula A reference to the propositional logic formula in CNF
 * associated to the level (list of clauses, which are lists of integers).
 *)
let getMovementClauses field size timesteps formula =
  for t = 0 to timesteps - 1 do
    for x = 0 to size.(0) - 1 do
      for y = 0 to size.(1) - 1 do
        if isAccessible (getElem [|x; y|] field size)
        then
          begin
            let neighbours = getAccessibleNeighbours [|x; y|] field size in
            let clause = ref [-(encodeVar x y t size)] in
            List.iter (fun n -> clause := (encodeVar n.(0) n.(1) (t+1) size) ::
                                          !clause) neighbours;
            formula := !clause :: !formula
          end
      done
    done
  done
;;

(**
 * Generate the clauses related to the accessibility of high (big) elements and
 * add them to the formula.
 *
 * @param big A list containing the different positions (one-dimensional arrays)
 * of the big elements.
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @param timesteps The total number of timesteps.
 * @param formula A reference to the propositional logic formula in CNF
 * associated to the level (list of clauses, which are lists of integers).
 *)
let getBigClauses big field size timesteps formula =
  let rec getBigClauses_aux big =
    match big with
        [] -> ()
      | h::t ->
          let tileType = getTileType field.(h.(0)).(h.(1)) in
          for ts = 0 to timesteps do
            formula := [-(encodeVar h.(0) h.(1) ts size);
                        encodeTypeHeightVar tileType (ts - 1) size timesteps] ::
                       !formula
          done;
          getBigClauses_aux t
  in

  getBigClauses_aux big;
  for tileType = 1 to nb_types do
    for t = 0 to timesteps - 1 do
      formula := [-(encodeTypeHeightVar tileType t size timesteps);
                  encodeTypeHeightVar tileType (t + 1) size timesteps] ::
                 !formula
    done
  done
;;

(**
 * Generate the clauses related to the sinking conditions of high (big) elements
 * and add them to the formula.
 *
 * @param elementType The element type (character) for which to generate the
 * sinking clauses.
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @param timesteps The total number of timesteps.
 * @param formula A reference to the propositional logic formula in CNF
 * associated to the level (list of clauses, which are lists of integers).
 *)
let getHighElementsSinkingClauses elementType field size timesteps formula =
  let tileType = getTileType elementType in

  let rec getHighElementsSinkingClauses_aux smallElements =
    match smallElements with
        [] -> ()
      | h::t ->
          for ts = 0 to timesteps do
            let clause = ref [-(encodeTypeHeightVar tileType ts size timesteps)]
            in

            for ts' = 0 to ts - 1 do
              clause := (encodeVar h.(0) h.(1) ts' size) :: !clause
            done;

            formula := !clause :: !formula
          done;

          getHighElementsSinkingClauses_aux t;
  in

  let smallElements = findSmallElem elementType field size in
  getHighElementsSinkingClauses_aux smallElements
;;

(*
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
 *)

(**
 * Generate the clauses related to the behaviour of the field's tiles and add
 * them to the formula.
 *
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @param timesteps The total number of timesteps.
 * @param formula A reference to the propositional logic formula in CNF
 * associated to the level (list of clauses, which are lists of integers).
 *)
let getBehavioralClauses field size timesteps formula =
  for x = 0 to size.(0) - 1 do
    for y = 0 to size.(1) - 1 do
      (* a destroyable (green) field cannot be accessed twice *)
      if isGreen (getElem [|x; y|] field size)
      then
        for t = 0 to timesteps - 2 do
          for u = t + 1 to timesteps - 1 do
            formula := [-(encodeVar x y t size); -(encodeVar x y u size)] ::
                       !formula
          done
        done;

      if isTurquoise (getElem [|x; y|] field size)
      then
        begin
          (* if once visited it has to be visited a second time *)
          for t = 0 to timesteps - 1 do
            let clause = ref [-(encodeVar x y t size)] in
              for u = 0 to timesteps - 1 do
                if u <> t
                then
                  clause := (encodeVar x y u size) :: !clause
              done;
              formula := !clause :: !formula;
          done;

          (* turquoise fields can at most be accessed twice *)
          for t = 0 to timesteps - 2 do
            for u = t + 1 to timesteps - 1 do
              for v = u + 1 to timesteps do
                formula := [-(encodeVar x y t size); -(encodeVar x y u size);
                            -(encodeVar x y v size)] :: !formula
              done
            done
          done
        end
    done
  done;

  getBigClauses ((findBigElem const_green field size) @
                 (findBigElem const_turquoise field size)) field size timesteps
                formula;
  getHighElementsSinkingClauses const_green field size timesteps formula;
  getHighElementsSinkingClauses const_turquoise field size timesteps formula;
                            
    (*
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
     *)
;;

(**
 * Generate the clauses related to the start of the level and add them to the
 * formula.
 *
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @param start An one-dimensional array giving the abscissa and the ordinate of
 * the starting point.
 * @param formula A reference to the propositional logic formula in CNF
 * associated to the level (list of clauses, which are lists of integers).
 *)
let getStartClauses field size start formula =
  formula := [(encodeVar start.(0) start.(1) 0 size)] :: !formula
;;

(**
 * Generate the clauses related to the end of the level and add them to the
 * formula.
 *
 * @param field The field of the level (two-dimensional array).
 * @param size The size of the field (one-dimensional array).
 * @param timesteps The total number of timesteps.
 * @param formula A reference to the propositional logic formula in CNF
 * associated to the level (list of clauses, which are lists of integers).
 *)
let getEndClauses field size timesteps formula =
  let clause = ref [] in

  (* the player has to be on a non-destroyable field at the last timestep *)
  for x = 0 to size.(0) - 1 do
    for y = 0 to size.(1) - 1 do
      if isAccessible (getElem [|x; y|] field size) &&
         not (isGreen (getElem [|x; y|] field size))
      then
        clause := (encodeVar x y timesteps size) :: !clause
    done
  done;
  formula := !clause :: !formula;

  (* all green tiles have to be destroyed *)
  for x = 0 to size.(0) - 1 do
    for y = 0 to size.(1) - 1 do
      if isGreen (getElem [|x; y|] field size)
      then
        begin
          clause := [];
          for t = 0 to timesteps - 1 do
            clause := (encodeVar x y t size) :: !clause
          done;
          formula := !clause :: !formula
        end
    done
  done
;;

if Array.length Sys.argv <> 4
then
  Printf.fprintf stderr "Usage : %s <instance_file> <nb_steps> <output_file>\n"
                 Sys.argv.(0)
else
  let (size, field, start) = readMap Sys.argv.(1) in

  if not (verifyStart start !field)
  then
    failwith "Invalid map. The player has to start on a non-waterfield.";

  let timesteps = try int_of_string Sys.argv.(2)
                  with Failure s ->
                         failwith "The given number isn't an integer or is too \
                                   huge."
  in

  let formula = ref [] in
  getStateClauses !field size timesteps formula;
  getMovementClauses !field size timesteps formula;
  getBehavioralClauses !field size timesteps formula;
  getStartClauses !field size start formula;
  getEndClauses !field size timesteps formula;

  Dimacs_cnf.write_formula_to_file (!formula, !Common.maxVar) Sys.argv.(3);

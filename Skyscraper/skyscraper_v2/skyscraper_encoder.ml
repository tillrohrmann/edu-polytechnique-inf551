(**
 * Read the size of the grid and the visibility constraints for an instance of
 * the skyscraper game from a given file.
 *
 * @param file_name The name (or path) of the file from which to read.
 * @return A triplet containing the size of the grid, the line visibility
 * constraints and the column visibility constraints.
 *)
let read_constraints_from_file file_name =
  let read_constraints_array size file =
    let constraints = Array.make size (0, 0) in
    for i = 0 to size - 1 do
      constraints.(i) <- Scanf.fscanf file "%d %d\n" (fun a b -> (a, b))
    done;
    constraints
  in

  try
    let file = open_in file_name in
    let size = Scanf.fscanf file "%d\n" (fun i -> i) in
    let line_constraints = read_constraints_array size file in
    let column_constraints = read_constraints_array size file in
    close_in file;
    (size, line_constraints, column_constraints)
  with
      Scanf.Scan_failure s -> failwith (Printf.sprintf
                                          "Bad file structure : %s." s)
    | End_of_file -> failwith "Bad file structure : end-of-file encountered \
                               before it should have been the case."
    | Sys_error e -> failwith (Printf.sprintf "Error while opening file : %s."
                                              e)
;;

(**
 * Encode an instance of the skyscraper game into a propositional logic formula
 * in CNF.
 *
 * @param n The size of the grid.
 * @param line_constraints The visibility constraints for the lines of the grid.
 * @param columns_constraints The visibility constraints for the columns of the
 * grid.
 * @return The encoding of the instance as a propositional logic formula in CNF.
 *)
let skyscraper_encode n line_constraints column_constraints =
  let formula = ref [] in
  let var i j k = n * n * i + n * j + k + 1 in

  (* Each square contains at least one skyscraper. *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let clause = ref [] in
      for k = 0 to n - 1 do
        clause := (var i j k) :: !clause
      done;
      formula := !clause :: !formula
    done
  done;

  (* Each square contains at most one skyscraper. *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 2 do
        for k' = k + 1 to n - 1 do
          formula := [-(var i j k); -(var i j k')] :: !formula
        done
      done
    done
  done;

  (* Each row contains one skyscraper of a given height exactly once. *)
  for i = 0 to n - 1 do
    for k = 0 to n - 1 do
      for j = 0 to n - 2 do
        for j' = j + 1 to n - 1 do
          formula := [-(var i j k); -(var i j' k)] :: !formula
        done
      done
    done
  done;
  
  (* Each column contains one skyscraper of a given height exactly once. *)
  for j = 0 to n - 1 do
    for k = 0 to n - 1 do
      for i = 0 to n - 2 do
        for i' = i + 1 to n - 1 do
          formula := [-(var i j k); -(var i' j k)] :: !formula
        done
      done
    done
  done;

  (**
   * Compute the number of skyscrapers seen for a given configuration.
   *
   * @param config An array containing a sequel of skyscrapers of a given
   * height.
   * @return A couple of integers indicating how much skyscrapers are seen from
   * each side of the skyscraper configuration (left-hand side and right-hand
   * side).
   *)
  let nb_seen config =
    let nb_left = ref 0 in
    let nb_right = ref 0 in
    let max_height_left = ref 0 in
    let max_height_right = ref 0 in

    for i = 0 to n - 1 do
      if config.(i) > !max_height_left
      then
        begin
          nb_left := !nb_left + 1;
          max_height_left := config.(i)
        end;

      if config.(n - 1 - i) > !max_height_right
      then
        begin
          nb_right := !nb_right + 1;
          max_height_right := config.(n - 1 - i)
        end
    done;

    (!nb_left, !nb_right)
  in

  (**
   * Generate all the configurations (in one row or column) that are forbidden
   * for a given visibility constraint.
   *
   * @param nb_to_see The number of skyscraper that must exactly be seen from
   * each side of a skyscraper configuration (in an array). 0 means that any
   * number of skyscrapers can be seen from a given side.
   * @return A list of arrays containing arrays with configurations of
   * skyscrapers that aren't allowed for the given visibility constraint.
   *)
  let generate_forbidden_configurations nb_to_see =
    let config = Array.make n 0 in
    let forbidden_configs = ref [] in
    let is_valid (sl, sr) (cl, cr) = (cl = 0 || cl = sl) && (cr = 0 || cr = sr)
    in

    let rec generate_forbidden_configurations_aux i =
      if i = n
      then
        if not (is_valid (nb_seen config) nb_to_see)
        then
          forbidden_configs := (Array.copy config) :: !forbidden_configs
        else ()

      else
        for height = 1 to n do
          let used = ref false in
          let j = ref 0 in
          while not !used && !j < i do
            if config.(!j) = height then used := true;
            j := !j + 1
          done;

          if not !used
          then
            begin
              config.(i) <- height;
              generate_forbidden_configurations_aux (i + 1)
            end
        done;
    in

    if nb_to_see <> (0, 0)
    then
      generate_forbidden_configurations_aux 0;

    !forbidden_configs
  in

  (**
   * Load visibility constraints for given forbidden configurations into the
   * propositional logic formula.
   *
   * @param forbidden_configs The forbidden configurations that must be loaded
   * into the formula.
   * @param var_mapping A function with two parameters that computes the
   * propositional variable for a given position in a forbidden configuration
   * and a given skyscraper height value.
   *)
  let rec load_visibility_constraints forbidden_configs var_mapping =
    match forbidden_configs with
        [] -> ()
      | h::t ->
          let clause = ref [] in
          for m = 0 to n - 1 do
            clause := -(var_mapping m (h.(m) - 1)) :: !clause
          done;

          formula := !clause :: !formula;
          load_visibility_constraints t var_mapping
  in

  (* Each line and column must contain a configuration of skyscrapers that is
     valid according to the four visibility constraints.
   *)
  for i = 0 to n - 1 do
    load_visibility_constraints
      (generate_forbidden_configurations line_constraints.(i))
      (function m -> function k -> var i m k);
    load_visibility_constraints
      (generate_forbidden_configurations column_constraints.(i))
      (function m -> function k -> var m i k);
  done;

  (!formula, n * n * n)
;;

if Array.length Sys.argv <> 3
then
  Printf.fprintf stderr "Usage : %s <instance_file> <output_file>\n"
                 Sys.argv.(0)
else
  let (size, line_constraints, column_constraints) =
    read_constraints_from_file Sys.argv.(1) in
  Dimacs_cnf.write_formula_to_file (skyscraper_encode size line_constraints
                                      column_constraints) Sys.argv.(2)
;;

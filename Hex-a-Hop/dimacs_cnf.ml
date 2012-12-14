exception SAT of int list;;
exception UNSAT;;

(**
 * Write a propositional logic formula in CNF to a file in the DIMACS CNF
 * format.
 *
 * @param formula The formula to write to a file.
 * @param file_name The name (or path) of the file in which to write.
 *)
let write_formula_to_file (formula, nb_variables) file_name =
  let rec write_clause_to_file clause file =
    match clause with
        [] -> Printf.fprintf file "0\n"
      | h::t -> Printf.fprintf file "%d " h; write_clause_to_file t file
  in

  let rec write_formula_to_file_aux formula file =
    match formula with
        [] -> ()
      | h::t -> write_clause_to_file h file; write_formula_to_file_aux t file
  in

  try
    let file = open_out file_name in
    Printf.fprintf file "p cnf %d %d\n" nb_variables (List.length formula);
    write_formula_to_file_aux formula file;
    close_out file
  with
      Sys_error e -> failwith (Printf.sprintf "Error while opening file : %s."
                                              e)
;;

(**
 * Get the solution for the satisfiability problem for a given formula from a
 * DIMACS CNF result file.
 *
 * @param file_name The name (or path) of the file from which to read.
 * @raise SAT Exception raised when the formula is satisfiable, along with a
 * solution.
 * @raise UNSAT Exception raised when the formula is unsatisfiable.
 *)
let read_solution_from_file file_name =
  try
    let file = open_in file_name in
    let result = Scanf.fscanf file "%s\n" (fun s -> s) in

    if result = "SAT"
    then
      begin
        let solution = ref [] in
        let literal = ref (Scanf.fscanf file "%d" (fun i -> i)) in

        while !literal <> 0 do
          solution := !literal :: !solution;
          literal := Scanf.fscanf file " %d" (fun i -> i)
        done;

        close_in file;
        raise (SAT !solution)
      end

    else
      begin
        close_in file;
        raise UNSAT
      end
  with
      Sys_error e -> failwith (Printf.sprintf "Error while opening file : %s."
                                              e)
;;

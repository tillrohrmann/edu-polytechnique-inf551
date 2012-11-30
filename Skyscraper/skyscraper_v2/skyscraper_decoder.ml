(**
 * Decode the solution to an instance of the skyscraper game given by the DPLL
 * algorithm.
 *
 * @param solution The solution list given by the DPLL algorithm.
 * @param n The size of the grid.
 * @return An array containing the solution.
 *)
let skyscraper_decode solution n =
  let sol = List.filter (fun x -> x > 0) solution in
  let res = Array.make_matrix n n 0 in

  let unvar x =
    let x' = x-1 in
    let k = x' mod n in
    let x' = x' / n in
    let j = x' mod n in
    let i = x' / n in
    (i,j,k) in

  List.iter (fun x -> let (i,j,k) = unvar x in res.(i).(j) <- k+1) sol;
  res
;;

(**
 * Print the solution to an instance of the skyscraper game.
 *
 * @param m The array containing the solution.
 *)
let print_skyscraper_solution m =
  let n = Array.length m in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      print_int m.(i).(j);
      print_char ' '
    done;
    print_newline ()
  done;
;;

if Array.length Sys.argv <> 3
then
  Printf.fprintf stderr "Usage : %s <instance_file> <result_file>\n"
                 Sys.argv.(0)
else
  try
    let file = open_in Sys.argv.(1) in
    let size = Scanf.fscanf file "%d" (fun i -> i) in
    close_in file;

    try
      Dimacs_cnf.read_solution_from_file Sys.argv.(2)
    with
        Dimacs_cnf.SAT solution ->
          print_skyscraper_solution (skyscraper_decode solution size)
      | Dimacs_cnf.UNSAT -> failwith "No solution."
  with
      Scanf.Scan_failure s -> failwith (Printf.sprintf
                                          "Bad file structure : %s." s)
    | End_of_file -> failwith "Bad file structure : end-of-file encountered \
                               before it should have been the case."
    | Sys_error e -> failwith (Printf.sprintf "Error while opening file : %s."
                                              e)
;;

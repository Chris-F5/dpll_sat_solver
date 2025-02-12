open Dpll_sat_solver.Sat
open Dpll_sat_solver.Sodoku

let sod = load_sodoku "080070030260050018000000400000602000390010086000709000004000800810040052050090070"
let prop = sodoku_to_prop_expr sod
let () = print_string (string_of_sodoku sod)

let () = print_endline "Solving..."
let result = sat_solve prop
let () = match result with
  | Some interp ->
    let sod_solved = interp_to_sodoku interp in
    print_endline "SOLVED:";
    print_string (string_of_sodoku sod_solved)
  | None -> print_endline "No Solution"

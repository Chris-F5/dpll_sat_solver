open Dpll_sodoku.Dpll
open Dpll_sodoku.Sodoku

let sod = load_sodoku "609000023003960001710000060580107004040008209906300500004073890005000307200040000"
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

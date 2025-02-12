open Dpll_sat_solver.Sat
open Dpll_sat_solver.Sodoku

let expr = Conjunction ( Variable "A", Complement ( Variable "B" ) )
let result = sat_solve expr

let () =
  print_endline
    ( (match result with | Some interp -> string_of_interp Fun.id interp | None -> "None")
    ^ " => "
    ^ (string_of_prop_expr Fun.id expr) )

let sod = load_sodoku "000093006000800900020006100000080053006000200370050000002500040001009000700130000"

let () = print_string (string_of_sodoku sod)

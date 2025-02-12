open Sat

let expr = Conjunction ( Variable "A", Complement ( Variable "B" ) )
let result = sat_solve expr

let () =
  print_endline
    ( (match result with | Some interp -> string_of_interp Fun.id interp | None -> "None")
    ^ " => "
    ^ (string_of_prop_expr Fun.id expr) )

open Sat

(* let e = Complement ( Disjunction ( Variable 1, Variable 2 ) ) *)
let e = Complement ( Disjunction ( Conjunction ( Variable 1, Disjunction(Variable 2, Variable 3) ), Disjunction ( Variable 4, Variable 5 ) ) )

let () = print_endline (
         (string_of_prop_expr string_of_int e)
       ^ " === "
       ^ (string_of_cnf_expr string_of_int (prop_to_cnf e))
       )

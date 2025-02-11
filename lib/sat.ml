type 'a prop_expr =
  | Variable of 'a
  | Complement of 'a prop_expr
  | Conjunction of 'a prop_expr * 'a prop_expr
  | Disjunction of 'a prop_expr * 'a prop_expr

type 'a literal =
  | Positive of 'a
  | Negative of 'a

type 'a cnf_expr = 'a literal list list


let string_of_literal f = function
  | Positive a -> f a
  | Negative a -> "!" ^ ( f a )

let rec string_of_prop_expr f = function
  | Variable a -> f a
  | Complement e -> "!" ^ (string_of_prop_expr f e)
  | Conjunction (e1,e2) -> "(" ^ (string_of_prop_expr f e1) ^ "*" ^ (string_of_prop_expr f e2) ^ ")"
  | Disjunction (e1,e2) -> "(" ^ (string_of_prop_expr f e1) ^ "+" ^ (string_of_prop_expr f e2) ^ ")"

let string_of_cnf_expr f cnf = String.concat "^" (
    List.map (fun c -> "{" ^ ( String.concat ", " (List.map (string_of_literal f) c) ) ^ "}") cnf
  )

let rec prop_to_cnf = function
  | Variable a -> [ [ Positive a ] ]
  | Complement (Variable a) -> [ [ Negative a ] ]
  | Complement (Complement e) -> prop_to_cnf e
  | Complement (Conjunction (e1, e2)) -> prop_to_cnf (Disjunction (Complement (e1), Complement(e2)))
  | Complement (Disjunction (e1, e2)) -> prop_to_cnf (Conjunction (Complement (e1), Complement(e2)))
  | Conjunction (e1, e2) -> (prop_to_cnf e1) @ (prop_to_cnf e2)
  | Disjunction (e1, e2) -> List.concat_map (fun c1 -> List.map (fun c2 -> c1 @ c2) (prop_to_cnf e2)) (prop_to_cnf e1)

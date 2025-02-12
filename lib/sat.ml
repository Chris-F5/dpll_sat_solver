type 'a prop_expr =
  | True
  | False
  | Variable of 'a
  | Complement of 'a prop_expr
  | Conjunction of 'a prop_expr * 'a prop_expr
  | Disjunction of 'a prop_expr * 'a prop_expr

type 'a literal =
  | Positive of 'a
  | Negative of 'a

type 'a cnf_expr = 'a literal list list
type 'a interpretation = ('a * bool) list


let complement = function
  | Positive a -> Negative a
  | Negative a -> Positive a

let string_of_literal f = function
  | Positive a -> f a
  | Negative a -> "!" ^ ( f a )
  
let literal_as_interp = function
  | Positive a -> (a, true)
  | Negative a -> (a, false)

let rec string_of_prop_expr f = function
  | True -> "t"
  | False -> "f"
  | Variable a -> f a
  | Complement e -> "!" ^ (string_of_prop_expr f e)
  | Conjunction (e1,e2) -> "(" ^ (string_of_prop_expr f e1) ^ "*" ^ (string_of_prop_expr f e2) ^ ")"
  | Disjunction (e1,e2) -> "(" ^ (string_of_prop_expr f e1) ^ "+" ^ (string_of_prop_expr f e2) ^ ")"

let string_of_cnf_expr f cnf = String.concat "^" (
    List.map (fun c -> "{" ^ ( String.concat ", " (List.map (string_of_literal f) c) ) ^ "}") cnf
  )

let string_of_interp f interp = String.concat "," (
    List.map (function | v,true -> (f v) ^ "=t" | v,false -> (f v) ^ "=f" ) interp
  )

let rec conjunction = function
  | t::[] -> t
  | t::ts -> Conjunction (t, (conjunction ts))
  | [] -> True

let rec disjunction = function
  | t::[] -> t
  | t::ts -> Disjunction (t, (disjunction ts))
  | [] -> False

let rec prop_to_cnf = function
  | True -> [ ]
  | False -> [ [ ] ]
  | Variable a -> [ [ Positive a ] ]
  | Complement (True) -> prop_to_cnf False
  | Complement (False) -> prop_to_cnf True
  | Complement (Variable a) -> [ [ Negative a ] ]
  | Complement (Complement e) -> prop_to_cnf e
  | Complement (Conjunction (e1, e2)) -> prop_to_cnf (Disjunction (Complement (e1), Complement(e2)))
  | Complement (Disjunction (e1, e2)) -> prop_to_cnf (Conjunction (Complement (e1), Complement(e2)))
  | Conjunction (e1, e2) -> (prop_to_cnf e1) @ (prop_to_cnf e2)
  | Disjunction (e1, e2) -> List.concat_map (fun c1 -> List.map (fun c2 -> c1 @ c2) (prop_to_cnf e2)) (prop_to_cnf e1)

let rec is_clause_tautology = function
  | l::ls -> if List.mem (complement l) ls then true else is_clause_tautology ls
  | [] -> false

let del_tautological_clauses cnf = List.filter (fun c -> not ( is_clause_tautology c )) cnf

let propigate_unit_clause l cnf =
    (List.filter (fun c -> not (List.mem l c)) (List.map (List.filter (fun l' -> l' <> complement l)) cnf))

let rec propigate_unit_clauses cnf interp =
  match List.find_opt (fun c -> List.length c == 1) cnf with
  | Some (l::[]) ->
    propigate_unit_clauses (propigate_unit_clause l cnf) ((literal_as_interp l)::interp)
  | Some (_) -> assert false
  | None -> (cnf, interp)

let uniq_cons x xs = if List.mem x xs then xs else x :: xs
let uniq xs = List.fold_right uniq_cons xs []

let del_pure_literals cnf interp =
    let literals = uniq (List.concat cnf) in
    let pure_literals = List.filter (fun l -> not (List.mem (complement l) literals)) literals in
    (List.fold_right propigate_unit_clause pure_literals cnf,
     (List.map literal_as_interp pure_literals) @ interp)

let rec dpll_sat cnf interp =
  let cnf = del_tautological_clauses cnf in
  let cnf, interp = propigate_unit_clauses cnf interp in
  let cnf, interp = del_pure_literals cnf interp in
  if List.mem [] cnf then None else
  match cnf with
    | (l::ls)::cs ->
      let v = match l with | Positive v -> v | Negative v -> v in
      ( match dpll_sat (propigate_unit_clause (Positive v) (ls::cs)) ((v,true)::interp) with
        | Some (interp) -> Some (interp)
        | None -> dpll_sat (propigate_unit_clause (Negative v) (ls::cs)) ((v,false)::interp) )
    | [[]] -> assert false
    | []::_ -> assert false
    | [] -> Some (interp)

let sat_solve expr = dpll_sat (prop_to_cnf expr) []

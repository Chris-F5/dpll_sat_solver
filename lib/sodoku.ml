open Dpll

type sodoku_puzzle = int list list
type sodoku_predicate = int * int * int

let rec take n l = match n,l with
  | 0,r -> [],r
  | _,[] -> [],[]
  | n,x::xs -> let a,b = take (n-1) xs in x::a,b

let load_sodoku str =
  let ints = List.map (fun c -> int_of_char c - int_of_char '0') (List.of_seq (String.to_seq str)) in
  assert (List.for_all (fun i -> 0 <= i && i <= 9) ints) ;
  let rec reshape x y = function
    | [] -> []
    | ls ->
      let row,rest = take x ls in assert (List.length row == x); row::(reshape x (y-1) rest) in
  reshape 9 9 ints

let export_sodoku sod =
  String.concat "" (List.map (fun row -> String.concat "" (List.map (fun c -> string_of_int c) row) ) sod)

let rec string_of_sodoku sod =
  let string_of_cell c = if c <> 0 then string_of_int c else "." in
  let rec string_of_row = function
    | c::[] -> string_of_cell c
    | c::cs -> (string_of_cell c)
             ^ (if List.length cs mod 3 == 0 then " | " else " ")
             ^ (string_of_row cs)
    | [] -> "" in
  let sep = "\n------+-------+------\n" in
  match sod with
    | r::[] -> (string_of_row r) ^ "\n"
    | r::rs -> (string_of_row r)
             ^ (if List.length rs mod 3 == 0 then sep else "\n")
             ^ string_of_sodoku rs
    | [] -> ""

let string_of_sodoku_predicate (i,j,n) = "M" ^ (string_of_int i) ^ (string_of_int j) ^ (string_of_int n)

let ns = List.init 9 (fun i -> i+1)
let nns = List.flatten ( List.init 9 (fun i -> List.init 9 (fun j -> i+1,j+1)) )
let nnns = List.flatten ( List.init 9 ( fun i -> (List.flatten ( List.init 9 (fun j -> List.init 9 (fun k -> i+1,j+1,k+1)) ) ) ) )

let sodoku_get sod (i,j) = List.nth (List.nth sod (i-1)) (j-1)

let sodoku_to_prop_expr sod =
  let cross1 f = List.map f ns in
  let cross2 f = List.map f nns in
  let cross3 f = List.map f nnns in
  let uniq_term = conjunction ( cross3 (fun (i,j,n) -> Disjunction (
      Complement ( Variable (i,j,n) ),
      conjunction (
        (cross1 (fun i' -> if i' == i then True else Complement (Variable (i',j,n)))) @
        (cross1 (fun j' -> if j' == j then True else Complement (Variable (i,j',n)))) @
        (cross1 (fun n' -> if n' == n then True else Complement (Variable (i,j,n')))) @
        (nns
          |> List.filter (fun (i',j') -> (i'-1)/3==(i-1)/3 && (j'-1)/3==(j-1)/3 && (i'<>i || j'<>j) )
          |> List.map (fun (i',j') -> Complement (Variable(i',j',n))) )
      )
    ))) in
  let complete_term = conjunction ( cross2 (fun (i,j) -> disjunction (
      cross1 (fun n -> Variable(i,j,n))
    ))) in
  let constraint_term = conjunction (
      List.map (fun (i,j,n) -> Variable (i, j, n))
        (List.filter (fun (i,j,n) -> (sodoku_get sod (i,j)) == n) nnns)
    ) in
  conjunction [uniq_term; complete_term; constraint_term]

let interp_to_sodoku interp = List.init 9 (fun x -> List.init 9 (fun y ->
    let i = x+1 in let j = y+1 in
    let n = List.find_opt (fun n -> List.mem ((i, j, n), true) interp) ns in
    match n with
      | Some n -> n
      | None -> 0
  ))

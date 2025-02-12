(* open Sat *)

type sodoku_puzzel = int list list

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

let rec string_of_sodoku sod =
  let string_of_cell c = if c <> 0 then string_of_int c else "." in
  let rec string_of_row = function
    | c::[] -> string_of_cell c
    | c::cs -> (string_of_cell c)
             ^ (if List.length cs mod 3 == 0 then " | " else " ")
             ^ (string_of_row cs)
    | [] -> "" in
  let sep = "\n-------+-------+------\n" in
  match sod with
    | r::[] -> (string_of_row r) ^ "\n"
    | r::rs -> (string_of_row r)
             ^ (if List.length rs mod 3 == 0 then sep else "\n")
             ^ string_of_sodoku rs
    | [] -> ""

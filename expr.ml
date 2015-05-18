type t =
  | True
  | False
  | Var of string
  | Neg of t
  | Conj of t * t
  | Disj of t * t
  | Impl of t * t
  | Equiv of t * t
  | Forall of string * t
  | Exists of string * t

let empty =
  fun x -> failwith ("Undefined variable: " ^ x)
                    
let extend f k v =
  fun x -> if x = k then v else f x

let eval e =
  let rec eval_aux env = function
    | True -> true
    | False -> false
    | Var x -> env x
    | Neg e -> not (eval_aux env e)
    | Conj (e1, e2) -> (eval_aux env e1) && (eval_aux env e2)
    | Disj (e1, e2) -> (eval_aux env e1) || (eval_aux env e2)
    | Impl (e1, e2) -> not (eval_aux env e1) || (eval_aux env e2)
    | Equiv (e1, e2) -> (eval_aux env e1) = (eval_aux env e2)
    | Forall (x, e) ->
       let e1 = eval_aux (extend env x true) e in
       let e2 = eval_aux (extend env x false) e in
       e1 && e2
    | Exists (x, e) ->
       let e1 = eval_aux (extend env x true) e in
       let e2 = eval_aux (extend env x false) e in
       e1 || e2
  in
  eval_aux empty e

let rec to_string =
  let parens s = "(" ^ s ^ ")" in
  function
  | True -> "TRUE"
  | False -> "FALSE"
  | Var x -> x
  | Neg e -> "~" ^ to_string e |> parens
  | Conj (e1, e2) -> to_string e1 ^ " /\\ " ^ to_string e2 |> parens
  | Disj (e1, e2) -> to_string e1 ^ " \\/ " ^ to_string e2 |> parens
  | Impl (e1, e2) -> to_string e1 ^ " => " ^ to_string e2 |> parens
  | Equiv (e1, e2) -> to_string e1 ^ " <=> " ^ to_string e2 |> parens
  | Forall (x, e) -> "FORALL " ^ x ^ " : " ^ to_string e |> parens
  | Exists (x, e) -> "EXISTS " ^ x ^ " : " ^ to_string e |> parens

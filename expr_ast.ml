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

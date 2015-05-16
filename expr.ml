type t =
  | True
  | False
  | Var of string
  | Neg of t
  | Conj of t * t
  | Disj of t * t
  | Impl of t * t
  | Equiv of t * t

let rec eval env = function
  | True -> true
  | False -> false
  | Var x -> env x
  | Neg e -> not (eval env e)
  | Conj (e1, e2) -> (eval env e1) && (eval env e2)
  | Disj (e1, e2) -> (eval env e1) || (eval env e2)
  | Impl (e1, e2) -> not (eval env e1) || (eval env e2)
  | Equiv (e1, e2) -> (eval env e1) = (eval env e2)

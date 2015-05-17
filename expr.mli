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

val eval : t -> bool

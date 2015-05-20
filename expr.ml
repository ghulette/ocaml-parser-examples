open Expr_ast

type t = Expr_ast.t

let to_string = Expr_ast.to_string
       
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

let parse fname menhir_parser lexbuf =
  let open Lexing in
  let pos = {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0
  } in
  let lexer () =
    let tok = Expr_lexer.token lexbuf in (tok, pos, pos)
  in
  MenhirLib.Convert.Simplified.traditional2revised menhir_parser lexer

let of_channel ch =
  try
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    parse "" Expr_parser.main lexbuf
  with
    Expr_parser.Error -> exit (-1)

open Printf

let parse fname menhir_parser lexbuf =
  let open Lexing in
  let pos = {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0
  } in
  let lexer () =
    let tok = Lexer.token lexbuf in
    (tok, pos, pos)
  in
  MenhirLib.Convert.Simplified.traditional2revised menhir_parser lexer

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  try
    let e = parse "" Parser.main lexbuf in
    let e_str = Expr.to_string e in
    let v = Expr.eval e in
    printf "[%s] = %b\n" e_str v
  with
    Parser.Error -> exit (-1)

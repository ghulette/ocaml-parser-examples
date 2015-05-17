let parse fname menhir_parser lexbuf =
  let open Lexing in
  let position =
    ref { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  let lexer () =
    let old_position = !position in
    let tok = Lexer.token lexbuf in
    position := {!position with pos_lnum = 1};
    let new_position = !position
    in (tok, old_position, new_position) in
  let revised_parser =
    MenhirLib.Convert.Simplified.traditional2revised menhir_parser
  in revised_parser lexer

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  try
    let _ = parse "" Parser.main lexbuf in
    print_endline "ok"
  with
  | Parser.Error -> exit (-1)

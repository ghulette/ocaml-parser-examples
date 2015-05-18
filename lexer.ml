let digit   = [%sedlex.regexp? '0'..'9']
let number  = [%sedlex.regexp? Plus digit]
let letter  = [%sedlex.regexp? 'a'..'z'|'A'..'Z']

let rec token buf =
  let open Parser in
  match%sedlex buf with
  | 0x2200 | "FORALL" -> FORALL
  | 0x2203 | "EXISTS" -> EXISTS
  | 0x00AC | '~' -> NEG
  | 0x2207 | "/\\" -> CONJ
  | 0x2208 | "\\/" -> DISJ
  | 0x21D2 | "=>" -> IMPL
  | 0x21D4 | "<=>" -> EQUIV
  | 0x22A4 | "TRUE" -> TRUE
  | 0x22A5 | "FALSE" -> FALSE
  | ':' -> COLON
  | '(' -> LPAREN
  | ')' -> RPAREN
  | 'a'..'z', Star ('A'..'Z' | 'a'..'z' | digit) ->
     let id = Sedlexing.Utf8.lexeme buf in
     VAR id
  | Plus white_space -> token buf
  | eof -> EOF
(*
  | any ->
     let tok = Sedlexing.Utf8.lexeme buf in
     failwith ("Unexpected character: " ^ tok)
*)
  | _ -> failwith "Lexing error"

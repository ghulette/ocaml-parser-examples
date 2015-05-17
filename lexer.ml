let sym_forall = [%sedlex.regexp? 0x2200]
let sym_exists = [%sedlex.regexp? 0x2203]
let sym_neg    = [%sedlex.regexp? 0x2207]
let sym_conj   = [%sedlex.regexp? 0x2207]
let sym_disj   = [%sedlex.regexp? 0x2208]
let sym_impl   = [%sedlex.regexp? 0x2207]
let sym_equiv  = [%sedlex.regexp? 0x2207]

let digit   = [%sedlex.regexp? '0'..'9']
let number  = [%sedlex.regexp? Plus digit]
let letter  = [%sedlex.regexp? 'a'..'z'|'A'..'Z']
let symbols = [%sedlex.regexp? (Chars "+*-/:=")
              | sym_forall
              | sym_exists
              | sym_neg
              | sym_conj
              | sym_disj
              | sym_impl
              | sym_equiv]

let rec token buf =
  let open Parser in
  match%sedlex buf with
  | ':' -> COLON
  (*
  | number ->
     let tok = Sedlexing.Utf8.lexeme buf in
     Printf.printf "Number: %s\n" tok; token buf
  | letter, Star ('A'..'Z' | 'a'..'z' | digit) ->
     let tok = Sedlexing.Utf8.lexeme buf in
     Printf.printf "Id: %s\n" tok; token buf
  | Plus symbols ->
     let tok = Sedlexing.Utf8.lexeme buf in
     Printf.printf "Symbol: %s\n" tok; token buf
  | Plus white_space ->
     token buf
  | eof -> print_endline "EOF"
  | any ->
     let tok = Sedlexing.Utf8.lexeme buf in
     failwith ("Unexpected character: " ^ tok)
   *)
  | _ -> failwith "Lexing error"

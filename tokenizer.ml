let sym_forall     = [%sedlex.regexp? 0x2200]
let sym_exists     = [%sedlex.regexp? 0x2203]
let sym_element_of = [%sedlex.regexp? 0x2208]

let digit   = [%sedlex.regexp? '0'..'9']
let number  = [%sedlex.regexp? Plus digit]
let letter  = [%sedlex.regexp? 'a'..'z'|'A'..'Z']
let symbols = [%sedlex.regexp? (Chars "+*-/:=")
              | sym_forall
              | sym_exists
              | sym_element_of]

let rec token buf =
  match%sedlex buf with
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
  | _ -> failwith "Lexing error"

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  token lexbuf

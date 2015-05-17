let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Tokenizer.token lexbuf

open Printf

let () =
  let e = Expr.of_channel stdin in
  let s = Expr.to_string e in
  let v = Expr.eval e in
  printf "The proposition %s is %b\n" s v

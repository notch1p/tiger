open Lex

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let res = Tiger.read lexbuf in
      Format.printf "%s%!" @@ Tokens.show_realtoken res
    done
  with
  | Tiger.Eof ->
    Format.printf "\n";
    exit 0
;;

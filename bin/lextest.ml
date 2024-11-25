let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let res = Lex.Tiger.read lexbuf in
      Format.printf "%s%!" res
    done
  with
  | Lex.Tiger.Eof ->
    Format.printf "\n";
    exit 0
;;

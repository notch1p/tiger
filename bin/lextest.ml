open Lex

let () =
  Array.iteri
    (fun i x ->
      if i <> 0
      then (
        Ocolor_format.printf "\n@{<ul;bold>File \"%s\"@}:%!" x;
        let file = open_in x in
        try
          let lexbuf = Lexing.from_channel file in
          Lexing.set_filename lexbuf x;
          while true do
            let res = Tiger.read lexbuf in
            (* ignore res *)
            Format.printf "%s%!" @@ Tokens.show_realtoken res
          done
        with
        | Tiger.Eof -> close_in file))
    Sys.argv
;;

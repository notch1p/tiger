open Lex

let () =
  Array.iteri
    (fun i x ->
       if i <> 0
       then (
         let file = open_in x in
         try
           let lexbuf = Lexing.from_channel file in
           Lexing.set_filename lexbuf x;
           while true do
             Tiger.read lexbuf |> ignore (* ignore res *)
           done
         with
         | Tiger.Eof -> close_in file))
    Sys.argv
;;

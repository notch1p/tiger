open Lexing
open Lex.Tiger
module I = Syntax.Tiger.MenhirInterpreter

let rec loop lexbuf (cp : unit I.checkpoint) =
  match cp with
  | I.InputNeeded _env ->
    let token = read lexbuf in
    let startp = lexbuf.lex_start_p
    and endp = lexbuf.lex_curr_p in
    let cp = I.offer cp (token, startp, endp) in
    loop lexbuf cp
  | I.Shifting _ | I.AboutToReduce _ ->
    let cp = I.resume cp in
    loop lexbuf cp
  | I.HandlingError _env ->
    let fname, lnum, s, p =
      match lexbuf.lex_curr_p, lexbuf.lex_start_p with
      | { pos_fname; pos_cnum; pos_bol; pos_lnum }, { pos_cnum = spos_cnum; _ } ->
        pos_fname, pos_lnum, spos_cnum - pos_bol + 1, pos_cnum - pos_bol + 1
    in
    Printf.fprintf stderr "Parsing error within %s:%d:%d-%d\n%!" fname lnum s p
  | I.Accepted _ -> ()
  | I.Rejected -> assert false
;;

let prog = Syntax.Tiger.Incremental.prog

let () =
  Array.iteri
    (fun i x ->
       if i <> 0
       then (
         let file = open_in x in
         let lexbuf = from_channel file in
         Lexing.set_filename lexbuf x;
         try loop lexbuf (prog lexbuf.lex_start_p) with
         | Eof -> ()))
    Sys.argv
;;

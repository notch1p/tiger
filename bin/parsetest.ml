open Lexing
open Lex.Tiger
open Syntax.Errormsg
module I = Syntax.Tiger.MenhirInterpreter
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* let rec loop lexbuf (cp : unit I.checkpoint) =
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
    let txt = Bytes.to_string lexbuf.lex_buffer in
    Syntax.Errormsg.fail txt lexbuf.lex_buffer cp
  (* let fname, lnum, s, p =
      match lexbuf.lex_curr_p, lexbuf.lex_start_p with
      | { pos_fname; pos_cnum; pos_bol; pos_lnum }, { pos_cnum = spos_cnum; _ } ->
        pos_fname, pos_lnum, spos_cnum - pos_bol + 1, pos_cnum - pos_bol + 1
    in *)
  (*let sn, lex_start_p, lex_curr_p = state _env in
    Lex.Errormsg.(
      emit_error
        { lexbuf with lex_start_p; lex_curr_p }
        (SyntaxError (Syntax.ParserError.message sn))) *)
  (* Printf.fprintf stderr "Parsing error within %s:%d:%d-%d\n%!" fname lnum s p *)
  | I.Accepted _ -> ()
  | I.Rejected -> assert false
;; *)

let[@warning "-32"] rec loop_handle' succeed fail read checkpoint =
  let open I in
  match checkpoint with
  | InputNeeded _ ->
    let triple = read () in
    let checkpoint = offer checkpoint triple in
    loop_handle' succeed fail read checkpoint
  | Shifting _ | AboutToReduce _ ->
    let checkpoint = resume checkpoint in
    loop_handle' succeed fail read checkpoint
  | HandlingError _ | Rejected ->
    fail checkpoint;
    let checkpoint = resume checkpoint in
    loop_handle' succeed fail read checkpoint
  | Accepted v -> succeed v
;;

let definition_or_else (tok : Syntax.Tiger.token) =
  match tok with
  | LET | FUNCTION -> `Def
  | SEMICOLON | RPAREN | RBRACK | END -> `Expr
  | _ -> `Nop
;;

let rec skipping_until_valid (sup : I.supplier) cp =
  let ((tok, _, _) as offer_tok) = sup () in
  match tok |> definition_or_else with
  | `Def -> I.offer cp offer_tok, sup
  | `Expr -> cp, sup
  | `Nop -> skipping_until_valid sup cp
;;

let rec loop_handle_undo'
          succeed
          fail
          read
          (inputneeded, checkpoint, last_red_cp)
          err_rec
          err_num
  =
  let open I in
  match checkpoint with
  | InputNeeded _ ->
    (* Update the last recorded [InputNeeded] checkpoint. *)
    let inputneeded = checkpoint in
    let triple = read () in
    let checkpoint = offer checkpoint triple in
    loop_handle_undo'
      succeed
      fail
      read
      (inputneeded, checkpoint, last_red_cp)
      err_rec
      err_num
  | Shifting _ ->
    loop_handle_undo'
      succeed
      fail
      read
      (inputneeded, resume checkpoint, last_red_cp)
      err_rec
      err_num
  | AboutToReduce _ ->
    (* Which strategy is passed to [resume] here is irrelevant,
         since this checkpoint is not [HandlingError _]. *)
    let checkpoint = resume checkpoint in
    loop_handle_undo'
      succeed
      fail
      read
      (inputneeded, checkpoint, inputneeded)
      err_rec
      err_num
  | Rejected | HandlingError _ ->
    if err_num > 3
    then Printf.eprintf "Too many errors. Abort."
    else (
      let warnmsg =
        if err_rec then "This diagnostic message may not be as accurate" else ""
      in
      let () = fail inputneeded checkpoint warnmsg in
      let cp, sup = skipping_until_valid read last_red_cp in
      loop_handle_undo' succeed fail sup (inputneeded, cp, last_red_cp) true (err_num + 1))
  (*| Rejected -> fail inputneeded checkpoint*)
  | Accepted v -> succeed v
;;

let prog = Syntax.Tiger.Incremental.prog

let () =
  Array.iteri
    (fun i x ->
       if i <> 0
       then (
         let lexbuf = L.init x (from_channel (open_in x)) in
         let sup = I.lexer_lexbuf_to_supplier read lexbuf in
         let buf, sup = E.wrap_supplier sup in
         let cp = prog lexbuf.lex_curr_p in
         try
           loop_handle_undo'
             (fun _ -> ())
             (fail' buf lexbuf.lex_buffer)
             sup
             (cp, cp, cp)
             false
             0
         with
         | Eof -> ()))
    Sys.argv
;;

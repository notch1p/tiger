exception SyntaxError of string

let emit_error
  ?(offset = 0)
  ({ lex_curr_p = { pos_fname; pos_lnum; pos_bol; pos_cnum }; _ } as lexbuf :
    Lexing.lexbuf)
  err
  =
  let lexbuf = Lexing.lexeme lexbuf in
  Ocolor_format.printf
    {|
@{<bold>File "%s", line %d, characters %d-%d@}:
%d | %s

@{<red>Error@}: %s
%!
|}
    pos_fname
    pos_lnum
    (pos_cnum - pos_bol + offset)
    (pos_cnum - pos_bol + String.length lexbuf + offset)
    pos_lnum
    lexbuf
    (match err with
     | SyntaxError s -> s
     | e -> raise e)
;;

let nums_of_newline (s : string) =
  if Sys.os_type = "Win32"
  then
    String.fold_left
      (fun a c -> if snd a = '\r' && c = '\n' then fst a + 1, c else fst a, c)
      (0, s.[0])
      s
    |> fst
  else String.fold_left (fun a c -> if c = '\n' then a + 1 else a) 0 s
;;

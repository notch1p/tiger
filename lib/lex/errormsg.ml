exception SyntaxError of string
exception TooManyError of string

let counter = ref 0
let reset_err_counter = counter := 0

let add_line_num (start_from : int) (spos : int) (epos : int) (s : string) =
  let add_cursor (s : string) (spos : int) (epos : int) =
    let sc = Bytes.make (epos - 1) ' ' in
    for i = 0 to epos - 2 do
      if s.[i] = '\t' then Bytes.set sc i s.[i];
      if i = spos - 1 then Bytes.set sc i '^';
      if i > spos - 1 then Bytes.set sc i '~'
    done;
    Bytes.unsafe_to_string sc
  in
  let line_num_str = Format.sprintf "%4d" start_from in
  Ocolor_format.asprintf
    "@{<hi_blue;bold>%s |@} %s\n%*s @{<hi_blue;bold>|@} %s"
    line_num_str
    s
    (String.length line_num_str)
    ""
    (Ocolor_format.asprintf "@{<green>%s@}" (add_cursor s spos epos))
;;

let emit_error
      ?(offset = 0)
      ({ lex_curr_p = { pos_fname; pos_lnum; pos_bol; pos_cnum }
       ; lex_start_p = { pos_cnum = spos_cnum; _ }
       ; _
       } as lexbuf :
        Lexing.lexbuf)
      err
  =
  if !counter > 5
  then (
    Ocolor_format.printf
      "@{<red;bold>Error@}: @{<bold;yellow>%s@}\n%!"
      "Aborting after 5 errors";
    exit 1)
  else (
    let spos = spos_cnum - pos_bol + offset + 1 in
    let epos = pos_cnum - pos_bol + offset + 1 in
    let lexbuf_line =
      Bytes.sub_string lexbuf.lex_buffer pos_bol (pos_cnum - pos_bol)
      |> add_line_num pos_lnum spos epos
    in
    Ocolor_format.printf
      {|
@{<bold>File "%s", line %d, characters %d-%d@}:

%s

@{<red;bold>Error@}: %s
%!
|}
      pos_fname
      pos_lnum
      spos
      epos
      lexbuf_line
      (match err with
       | SyntaxError s -> s
       | e -> raise e);
    counter := !counter + 1)
;;

module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Tiger.MenhirInterpreter

exception SyntaxError of string
exception TooManyError of string

let counter = ref 0
let reset_err_counter = counter := 0

let add_line_num (start_from : int) (spos : int) (epos : int) (s : string) =
  let add_cursor (s : string) (spos : int) (epos : int) =
    let sc = Bytes.make epos ' ' in
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

let[@warning "-32"] env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false
;;

let showErr txt pos = E.extract txt pos

let getRange txt cp i =
  match I.get i (env cp) with
  | Some (I.Element (_, _, pos1, pos2)) -> showErr txt (pos1, pos2)
  | None -> assert false
;;

let state e =
  match I.top e with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0
;;

let fail' buf lexbuf _ cp_last after =
  let ((p1, p2) as pos) = E.last buf in
  let loc = L.range pos in
  let start_pos = p1.pos_bol in
  let end_pos =
    try Bytes.index_from lexbuf start_pos '\n' with
    | _ -> Bytes.length lexbuf
  in
  let spos = p1.pos_cnum - p1.pos_bol + 1 in
  let epos = p2.pos_cnum - p2.pos_bol + 1 in
  let ctx =
    Bytes.sub_string lexbuf start_pos (end_pos - start_pos)
    |> add_line_num p1.pos_lnum spos epos
  in
  let msg = ParserError.message (state @@ env cp_last) in
  let msg = E.expand (getRange (Bytes.to_string lexbuf) cp_last) msg in
  Ocolor_format.printf
    {|
@{<bold>%s@}
%s

@{<red;bold>Error@}: %s@{<faint>%s@}
%!
|}
    loc
    ctx
    msg
    after
;;

let fail txt buf lexbuf cp =
  let ((p1, p2) as pos) = E.last buf in
  let loc = L.range pos in
  let start_pos = p1.pos_bol in
  let end_pos =
    try Bytes.index_from lexbuf start_pos '\n' with
    | _ -> Bytes.length lexbuf
  in
  let spos = p1.pos_cnum - p1.pos_bol + 1 in
  let epos = p2.pos_cnum - p2.pos_bol + 1 in
  let ctx =
    Bytes.sub_string lexbuf start_pos (end_pos - start_pos)
    |> add_line_num p1.pos_lnum spos epos
  in
  let msg = ParserError.message (state @@ env cp) in
  let msg = E.expand (getRange txt cp) msg in
  Ocolor_format.printf
    {|
@{<bold>%s@}
%s

@{<red;bold>Error@}: %s
%!
|}
    loc
    ctx
    msg
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
    let start_pos = pos_bol in
    let end_pos =
      try Bytes.index_from lexbuf.lex_buffer spos_cnum '\n' with
      | _ -> Bytes.length lexbuf.lex_buffer
    in
    let lexbuf_line =
      Bytes.sub_string lexbuf.lex_buffer start_pos (end_pos - start_pos)
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
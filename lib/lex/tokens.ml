type linenum = int [@@deriving show { with_path = false }]
type token = string

type realtoken =
  | TYPE of linenum * linenum
  | VAR of linenum * linenum
  | FUNCTION of linenum * linenum
  | BREAK of linenum * linenum
  | OF of linenum * linenum
  | END of linenum * linenum
  | IN of linenum * linenum
  | NIL of linenum * linenum
  | LET of linenum * linenum
  | DO of linenum * linenum
  | TO of linenum * linenum
  | FOR of linenum * linenum
  | WHILE of linenum * linenum
  | ELSE of linenum * linenum
  | THEN of linenum * linenum
  | IF of linenum * linenum
  | ARRAY of linenum * linenum
  | ASSIGN of linenum * linenum
  | OR of linenum * linenum
  | AND of linenum * linenum
  | GE of linenum * linenum
  | GT of linenum * linenum
  | LE of linenum * linenum
  | LT of linenum * linenum
  | NEQ of linenum * linenum
  | EQ of linenum * linenum
  | DIVIDE of linenum * linenum
  | TIMES of linenum * linenum
  | MINUS of linenum * linenum
  | PLUS of linenum * linenum
  | DOT of linenum * linenum
  | RBRACE of linenum * linenum
  | LBRACE of linenum * linenum
  | RBRACK of linenum * linenum
  | LBRACK of linenum * linenum
  | RPAREN of linenum * linenum
  | LPAREN of linenum * linenum
  | SEMICOLON of linenum * linenum
  | COLON of linenum * linenum
  | COMMA of linenum * linenum
  | STRING of string * linenum * linenum
  | INT of int * linenum * linenum
  | ID of string * linenum * linenum
  | EOF of linenum * linenum
[@@deriving show { with_path = false }]

let _TYPE (i, j) = show_realtoken @@ TYPE (i, j)
let _VAR (i, j) = show_realtoken @@ VAR (i, j)
let _FUNCTION (i, j) = show_realtoken @@ FUNCTION (i, j)
let _BREAK (i, j) = show_realtoken @@ BREAK (i, j)
let _OF (i, j) = show_realtoken @@ OF (i, j)
let _END (i, j) = show_realtoken @@ END (i, j)
let _IN (i, j) = show_realtoken @@ IN (i, j)
let _NIL (i, j) = show_realtoken @@ NIL (i, j)
let _LET (i, j) = show_realtoken @@ LET (i, j)
let _DO (i, j) = show_realtoken @@ DO (i, j)
let _TO (i, j) = show_realtoken @@ TO (i, j)
let _FOR (i, j) = show_realtoken @@ FOR (i, j)
let _WHILE (i, j) = show_realtoken @@ WHILE (i, j)
let _ELSE (i, j) = show_realtoken @@ ELSE (i, j)
let _THEN (i, j) = show_realtoken @@ THEN (i, j)
let _IF (i, j) = show_realtoken @@ IF (i, j)
let _ARRAY (i, j) = show_realtoken @@ ARRAY (i, j)
let _ASSIGN (i, j) = show_realtoken @@ ASSIGN (i, j)
let _OR (i, j) = show_realtoken @@ OR (i, j)
let _AND (i, j) = show_realtoken @@ AND (i, j)
let _GE (i, j) = show_realtoken @@ GE (i, j)
let _GT (i, j) = show_realtoken @@ GT (i, j)
let _LE (i, j) = show_realtoken @@ LE (i, j)
let _LT (i, j) = show_realtoken @@ LT (i, j)
let _NEQ (i, j) = show_realtoken @@ NEQ (i, j)
let _EQ (i, j) = show_realtoken @@ EQ (i, j)
let _DIVIDE (i, j) = show_realtoken @@ DIVIDE (i, j)
let _TIMES (i, j) = show_realtoken @@ TIMES (i, j)
let _MINUS (i, j) = show_realtoken @@ MINUS (i, j)
let _PLUS (i, j) = show_realtoken @@ PLUS (i, j)
let _DOT (i, j) = show_realtoken @@ DOT (i, j)
let _RBRACE (i, j) = show_realtoken @@ RBRACE (i, j)
let _LBRACE (i, j) = show_realtoken @@ LBRACE (i, j)
let _RBRACK (i, j) = show_realtoken @@ RBRACK (i, j)
let _LBRACK (i, j) = show_realtoken @@ LBRACK (i, j)
let _RPAREN (i, j) = show_realtoken @@ RPAREN (i, j)
let _LPAREN (i, j) = show_realtoken @@ LPAREN (i, j)
let _SEMICOLON (i, j) = show_realtoken @@ SEMICOLON (i, j)
let _COLON (i, j) = show_realtoken @@ COLON (i, j)
let _COMMA (i, j) = show_realtoken @@ COMMA (i, j)
let _STRING (s, i, j) = show_realtoken @@ STRING (s, i, j)
let _INT (c, i, j) = show_realtoken @@ INT (c, i, j)
let _ID (s, i, j) = show_realtoken @@ ID (s, i, j)
let _EOF (i, j) = show_realtoken @@ EOF (i, j)

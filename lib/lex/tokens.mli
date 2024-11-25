type linenum = int
val pp_linenum : Format.formatter -> linenum -> unit
val show_linenum : linenum -> string
type token = string
type realtoken =
    TYPE of linenum * linenum
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
  | STRING of token * linenum * linenum
  | INT of linenum * linenum * linenum
  | ID of token * linenum * linenum
  | EOF of linenum * linenum
val pp_realtoken : Format.formatter -> realtoken -> unit
val show_realtoken : realtoken -> string
val _TYPE : linenum * linenum -> string
val _VAR : linenum * linenum -> string
val _FUNCTION : linenum * linenum -> string
val _BREAK : linenum * linenum -> string
val _OF : linenum * linenum -> string
val _END : linenum * linenum -> string
val _IN : linenum * linenum -> string
val _NIL : linenum * linenum -> string
val _LET : linenum * linenum -> string
val _DO : linenum * linenum -> string
val _TO : linenum * linenum -> string
val _FOR : linenum * linenum -> string
val _WHILE : linenum * linenum -> string
val _ELSE : linenum * linenum -> string
val _THEN : linenum * linenum -> string
val _IF : linenum * linenum -> string
val _ARRAY : linenum * linenum -> string
val _ASSIGN : linenum * linenum -> string
val _OR : linenum * linenum -> string
val _AND : linenum * linenum -> string
val _GE : linenum * linenum -> string
val _GT : linenum * linenum -> string
val _LE : linenum * linenum -> string
val _LT : linenum * linenum -> string
val _NEQ : linenum * linenum -> string
val _EQ : linenum * linenum -> string
val _DIVIDE : linenum * linenum -> string
val _TIMES : linenum * linenum -> string
val _MINUS : linenum * linenum -> string
val _PLUS : linenum * linenum -> string
val _DOT : linenum * linenum -> string
val _RBRACE : linenum * linenum -> string
val _LBRACE : linenum * linenum -> string
val _RBRACK : linenum * linenum -> string
val _LBRACK : linenum * linenum -> string
val _RPAREN : linenum * linenum -> string
val _LPAREN : linenum * linenum -> string
val _SEMICOLON : linenum * linenum -> string
val _COLON : linenum * linenum -> string
val _COMMA : linenum * linenum -> string
val _STRING : string * linenum * linenum -> string
val _INT : int * linenum * linenum -> string
val _ID : string * linenum * linenum -> string
val _EOF : linenum * linenum -> string

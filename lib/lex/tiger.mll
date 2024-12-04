{
open Lexing
open Tokens
open Errormsg
exception Eof
let (spos, epos) = (lexeme_start, lexeme_end)
let _DEBUG_NONPRINTABLE s = Format.printf "%s%!" s
}

let whitespace   = [' ' '\t']
let newline      = '\r' | '\n' | "\r\n" | '\x0C'
let di           = ['0'-'9']
let integers     = ['0'-'9']+
let letter       = ['a'-'z' 'A'-'Z']
let id           = letter+ (di | letter | '_')*

rule read = parse
| whitespace+   { (* _DEBUG_NONPRINTABLE (lexeme lexbuf) *)
                   read lexbuf}
| newline       { (* _DEBUG_NONPRINTABLE (lexeme lexbuf) *)
                   new_line lexbuf
                 ; read lexbuf}
| "while"       {WHILE              (spos lexbuf, epos lexbuf)}
| "for"         {FOR                (spos lexbuf, epos lexbuf)}
| "to"          {TO                 (spos lexbuf, epos lexbuf)}
| "break"       {BREAK              (spos lexbuf, epos lexbuf)}
| "let"         {LET                (spos lexbuf, epos lexbuf)}
| "in"          {IN                 (spos lexbuf, epos lexbuf)}
| "end"         {END                (spos lexbuf, epos lexbuf)}
| "function"    {FUNCTION           (spos lexbuf, epos lexbuf)}
| "var"         {VAR                (spos lexbuf, epos lexbuf)}
| "type"        {TYPE               (spos lexbuf, epos lexbuf)}
| "array"       {ARRAY              (spos lexbuf, epos lexbuf)}
| "if"          {IF                 (spos lexbuf, epos lexbuf)}
| "then"        {THEN               (spos lexbuf, epos lexbuf)}
| "else"        {ELSE               (spos lexbuf, epos lexbuf)}
| "do"          {DO                 (spos lexbuf, epos lexbuf)}
| "of"          {OF                 (spos lexbuf, epos lexbuf)}
| "nil"         {NIL                (spos lexbuf, epos lexbuf)}
| ","           {COMMA              (spos lexbuf, epos lexbuf)}
| ":"           {COLON              (spos lexbuf, epos lexbuf)}
| ";"           {SEMICOLON          (spos lexbuf, epos lexbuf)}
| "("           {LPAREN             (spos lexbuf, epos lexbuf)}
| ")"           {RPAREN             (spos lexbuf, epos lexbuf)}
| "["           {LBRACE             (spos lexbuf, epos lexbuf)}
| "]"           {RBRACE             (spos lexbuf, epos lexbuf)}
| "{"           {LBRACK             (spos lexbuf, epos lexbuf)}
| "}"           {RBRACK             (spos lexbuf, epos lexbuf)}
| "."           {DOT                (spos lexbuf, epos lexbuf)}
| "+"           {PLUS               (spos lexbuf, epos lexbuf)}
| "-"           {MINUS              (spos lexbuf, epos lexbuf)}
| "*"           {TIMES              (spos lexbuf, epos lexbuf)}
| "/"           {DIVIDE             (spos lexbuf, epos lexbuf)}
| "="           {EQ                 (spos lexbuf, epos lexbuf)}
| "<>"          {NEQ                (spos lexbuf, epos lexbuf)}
| "<"           {LT                 (spos lexbuf, epos lexbuf)}
| "<="          {LE                 (spos lexbuf, epos lexbuf)}
| ">"           {GT                 (spos lexbuf, epos lexbuf)}
| ">="          {GE                 (spos lexbuf, epos lexbuf)}
| "&"           {AND                (spos lexbuf, epos lexbuf)}
| "|"           {OR                 (spos lexbuf, epos lexbuf)}
| ":="          {ASSIGN             (spos lexbuf, epos lexbuf)}
| '='           {EQ                 (spos lexbuf, epos lexbuf)}
| '"'           {read_str (spos lexbuf) (Buffer.create 80) lexbuf}
| id            {ID                 (lexeme lexbuf
                                     , spos lexbuf
                                     , epos lexbuf)}
| integers      {INT                (int_of_string (lexeme lexbuf)
                                     , spos lexbuf
                                     , epos lexbuf)}
| "/*"          {comment 1 lexbuf; read lexbuf}
| _             {emit_error lexbuf (SyntaxError ("Illegal character")); read lexbuf}
| eof           {raise Eof}


and comment depth = parse
| "*/"                    {if depth = 1 then () else comment (depth - 1) lexbuf}
| "/*"                    {comment (depth + 1) lexbuf}
| newline                 {new_line lexbuf; comment depth lexbuf}
| [^ '*' '/' '\n']+       {comment depth lexbuf}
| '*'                     {comment depth lexbuf}
| '/'                     {comment depth lexbuf}
| _                       {comment depth lexbuf}
| eof                     {raise (SyntaxError "Unclosed comments")}

and read_str spos strbuf = parse
| '"'           {STRING             (Buffer.contents strbuf, spos, epos lexbuf)}
| newline       {new_line lexbuf; Buffer.add_char strbuf '\n'; read_str spos strbuf lexbuf}
| '\\' 'n'      {Buffer.add_char strbuf '\n'; read_str spos strbuf lexbuf}
| '\\' 't'      {Buffer.add_char strbuf '\t'; read_str spos strbuf lexbuf}

| '\\' newline  {new_line lexbuf; handle_nonprintable lexbuf; read_str spos strbuf lexbuf}
| '\\' whitespace
                {handle_nonprintable lexbuf;  read_str spos strbuf lexbuf}

| '\\' (di as d1) (di as d2) (di as d3)
                {let code = (Char.code d1 - Char.code '0') * 100 + 
                            (Char.code d2 - Char.code '0') * 10  +
                            (Char.code d3 - Char.code '0')       in
                Buffer.add_char strbuf (Char.chr code); 
                read_str spos strbuf lexbuf}

| '\\' '"'      {Buffer.add_char strbuf '\"'; read_str spos strbuf lexbuf}
| '\\' '\\'     {Buffer.add_char strbuf '\\'; read_str spos strbuf lexbuf}


| [^ '\\' '"' '\n']+ {Buffer.add_string strbuf (lexeme lexbuf); read_str spos strbuf lexbuf}
| '\\' [^ '\n']
                {emit_error lexbuf (SyntaxError ("Undefined escape sequence")); read_str spos strbuf lexbuf}
| _             {raise (SyntaxError ("Uncaught error when lexing string"))}
| eof           {raise (SyntaxError ("Expecting closing quote"))}

and handle_nonprintable = parse
| '\\'          {()}
| newline       {new_line lexbuf; handle_nonprintable lexbuf}
| whitespace    {handle_nonprintable lexbuf}
| "/*"          {comment 1 lexbuf; handle_nonprintable lexbuf}
| [^ '\r' '\n' ' ' '\t' '\\' ]+             
                {emit_error lexbuf (SyntaxError ("Garbage within \\...\\, only non-printable characters are allowed here")); handle_nonprintable lexbuf}
| eof           {emit_error lexbuf (SyntaxError ("Unclosed multiline ignore sequence"))}

{
open Lexing
open Tokens
exception SyntaxError of string
exception Eof

let (spos, epos) = (lexeme_start, lexeme_end)
let _DEBUG_NONPRINTABLE s = Format.printf "%s%!" s
}

let whitespace   = [' ' '\t']
let newline      = '\r' | '\n' | "\r\n" | '\x0C'
let di           = ['0'-'9']
let integers     = ['0'-'9']+
let nonprintable = newline | whitespace
let letter       = ['a'-'z' 'A'-'Z']
let id           = letter+ (di | letter | '_')*

rule read = parse
| whitespace+   {_DEBUG_NONPRINTABLE (lexeme lexbuf); read lexbuf}
| newline       {_DEBUG_NONPRINTABLE (lexeme lexbuf); read lexbuf}
| "while"       {_WHILE              (spos lexbuf, epos lexbuf)}
| "for"         {_FOR                (spos lexbuf, epos lexbuf)}
| "to"          {_TO                 (spos lexbuf, epos lexbuf)}
| "break"       {_BREAK              (spos lexbuf, epos lexbuf)}
| "let"         {_LET                (spos lexbuf, epos lexbuf)}
| "in"          {_IN                 (spos lexbuf, epos lexbuf)}
| "end"         {_END                (spos lexbuf, epos lexbuf)}
| "function"    {_FUNCTION           (spos lexbuf, epos lexbuf)}
| "var"         {_VAR                (spos lexbuf, epos lexbuf)}
| "type"        {_TYPE               (spos lexbuf, epos lexbuf)}
| "array"       {_ARRAY              (spos lexbuf, epos lexbuf)}
| "if"          {_IF                 (spos lexbuf, epos lexbuf)}
| "then"        {_THEN               (spos lexbuf, epos lexbuf)}
| "else"        {_ELSE               (spos lexbuf, epos lexbuf)}
| "do"          {_DO                 (spos lexbuf, epos lexbuf)}
| "of"          {_OF                 (spos lexbuf, epos lexbuf)}
| "nil"         {_NIL                (spos lexbuf, epos lexbuf)}
| ","           {_COMMA              (spos lexbuf, epos lexbuf)}
| ":"           {_COLON              (spos lexbuf, epos lexbuf)}
| ";"           {_SEMICOLON          (spos lexbuf, epos lexbuf)}
| "("           {_LPAREN             (spos lexbuf, epos lexbuf)}
| ")"           {_RPAREN             (spos lexbuf, epos lexbuf)}
| "["           {_LBRACE             (spos lexbuf, epos lexbuf)}
| "]"           {_RBRACE             (spos lexbuf, epos lexbuf)}
| "{"           {_LBRACK             (spos lexbuf, epos lexbuf)}
| "}"           {_RBRACK             (spos lexbuf, epos lexbuf)}
| "."           {_DOT                (spos lexbuf, epos lexbuf)}
| "+"           {_PLUS               (spos lexbuf, epos lexbuf)}
| "-"           {_MINUS              (spos lexbuf, epos lexbuf)}
| "*"           {_TIMES              (spos lexbuf, epos lexbuf)}
| "/"           {_DIVIDE             (spos lexbuf, epos lexbuf)}
| "="           {_EQ                 (spos lexbuf, epos lexbuf)}
| "<>"          {_NEQ                (spos lexbuf, epos lexbuf)}
| "<"           {_LT                 (spos lexbuf, epos lexbuf)}
| "<="          {_LE                 (spos lexbuf, epos lexbuf)}
| ">"           {_GT                 (spos lexbuf, epos lexbuf)}
| ">="          {_GE                 (spos lexbuf, epos lexbuf)}
| "&"           {_AND                (spos lexbuf, epos lexbuf)}
| "|"           {_OR                 (spos lexbuf, epos lexbuf)}
| ":="          {_ASSIGN             (spos lexbuf, epos lexbuf)}
| '='           {_EQ                 (spos lexbuf, epos lexbuf)}
| '"'           {read_str (Buffer.create 80) lexbuf}
| id            {_ID                 (lexeme lexbuf,spos lexbuf, epos lexbuf)}
| integers      {_INT                (int_of_string (lexeme lexbuf), spos lexbuf, epos lexbuf)}
| "/*"          {comment 1 lexbuf}
| _             {raise (SyntaxError ("Undefined: " ^ lexeme lexbuf))}
| eof           {raise Eof}


and comment depth = parse
| "*/"          {if depth = 1 then read lexbuf else comment (depth - 1) lexbuf}
| "/*"          {comment (depth + 1) lexbuf}
| [^ '*' '/']+  {comment depth lexbuf}
| '*'           {comment depth lexbuf}
| '/'           {comment depth lexbuf}
| _             {comment depth lexbuf}
| eof           {raise (SyntaxError "Comments not closed")}

and read_str strbuf = parse
| '"'           {_STRING             (Buffer.contents strbuf,spos lexbuf, epos lexbuf)}
| '\\' 'n'      {Buffer.add_char strbuf '\n'; read_str strbuf lexbuf}
| '\\' 't'      {Buffer.add_char strbuf '\t'; read_str strbuf lexbuf}

| '\\' (di as d1) (di as d2) (di as d3)
                {let code = (Char.code d1 - Char.code '0') * 100 + 
                            (Char.code d2 - Char.code '0') * 10  +
                            (Char.code d3 - Char.code '0')       in
                Buffer.add_char strbuf (Char.chr code); 
                read_str strbuf lexbuf}

| '\\' '"'      {Buffer.add_char strbuf '\"'; read_str strbuf lexbuf}
| '\\' '\\'     {Buffer.add_char strbuf '\\'; read_str strbuf lexbuf}

| '\\' nonprintable+ '\\'
                {read_str strbuf lexbuf}

| [^ '\\' '"']+ {Buffer.add_string strbuf (lexeme lexbuf); read_str strbuf lexbuf}
| _             {raise (SyntaxError ("Undefined escape sequence: " ^ lexeme lexbuf))}
| eof           {raise (SyntaxError ("Expecting closing quote"))}
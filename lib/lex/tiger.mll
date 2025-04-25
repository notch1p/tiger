{
open Lexing
open Syntax.Errormsg
open Syntax.Tiger

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
| "while"       {WHILE    }
| "for"         {FOR      }
| "to"          {TO       }
| "break"       {BREAK    }
| "let"         {LET      }
| "in"          {IN       }
| "end"         {END      }
| "function"    {FUNCTION }
| "var"         {VAR      }
| "type"        {TYPE     }
| "array"       {ARRAY    }
| "if"          {IF       }
| "then"        {THEN     }
| "else"        {ELSE     }
| "do"          {DO       }
| "of"          {OF       }
| "nil"         {NIL      }
| ","           {COMMA    }
| ":"           {COLON    }
| ";"           {SEMICOLON}
| "("           {LPAREN   }
| ")"           {RPAREN   }
| "["           {LBRACE   }
| "]"           {RBRACE   }
| "{"           {LBRACK   }
| "}"           {RBRACK   }
| "."           {DOT      }
| "+"           {PLUS     }
| "-"           {MINUS    }
| "*"           {TIMES    }
| "/"           {DIVIDE   }
| "="           {EQ       }
| "<>"          {NEQ      }
| "<"           {LT       }
| "<="          {LE       }
| ">"           {GT       }
| ">="          {GE       }
| "&"           {AND      }
| "|"           {OR       }
| ":="          {ASSIGN   }
| '='           {EQ       }
| '"'           {read_str (spos lexbuf) (Buffer.create 80) lexbuf}
| id            {ID (lexeme lexbuf)}
| integers      {INT(int_of_string @@ lexeme lexbuf)}
| "/*"          {comment 1 lexbuf; read lexbuf}
| _             {emit_error lexbuf (SyntaxError ("Illegal character")); exit 1}
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
| '"'           {STRING (Buffer.contents strbuf)}
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

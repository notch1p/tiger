%token          EOF
%token <string> ID
%token <int>    INT
%token <string> STRING
%token          COMMA        ","
%token          COLON        ":"
%token          SEMICOLON    ";"
%token          LPAREN       "("
%token          RPAREN       ")"
%token          LBRACK       "{"
%token          RBRACK       "}"
%token          LBRACE       "["
%token          RBRACE       "]"
%token          DOT          "."
%token          PLUS         "+"
%token          MINUS        "-"
%token          UMINUS
%token          TIMES        "*"
%token          DIVIDE       "/"
%token          EQ           "="
%token          NEQ          "<>"
%token          LT           "<"
%token          LE           "<="
%token          GT           ">"
%token          GE           ">="
%token          AND          "&"
%token          OR           "|"
%token          ASSIGN       ":="
%token          ARRAY        "array"
%token          IF           "if"
%token          THEN         "then"
%token          ELSE         "else"
%token          WHILE        "while"
%token          FOR          "for"
%token          TO           "to"
%token          DO           "do"
%token          LET          "let"
%token          IN           "in"
%token          END          "end"
%token          OF           "of"
%token          BREAK        "break"
%token          NIL          "nil"
%token          FUNCTION     "function"
%token          VAR          "var"
%token          TYPE         "type"

%nonassoc           ":="
%left               "|"
%left               "&"
%nonassoc           "=" "<>" ">" "<" ">=" "<="
%left               "+" "-"
%left               "*" "/"
%left               UMINUS

// %start          expr
%start <unit> prog

%%

prog: expr EOF                 {()}

// declarations
decs:
| (* empty *)               {}
| decs dec                  {}

%inline dec:
| tydec                     {}
| vardec                    {}
| fundec                    {}



// datatypes
%inline tid: ID             {}

tydec:
| TYPE tid "=" ty           {}

%inline ty:
| tid                       {}
| "{" tyfields "}"          {}
| "array" "of" tid          {}

tyfields:
|                           {}
| ID ":" tid                {}
| tyfields "," ID ":" tid   {}

// variables
vardec:
| "var" ID         ":=" expr {}
| "var" ID ":" tid ":=" expr {}

// functions
fundec:
| "function" ID "(" tyfields ")" "=" expr   {}
| "function" ID "(" tyfields ")" ":" tid "=" expr   {}

// expressions
expr:
| lvalue                                            {}
| NIL                                               {}
| "(" ")"                                           {}
| "(" exprs ")"                                     {}
| arith                                             {}
| STRING                                            {}
| funcall                                           {}
| compare                                           {}
| instOfRec                                         {}
| instOfArr                                         {}
| assignment                                        {}
| ite                                               {}
| while_loop                                        {}
| for_loop                                          {}
| BREAK                                             {}
| letexp                                            {}

letexp:
| "let" decs "in" exprs "end"                       {}

%inline while_loop: "while" expr "do" expr          {}

%inline for_loop: "for" ID ":=" expr "to" expr "do" expr {}

ite: (* favors shifting *)
| "if" expr "then" expr                             {}
| "if" expr "then" expr "else" expr                 {}

compare:
| expr cop expr                                     {}
%inline cop: "=" {} | "<>" {} | ">" {} | "<" {} | ">=" {} | "<=" {} | "&" {} | "|" {}

assignment:
| lvalue ":=" expr                                  {}

instOfRec:
| tid "{" tdefs "}"                                 {}
tdefs: 
|
| ID "=" expr                                       {} 
| tdefs "," ID "=" expr                             {}

funcall:
| fid "(" ")"                                       {}
| fid "(" exprc ")"                                 {}
%inline fid: ID                                     {}

arith:
| INT                                               {}
| expr "+" expr                                     {}
| expr "-" expr                                     {}
| expr "*" expr                                     {}
| expr "/" expr                                     {}
| "-" expr %prec UMINUS                             {}

exprs:
| expr                                              {}
| exprs ";" expr                                    {}

exprc:
| expr                                              {}
| exprc "," expr                                    {}

(*
lvalue' -> ID
        -> lvalue

lvalue -> eps
       -> . ID lvalue
       -> [ expr ] lvalue
*)

(* As soon as a ID is given it is then reduced to lvalue immediately *)
lvalue: ID lvalue_ {}
(* if follwed by a `LBRACE expr RBRACE`, 
   will safely reduce to lvalue_, 
   then reduce to lvalue 
*)
lvalue_:
| {}
| "." ID lvalue_ {}
| "[" expr "]" lvalue_ {}




(* The conflict one. `ID [ expr ] and ID [ expr ] of expr`

prefer reduce over shift
lvalue:
| ID                                                {}
| lvalue "." ID                                     {}
| lvalue "[" expr "]"                               {}
*)

instOfArr:
| tid "[" expr "]" "of" expr                        {}


(* Analyseur lexical pour mini-Turtle *)

{
  open Lexing
  open Parser

  (* exception à lever pour signaler une erreur lexicale *)
  exception Lexing_error of string

  (* note : penser à appeler la fonction Lexing.new_line
     à chaque retour chariot (caractère '\n') *)


let keywords_table = Hashtbl.create 17
let () = List.iter
(fun (s,t) -> Hashtbl.add keywords_table s t)
[
"else", ELSE;
"false", FALSE;
"fn", FN;
"if", IF;
"let", LET;
"mut", MUT;
"return", RETURN;
"struct", STRUCT;
"true", TRUE;
"while", WHILE;
"len", LEN;
"vec", VEC;
"print", PRINT
]

let symbols_table = Hashtbl.create 17
let () = List.iter
(fun (s,t) -> Hashtbl.add symbols_table s t)
[
"=", EQUAL;
"||", LOGIC_OR;
"&&", LOGIC_AND;
"==", EQUALITY;
"!=", DISTINCT;
"<", SMALLER;
"<=", SMALLER_EQUAL;
">", GREATER;
">=", GREATER_EQUAL;
"+", PLUS;
"-", MINUS;
"*", PROD;
"/", DIV;
"%", MOD;
"!", NOT;
"&", AND;
"[", OPEN_BRACKET;
"]", CLOSE_BRACKET;
".", POINT;
",", COMMA;
"{", OPEN_BRACE;
"}", CLOSE_BRACE;
":", COLON;
"(", OPEN_PAR;
")", CLOSE_PAR;
";", SEMI_COLON
]


}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let newline = '\n'
let white = [' ' '\t']
let symbols = "="
| "||"
| "&&"
| "=="
| "!="
| "<"
| "<="
| ">"
| ">="
| "+"
| "-"
| "*"
| "/"
| "%"
| "!"
| "&"
| "["
| "]"
| "."
| ","
| "{"
| "}"
| ":"
| "("
| ")"
| ";"
let integer = digit+
let character = [^'\n' ]

rule token = parse
| "//"
{ comment_line lexbuf }

| "/*"
{ comment lexbuf; token lexbuf}

| '&' ' '* "mut"
{ AND_MUT }

| letter (letter | digit | '_')* as ident
{ try Hashtbl.find keywords_table ident with Not_found -> IDENT ident }

| white
{ token lexbuf }

| newline
{ Lexing.new_line lexbuf; token lexbuf }

| symbols as c
{ Hashtbl.find symbols_table c }

| integer as s
{ (* print_string s; print_string "\n"; print_int (int_of_string s);
print_string "\n"; *) CONSTINT (int_of_string s) }

| "\""
{ STRING (user_string lexbuf) } 

| _ as c
{ raise (Lexing_error ("Caractere non reconnu: " ^ (String.make 1 c))) }

| eof
{ EOF }

and comment = parse
| "*/"
{ () }
| "/*"
{ comment lexbuf; comment lexbuf }
| "\n"
{ Lexing.new_line lexbuf; comment lexbuf}
| _
{ comment lexbuf }
| eof
{ raise (Lexing_error "Commentaire non terminé") }

and comment_line = parse
| "\n"
{ Lexing.new_line lexbuf; token lexbuf}
| eof
{ EOF }
| _
{ comment_line lexbuf }

and user_string = parse
| "\\\"" 
{ "\"" ^ (user_string lexbuf) }
| "\\\\"
{ "\\" ^ (user_string lexbuf) }
| "\\n" 
{ "\n" ^ (user_string lexbuf) }
| "\""
{ "" }
| "\n"
{ Lexing.new_line lexbuf; "\n" ^ user_string lexbuf }
| character as c
{ (String.make 1 c) ^ (user_string lexbuf) }
| eof
{ raise (Lexing_error "Chaine non terminée") }


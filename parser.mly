
/* Analyseur syntaxique pour  */

%{
  open Ast
%}

/* Déclaration des tokens */

%token EOF

%token ELSE
%token FALSE
%token FN
%token IF
%token LET
%token MUT
%token RETURN
%token STRUCT
%token TRUE
%token WHILE

%token LEN
%token VEC
%token PRINT

%token EQUAL /* affectation */
%token LOGIC_OR
%token LOGIC_AND
%token EQUALITY /* test d'egalité */
%token DISTINCT
%token SMALLER
%token SMALLER_EQUAL
%token GREATER
%token GREATER_EQUAL
%token PLUS
%token MINUS
%token PROD
%token DIV
%token MOD
%token NOT
%token AND
%token AND_MUT
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token POINT

%token <string> IDENT
%token <string> STRING
%token <int> CONSTINT

%token COMMA
%token OPEN_BRACE /* { */
%token CLOSE_BRACE /* } */
%token COLON
%token OPEN_PAR
%token CLOSE_PAR
%token SEMI_COLON

/* Priorités et associativités des tokens */


%right EQUAL
%left LOGIC_OR
%left LOGIC_AND
%nonassoc EQUALITY DISTINCT SMALLER SMALLER_EQUAL GREATER GREATER_EQUAL
%left PLUS MINUS
%left PROD DIV MOD
%nonassoc uprod uminus NOT AND AND_MUT
%nonassoc OPEN_BRACKET 
%nonassoc POINT



/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> prog

%%


/* Règles de grammaire */


prog:
| d = list(decl); EOF
{ {decls = d} }
;

decl:
| d = decl_fun
{ Ddeclfun d }
| d = decl_struct
{ Ddeclstruct d }
;

decl_struct:
| STRUCT; s = ident_global; l = infos_vecteur
{ (s, l, ($startpos, $endpos)) }
;

decl_fun:
| FN; s = ident_global;
    OPEN_PAR; l = separated_list(COMMA, argument); CLOSE_PAR;
    c = option_typ; b = bloc
{ (s, l, c, b, ($startpos, $endpos)) }
;

type_d:
| s = ident_global
{ Typeident s }
| s = ident_global; SMALLER; t = type_d ; GREATER
{ Template (s,t) }
| AND; t = type_d
{ Addrtype t }
| AND_MUT; t = type_d
{ Addrmuttype t }
;

option_typ:
| ARROW; t = type_d
{ Some t }
| { None }
;

option_mut:
| MUT { true }
| { false }
;

argument:
| b = option_mut; s = ident_global; COLON; t = type_d
{ (b, s, t, ($startpos, $endpos)) }
;

bloc:
| OPEN_BRACE; c = construction_bloc; CLOSE_BRACE
{ c }
;

construction_bloc:
| i = pretty_instr; c = construction_bloc 
{ let (l,e) = c in (i::l,e) }
| e = option(prettyExpr)
{ ([], e) }
;



pretty_instr:
| i = instr
{ { s = i; spos = ($startpos, $endpos) } }
;

instr:
| SEMI_COLON
{ Semptystmt }
| e = prettyExpr; SEMI_COLON
{ Sexpr e }
| LET; b = option_mut; s = ident_global; EQUAL; 
    e = prettyExpr; SEMI_COLON
{ Slet (s, b, e) }
| LET; b = option_mut; s = ident_global; EQUAL;  
    e = ident_global; l = infos_mut_vecteur; SEMI_COLON
{ Sletv (s, b, e, l) }
| WHILE; e = prettyExpr; b = bloc
{ Swhile (e, b) }
| RETURN; e = option(prettyExpr); SEMI_COLON
{ Sret e }
| i = blocif
{ Sif i }
;

blocif:
| IF; e = prettyExpr; b = bloc; l = option(blocelse)
{ (e, b, l) }
;

blocelse:
| ELSE; b = bloc
{ Selse b }
| ELSE; i = blocif
{ Selif i }
;



prettyExpr:
| e = expr
{ { e=e; epos = ($startpos, $endpos)} }
;

expr:
| i = CONSTINT
{ Econst i }
| TRUE
{ Ebool true }
| FALSE
{ Ebool false }
| s = ident_global
{ Evar s }
| e1 = prettyExpr; b = binop; e2 = prettyExpr
{ Ebinop (b,e1,e2) }

| NOT; e = prettyExpr
{ Eunop (Not,e) }
| PROD; e = prettyExpr;  %prec uprod
{ Eunop (Unaddr,e) }
| MINUS; e = prettyExpr; %prec uminus
{ Eunop (Unmin,e) }
| AND; e = prettyExpr
{ Eunop (And,e) }
| AND_MUT; e = prettyExpr
{ Eunop (Andmut,e) }

| e = prettyExpr; POINT; LEN ; OPEN_PAR; CLOSE_PAR
{ Elength e }
| e = prettyExpr; POINT; s = ident_global;
{ Eattribut (e,s) }
| e1 = prettyExpr; OPEN_BRACKET; e2 = prettyExpr; CLOSE_BRACKET
{ Eacces (e1, e2) }
| s = ident_global; OPEN_PAR; l = separated_list(COMMA, prettyExpr) ; CLOSE_PAR
{ Eapp (s, l) }
| VEC; NOT; OPEN_BRACKET; l = separated_list(COMMA, prettyExpr) ; CLOSE_BRACKET
{ Evec l }
| PRINT; NOT; OPEN_PAR; s = STRING; CLOSE_PAR;
{ Eprint s }
| b = bloc
{ Ebloc b }
| OPEN_PAR; e = expr; CLOSE_PAR
{ e }
;

%inline binop:
| EQUAL
{ Equ }
| LOGIC_OR
{ Lor }
| LOGIC_AND
{ Land }
| EQUALITY
{ Equality }
| DISTINCT
{ Dist }
| SMALLER
{ Small }
| SMALLER_EQUAL
{ Smalleq }
| GREATER
{ Great }
| GREATER_EQUAL
{ Greateq }
| PLUS
{ Add }
| MINUS
{ Sub }
| PROD
{ Mul }
| DIV
{ Div }
| MOD
{ Mod }
;







/* Vocabulaire */

ARROW:
| MINUS; GREATER; { () }
;

info_decl_struct:
| s = ident_global; COLON; t = type_d
{ (s,t) }
;

infos_vecteur:
| OPEN_BRACE; l = separated_list(COMMA, info_decl_struct); CLOSE_BRACE
{ l }
;


info_mut_decl_struct:
| s = ident_global; COLON; e = prettyExpr
{ (s,e) }
;

infos_mut_vecteur:
| OPEN_BRACE; l = separated_list(COMMA, info_mut_decl_struct); CLOSE_BRACE
{ l }
;


ident_global:
| s = IDENT
{ s }
| PRINT
{ "print" }
| VEC
{ "vec" }
| LEN
{ "len" }
;



















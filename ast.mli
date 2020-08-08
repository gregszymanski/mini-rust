
(* expressions entières *)

type pos = Lexing.position * Lexing.position

type ident = string

type binop =
| Add | Sub | Mul | Div | Mod
| Equality | Dist | Small | Smalleq | Great | Greateq
| Land | Lor | Equ

type unop =
| Unmin | Not | Unaddr | And | Andmut



type expr = {
    e : local_expr;
    epos : pos;
}
and local_expr =
| Econst of int
| Ebool of bool
| Evar of string
| Ebinop of binop * expr * expr
| Eunop of unop * expr
| Eattribut of expr * ident
| Elength of expr
| Eacces of expr * expr
| Eapp of ident * expr list
| Evec of expr list
| Eprint of string
| Ebloc of bloc



(* Types *)

and user_type =
| Typeident of ident
| Template of ident * user_type
| Addrtype of user_type
| Addrmuttype of user_type


(* instructions, blocs et structures conditionnelles *)

and stmt = {
    s : local_stmt;
    spos : pos;
}
and local_stmt =
| Semptystmt
| Sexpr of expr
| Slet of ident * bool * expr
| Sletv of ident * bool * ident * (ident * expr) list
| Swhile of expr * bloc
| Sret of expr option
| Sif of sif

and sif = expr * bloc * selse option

and selse =
| Selse of bloc
| Selif of sif

and bloc = stmt list * expr option




(* Declarations *)

type args = bool * ident * user_type * pos

type decl_struct = ident * (ident * user_type) list * pos

type decl_fun = ident * args list * user_type option * bloc * pos

type decl =
| Ddeclfun of decl_fun
| Ddeclstruct of decl_struct


(* programme *)

type program = {
    decls : decl list;
}



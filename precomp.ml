open Ast
open Typing
open Lexing

exception Error of string

module Smap = Map.Make(String)

type cexpr = {
  ce : clocal_expr;
  ctyp : typ;
}
and clocal_expr =
  | Cconst of int
  | Cbool of bool
  | Cvar of int
  | Cbinop of binop * cexpr * cexpr * int
  | Cassig of cexpr * cexpr
  | Cunop of unop * cexpr
  | Cattribut of cexpr * int
  | Clength of cexpr
  | Cacces of cexpr * cexpr
  | Capp of string * cexpr Smap.t
  | Cvec of int * int * (int * cexpr) list
  | Cprint of string
  | Cbloc of cbloc
  | Ceif of csif
  | Cemptyexpr

and cstmt =
  | Cemptystmt
  | Cexpr of cexpr
  | Clet of int * cexpr
  | Cletstr of int * ident * cexpr Smap.t
  | Cwhile of int * cexpr * cbloc
  | Cret of cexpr option
  | Cif of csif
  | Tfreevec of cexpr

and csif = {
  cityp : typ;
  cicond : cexpr;
  citodo : cbloc;
  cielse : cbloc;
  ccount : int;
}

and cbloc = {
  cbtodo : cstmt list;
  cexpr : cexpr;
  cret : bool;
  ctofree : cstmt list;
}

and cdecl_struct = {
  csname : ident;
  csparam : int Smap.t;
  csparamtyp : typ Smap.t;
  cssize : int;
}

and cdecl_fun = {
  cfname : ident;
  cfargs : int Smap.t;
  ctodo : cbloc;
  cframe : int;
  csizeargs : int;
  cnum : int;
}

type cprogram = {
  cfuncts : cdecl_fun Smap.t;
  cstructs : (string,cdecl_struct) Hashtbl.t;
  cstrings : string Smap.t;
}

type env = int Smap.t



let init_count () =
  let count = ref 0 in
  let c () =
    count := !count + 1;
    !count
  in c

let count_while = init_count ()
let count_if = init_count ()
let count_binop = init_count ()
let count_fun = init_count ()

let init_strings () =
  let lstr = ref Smap.empty in
  let count = ref 0 in
  let name () =
    count := !count + 1;
    "string_" ^ (string_of_int !count)
  in
  let a s =
    let n = name () in
    lstr := Smap.add n s !lstr;
    n
  in
  let g () =
    !lstr
  in
  a, g

let add_string, get_strings = init_strings ()

let max x y = if x > y then x else y
let min x y = if x < y then x else y

let empty_expr = {
  ce = Cemptyexpr;
  ctyp = Tunit;
}


let size_of_typ structs = function
  | Tunit -> 8
  | Ti32 -> 8
  | Tybool -> 8
  | Tstruct str ->
    let str = Hashtbl.find structs str in
    str.cssize
  | Tyvec _ -> 16
  | Tyfreevec -> 16
  | Tborrow _ -> 8
  | Tborrowmut _ -> 8

let get_name_struct t =
  match t with
  | Tstruct str -> str
  | _ -> assert false

let is_mul_typ = function
  | Tunit | Ti32 | Tybool  | Tborrow _ | Tborrowmut _ -> false
  | Tstruct _ | Tyvec _ | Tyfreevec -> true

let is_vec = function
  | Tunit | Ti32 | Tybool | Tborrow _ | Tborrowmut _ | Tstruct _ -> false
  | Tyvec s -> true
  | Tyfreevec -> true

let type_of_vec = function
  | Tunit | Ti32 | Tybool | Tborrow _ | Tborrowmut _ | Tstruct _ -> assert false
  | Tyvec s -> s
  | Tyfreevec -> Tunit

let vec_in_struct prg s =
  let str = Hashtbl.find prg.cstructs s in
  Smap.fold (fun key elem acc ->
      match elem with
      | Tyvec _ | Tyfreevec -> (Smap.find key str.csparam, elem) :: acc
      | _ -> acc
    ) str.csparamtyp []

let struct_in_struct prg s =
  let str = Hashtbl.find prg.cstructs s in
  Smap.fold (fun key elem acc ->
      match elem with
      | Tstruct s -> (Smap.find key str.csparam, Tstruct s) :: acc
      | _ -> acc
    ) str.csparamtyp []


let rec precomp_expr prg env next expr =
  let next, nexpr = precomp_local_expr prg env next expr.te in
  next, {
    ce = nexpr;
    ctyp = expr.ttyp;
  }

and precomp_local_expr prg env next = function
  | Tconst c -> next, Cconst c
  | Tbool b -> next, Cbool b
  | Tvar s -> next, Cvar (Smap.find s env)
  | Tbinop (Equ,e1,e2) ->
    let next1, e1 = precomp_expr prg env next e1 in
    let next2, e2 = precomp_expr prg env next e2 in
    max next1 next2, Cassig(e1, e2)
  | Tbinop (b,e1,e2) ->
    let next1, e1 = precomp_expr prg env next e1 in
    let next2, e2 = precomp_expr prg env next e2 in
    max next1 next2, Cbinop (b, e1, e2, count_binop ())
  | Tunop (u, e1) ->
    let next, e1 = precomp_expr prg env next e1 in
    next, Cunop (u, e1)
  | Tattribut (e, id) ->
    let str = Hashtbl.find prg.cstructs (get_name_struct e.ttyp) in
    let pos = Smap.find id str.csparam in
    let next, e = precomp_expr prg env next e in
    next, Cattribut (e, pos)
  | Tlength e ->
    let next, e = precomp_expr prg env next e in
    next, Clength e
  | Tacces (e1, e2) ->
    let next1, e1 = precomp_expr prg env next e1 in
    let next2, e2 = precomp_expr prg env next e2 in
    max next1 next2, Cacces (e1, e2)
  | Tapp (str, le) ->
    let next = ref next in
    let le = Smap.map
      (fun a -> let n,b = precomp_expr prg env !next a in
		next := max !next n;
		b
      ) le in
    !next, Capp (str, le)
  | Tvec le ->
     let next = ref next in
     let se = List.length le in
     let su =
       if se = 0
       then 0
       else size_of_typ prg.cstructs ((List.hd le).ttyp) in
     let le = List.mapi
                (fun i a -> let n,b = precomp_expr prg env !next a in
		          next := max !next n;
		          (i,b)
                ) le in
     !next, Cvec (se, su, le)
  | Tprint t -> next, Cprint (add_string t)
  | Tbloc b ->
    let next, b = precomp_bloc prg env next b in
    next, Cbloc b
  | Teif eif ->
    let next, eif = precomp_if prg env next eif in 
    next, Ceif eif

and precomp_stmt prg env next stmt =
  precomp_local_stmt prg env next stmt

and precomp_local_stmt prg env next = function
  | Temptystmt -> next, env, Cemptystmt
  | Texpr expr ->
    let next, expr = precomp_expr prg env next expr in
    next, env, Cexpr expr
  | Tlet (str,b,expr) ->
    let size = size_of_typ prg.cstructs expr.ttyp in
    let nextVar = size + next in
    let next, expr = precomp_expr prg env next expr in
    let env = Smap.add str (-nextVar) env in
    max next nextVar, env, Clet(-nextVar, expr)
  | Tletv (nameVar, b, nameStr, le) ->
     let str = Hashtbl.find prg.cstructs nameStr in
     let size = str.cssize in
     let nextVar = size + next in
     let next = ref nextVar in
     let le =
       Smap.mapi (fun name expr ->
         let n, expr = precomp_expr prg env !next expr in
         next := max !next n;
         expr
       ) le
     in
     let env = Smap.add nameVar (-nextVar) env in
     !next, env, Cletstr(-nextVar, nameStr, le)
  | Twhile (expr, bloc) ->
    let next1, expr = precomp_expr prg env next expr in
    let next2, bloc = precomp_bloc prg env next bloc in
    max next1 next2, env, Cwhile(count_while (), expr, bloc)
  | Tret (Some expr) ->
    let next, expr = precomp_expr prg env next expr in
    next, env, Cret (Some expr)
  | Tret None ->
    next, env, Cret None
  | Tif eif ->
    let next, eif = precomp_if prg env next eif in
    next, env, Cif eif
  | Tfree expr ->
     let next, expr = precomp_expr prg env next expr in
     precomp_free prg env next expr

and precomp_free prg env next expr =
  match expr.ctyp with
  | Tyvec _ | Tyfreevec ->
     next, env, Tfreevec expr
  | Tstruct s ->
     let l = vec_in_struct prg s in
     let ls = struct_in_struct prg s in
     let b = {
         cbtodo = List.fold_right (fun e acc ->
                      let expr = {
                          ce = Cattribut(expr, fst e);
                          ctyp = snd e;
                        } in
                      let _,_,stmt = precomp_free prg env next expr in
                      stmt :: acc) ls [];
         cexpr = {
             ce = Cemptyexpr;
             ctyp = Tunit;
                 };
         cret = false;
         ctofree = List.fold_right (fun e acc ->
                       let expr = {
                           ce = Cattribut (expr, fst e);
                           ctyp = snd e
                         } in
                       Tfreevec expr :: acc
                     ) l [];
       } in
     next, env, Cexpr {
         ce = Cbloc b;
         ctyp = Tunit;
       }
  | _ -> next, env, Cemptystmt

and precomp_if prg env next sif =
  let next1, expr = precomp_expr prg env next sif.icond in
  let next2, bloc = precomp_bloc prg env next sif.itodo in
  let next = max next1 next2 in
  let next3, elbl = match sif.ielse with
    | Some ie -> precomp_bloc prg env next ie
    | None -> next, {
          cbtodo = [];
          cexpr = empty_expr;
          cret = false;
          ctofree = [];
        }
  in
  max next next3, {
    cityp = sif.ityp;
    cicond = expr;
    citodo = bloc;
    cielse = elbl;
    ccount = count_if ();
  }

and precomp_bloc prg env next bloc =
  match bloc.btodo with
    | first :: follow ->
      let next, env, first = precomp_stmt prg env next first.ts in
      let next, follow =
        precomp_bloc prg env next
	  {
            btodo = follow;
            bexpr = bloc.bexpr;
            bret = bloc.bret;
            btyp = bloc.btyp;
            btofree = bloc.btofree;
          }
      in next, {
	cbtodo = first :: follow.cbtodo;
	cexpr = follow.cexpr;
	cret = follow.cret;
        ctofree = follow.ctofree;
      }
    | [] ->
      let next, expr = precomp_some_expr prg env next bloc.bexpr in
      next, {
	cbtodo = [];
	cexpr = expr;
	cret = bloc.bret;
        ctofree = free_list_expr prg env next bloc.btofree;
      }

and free_list_expr prg env next tofree =
  List.fold_right
    (
      fun expr acc ->
      let _, expr = precomp_expr prg env next expr in
      let _, _, s =  precomp_free prg env next expr in
      s :: acc
    ) tofree []

and precomp_some_expr prg env next = function
  | None -> next, empty_expr
  | Some expr ->
    let next, expr = precomp_expr prg env next expr in
    next, expr


let precomp_fun prg funct =
  let env, sizeargs = Smap.fold
    (fun key (_, elem) (acc, next) ->
      let nextAccess = next + 8 in
      let next = next + (size_of_typ prg.cstructs elem.atyp) in
      Smap.add key nextAccess acc, next
    )
    funct.args (Smap.empty, 8)
  in
  let next, bloc = precomp_bloc prg env 0 funct.todo in
  {
    cfname = funct.fname;
    cfargs = env;
    ctodo =  bloc;
    cframe = next;
    csizeargs = sizeargs;
    cnum = count_fun ();
  }

let rec get_internal_struct = function
  | Tunit | Ti32 | Tybool | Tyfreevec | Tyvec _ -> ""
  | Tstruct s -> s
  | Tborrow t | Tborrowmut t -> get_internal_struct t

let rec add_struct lstr structs key str =
  if not (Hashtbl.mem structs str.sname) then
    let size = ref 0 in
    let param = Smap.mapi (
      fun key elem ->
	let internalStruct = get_internal_struct elem in
	if not (internalStruct = "") then
	  add_struct lstr structs internalStruct (Smap.find internalStruct lstr);
        let s = size_of_typ structs elem in
        size := !size + s; !size - s
    ) str.param in
    Hashtbl.add structs key {
      csname = key;
      csparam = param;
      cssize = !size;
      csparamtyp = str.param;
    }
  else ()


let precomp_prg prg =
  let (structs : (string, cdecl_struct) Hashtbl.t) = Hashtbl.create 17 in
  let () = Smap.iter (add_struct prg.structs structs) prg.structs in
  Smap.fold
    ( fun key elem acc ->
      let r = precomp_fun acc elem in
      let r = (* if key = "main" then change_end_main r else *) r in
      let r = Smap.add key r acc.cfuncts in
      {
      cfuncts = r;
      cstructs = structs;
      cstrings = get_strings ();
    } )
    prg.functs {
      cfuncts = Smap.empty;
      cstructs = structs;
      cstrings = get_strings ();
    }





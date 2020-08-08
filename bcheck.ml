
open Ast
open Lexing
open Typing


exception Error of string * Lexing.position * Lexing.position
exception Undetermined_vec

module Smap = Map.Make(String)

type status =
  | Empty
  | Full
  | Borrowed of int * bool

type decor =
  | Dunit
  | Di32
  | Dbool
  | Dstruct
  | Dvec of decor
  | Dfreevec
  | Dborrow of int * bool * decor

type bvar = {
    bname : string;
    bdecor : decor;
    blife : int;
    bstatus : status;
    btyp : typ;
}

type benv = {
    bvars : bvar Smap.t;
    bborowed : string list Smap.t list;
}


let empty_benv = {
    bvars = Smap.empty;
    bborowed = [];
  }


let get_decor_var env x =
  (Smap.find x env.bvars).bdecor

let add_var env v = {
    bvars = Smap.add v.bname v env.bvars;
    bborowed = env.bborowed;
  }

let rec typ_to_decor life = function
  | Tunit -> Dunit
  | Ti32 -> Di32
  | Tybool -> Dbool
  | Tstruct _ -> Dstruct
  | Tyvec t -> Dvec (typ_to_decor life t)
  | Tyfreevec -> Dfreevec
  | Tborrow t -> Dborrow(life, false, typ_to_decor life t)
  | Tborrowmut t -> Dborrow(life, true, typ_to_decor life t)

let create_var level typ name = {
    bname = name;
    bdecor = typ_to_decor level typ;
    blife = level;
    bstatus = Full;
    btyp = typ;
  }

let get_var env x =
  (Smap.find x env.bvars)

let rec is_valid_lifetime now = function
  | Dunit | Di32 | Dbool | Dstruct | Dfreevec  -> true
  | Dvec t -> is_valid_lifetime now t
  | Dborrow (i,_,t) -> i <= now && is_valid_lifetime i t

let rec is_valid_decor wanted  = function
  | Dunit -> wanted == Dunit
  | Di32 -> wanted == Di32
  | Dbool -> wanted == Dbool
  | Dstruct -> wanted == Dstruct
  | Dvec t ->
     begin
       match wanted with
       | Dvec wt -> is_valid_decor wt t
       | _ -> false
     end
  | Dborrow (i, false, t) ->
     begin
       match wanted with
       | Dborrow(j, _, wt) -> j <= i && is_valid_decor wt t
       | _ -> false
     end
  | Dborrow (i, true, t) ->
     begin
       match wanted with
       | Dborrow(j, true, wt) -> j <= i && wt == t
       | _ -> false
     end
  | Dfreevec ->
     begin
       match wanted with
       | Dvec _ | Dfreevec -> true
       | _ -> false
     end

let unaddr_decor = function
  | Di32 | Dbool | Dunit | Dvec _ | Dfreevec | Dstruct -> assert false
  | Dborrow (_,_,t) -> t

let get_name_struct = function
  | Tunit -> assert false
  | Ti32 -> assert false
  | Tybool -> assert false
  | Tstruct s -> s
  | Tyvec t -> assert false
  | Tyfreevec -> assert false
  | Tborrow t -> assert false
  | Tborrowmut t -> assert false

let is_movable = function
  | Di32 | Dbool | Dunit | Dborrow (_,false,_) -> false
  | Dvec _ | Dfreevec | Dstruct | Dborrow (_,true,_) -> true

let get_vec = function
  | Di32 | Dbool | Dunit | Dborrow (_,_,_) | Dstruct-> assert false
  | Dvec t -> t
  | Dfreevec -> raise Undetermined_vec

let free_status_var env x =
  let v = get_var env x in
  add_var env {
      bname = x;
      bdecor = v.bdecor;
      blife = v.blife;
      bstatus = if is_movable v.bdecor then Empty else v.bstatus;
      btyp = v.btyp;
    }

let is_free_var env x=
  let v = get_var env x in
  v.bstatus == Empty

let push_status_var env x =
  let v = get_var env x in
  add_var env {
      bname = x;
      bdecor = v.bdecor;
      blife = v.blife;
      bstatus = Full;
      btyp = v.btyp;
    }

let rec decor_to_str = function
  | Dunit -> "()"
  | Di32 -> "i32"
  | Dbool -> "bool"
  | Dstruct -> "S"
  | Dvec d -> "vec<" ^ (decor_to_str d) ^ ">"
  | Dfreevec -> "vec<'a>"
  | Dborrow (i, b, t) ->
     "&"
     ^ (string_of_int i)
     ^ (if b then " mut " else " ")
     ^ decor_to_str t

let can_be_free = function
  | Dvec _ | Dfreevec | Dstruct -> true
  | _ -> false


let bcheck_error t1 t2 p =
  raise (Error ("mismactched types: expected "
                ^ decor_to_str t1 ^ ", found "
                ^ decor_to_str t2
              , fst p, snd p))



let tree_free pos l expr =
  if List.length l == 0
  then expr
  else
    let expr = {
        te = expr;
        tepos = pos;
        ttyp = Tunit;
        tlv = false;
      } in
    let b = {
        btodo = List.fold_right (
                    fun e acc ->
                    { ts = Tfree e; tspos = pos } :: acc
                  ) l [];
        bexpr = Some expr;
        bret = false;
        btyp = expr.ttyp;
        btofree = [];
      } in
    Tbloc b


let find_nofree_env env level =
  Smap.fold (fun key var (env, acc) ->
      if can_be_free var.bdecor
         && var.blife >= level
         && var.bstatus != Empty
      then
        let env = free_status_var env key in
        let acc = {
            te = Tvar key;
            tepos = empty_pos, empty_pos;
            ttyp = var.btyp;
            tlv = false;
          } :: acc in
        env, acc
      else
        env, acc
    ) env.bvars (env,[])


let rec bcheck_expr ?(left=false) prg env level expr =
  let env, dec, localexpr =
    bcheck_local_expr prg env level expr.tepos left expr.te in
  env, dec, {
      te = localexpr;
      tepos = expr.tepos;
      ttyp = expr.ttyp;
      tlv = expr.tlv
    }

and bcheck_push_expr prg env expr =
  match expr.te with
  | Tconst _ -> env
  | Tbool _ -> env
  | Tvar x -> push_status_var env x
  | Tbinop _ -> env
  | Tunop (u,e) -> env
  | Tattribut (e, s) -> bcheck_push_expr prg env e
  | Tlength e -> env
  | Tacces (e1,e2) -> env
  | Tapp _ -> env
  | Tvec _ -> env
  | Tprint s -> env
  | Tbloc b -> env
  | Teif tsif -> env

and bcheck_free_expr prg env expr =
  match expr.te with
  | Tconst _ -> env
  | Tbool _ -> env
  | Tvar x ->
     if is_movable (typ_to_decor 0 expr.ttyp)
     then free_status_var env x
     else env
  | Tbinop _ -> env
  | Tunop (u,e) -> env
  | Tattribut (e, s) ->
     let str = Smap.find (get_name_struct e.ttyp) prg.structs in
     if is_movable (typ_to_decor 0 (Smap.find s str.param))
     then bcheck_free_expr prg env e
     else env
  | Tlength e -> env
  | Tacces (e1,e2) -> env
  | Tapp _ -> env
  | Tvec _ -> env
  | Tprint s -> env
  | Tbloc b -> env
  | Teif tsif -> env

and bcheck_notfree_expr prg env expr =
  match expr.te with
  | Tconst _ -> []
  | Tbool _ -> []
  | Tvar x ->
     if is_movable (typ_to_decor 0 expr.ttyp)
     then
       let v = get_var env x in
       if v.bstatus != Empty
       then [expr]
       else []
     else []
  | Tbinop _ -> []
  | Tunop (u,e) -> []
  | Tattribut (e, s) ->
     let str = Smap.find (get_name_struct e.ttyp) prg.structs in
     if is_movable (typ_to_decor 0 (Smap.find s str.param))
     then [expr]
     else []
  | Tlength e -> []
  | Tacces (e1,e2) -> []
  | Tapp _ -> []
  | Tvec _ -> []
  | Tprint s -> []
  | Tbloc b -> []
  | Teif tsif -> []


and bcheck_local_expr prg env level pos left = function
  | Tconst i -> env, Di32, Tconst i
  | Tbool b -> env, Dbool, Tbool b
  | Tvar x ->
     if not left && is_free_var env x
     then raise (Error ("use of moved value: `" ^ x ^ "`", fst pos, snd pos))
     else env, get_decor_var env x, Tvar x
  | Tbinop (Equ, e1, e2) ->
     let env, t2, e2 = bcheck_expr prg env level e2 in
     let env = bcheck_free_expr prg env e2 in
     let env, t1, e1 = bcheck_expr ~left:true prg env level e1 in
     let l = bcheck_notfree_expr prg env e1 in
     let env = bcheck_push_expr prg env e1 in
     if is_valid_decor t1 t2
     then
       let op = tree_free pos l (Tbinop(Equ, e1, e2)) in
       env, Dunit, op
     else  bcheck_error t1 t2 pos
  | Tbinop (b,e1,e2) ->
     let env, t1, e1 = bcheck_expr prg env level e1 in
     let env, t2, e2 = bcheck_expr prg env level e2 in
     let t =
       begin
         match b with
         | Add | Sub | Mul | Div | Mod -> Di32
         | Equality | Dist | Small | Smalleq -> Dbool
         | Great | Greateq | Land | Lor -> Dbool
         | Equ -> assert false
       end in
     env, t, Tbinop(b,e1,e2)
  | Tunop (u, e) ->
     let env, t, e = bcheck_expr prg env level e in
     let t =
       begin
         match u with
         | Unmin -> Di32
         | Not -> Dbool
         | Unaddr -> unaddr_decor t
         | And -> Dborrow (level, false, t)
         | Andmut -> Dborrow (level, true, t)
       end in
     env, t, Tunop(u,e)
  | Tattribut (e, s) ->
     let env, t, e = bcheck_expr ~left:left prg env level e in
     let str = get_name_struct e.ttyp in
     let t = typ_to_decor level
               (Smap.find s (Smap.find str prg.structs).param) in
     env, t, Tattribut(e,s)
  | Tlength e ->
     let env, t, e = bcheck_expr prg env level e in
     env, Di32, Tlength e
  | Tacces (e1, e2) ->
     let env, t1, e1 = bcheck_expr ~left:left prg env level e1 in
     let env, t2, e2 = bcheck_expr prg env level e2 in
     env, get_vec t1, Tacces(e1, e2)
  | Tapp (s, le) ->
     let env = Smap.fold (fun key e env -> bcheck_app prg env level e) le env in
     env, typ_to_decor level (Smap.find s prg.functs).rtyp, Tapp(s, le)
  | Tvec le ->
     bcheck_vec prg env level le
  | Tprint s ->
     env, Dunit, Tprint s
  | Tbloc b ->
     let env, t, b = bcheck_bloc prg env (level+1) b in
     env, t, Tbloc b
  | Teif sif ->
     let env, t, sif = bcheck_if prg env level sif in
     env, t, Teif sif

and bcheck_app prg env level expr =
  let env = bcheck_free_expr prg env expr in
  env

and bcheck_vec prg env level le =
  if (List.length le) == 0 then
    env, typ_to_decor level Tyfreevec,
    Tvec le
  else
    let decor = typ_to_decor level (List.hd le).ttyp in
    let decor, env =
      List.fold_right(
          fun expr (decor, env) ->
          let env, currentDecor, expr = bcheck_expr prg env level expr in
          let decor = if is_valid_decor decor currentDecor
                      then decor
                      else
                        begin
                          if is_valid_decor currentDecor decor
                          then currentDecor
                          else bcheck_error decor currentDecor expr.tepos
                        end
          in
          let env = bcheck_free_expr prg env expr in
          decor, env) le (decor, env)
    in
    env, Dvec decor, Tvec le

and bcheck_stmt prg env level stmt =
  let env, ts = bcheck_local_stmt prg env level stmt.ts in
  env, { ts=ts; tspos=stmt.tspos}

and bcheck_local_stmt prg env level = function
  | Temptystmt -> env, Temptystmt
  | Texpr e ->
     let env,t, e = bcheck_expr prg env level e in
     env, Texpr e
  | Tlet (s,b,e) ->
     let env, t, e = bcheck_expr prg env level e in
     let env = bcheck_free_expr prg env e in
     add_var env (create_var level e.ttyp s), Tlet (s,b,e)
  | Tletv (nameVar, b, nameStr, le) ->
     let env = Smap.fold (
                   fun key expr env ->
                   bcheck_free_expr prg env expr
                 ) le env in
     add_var env (create_var level (Tstruct nameStr) nameVar),
     Tletv(nameVar, b, nameStr, le)
  | Twhile (e,b) ->
     let env, t, e = bcheck_expr prg env level e in
     let env, t, b = bcheck_bloc prg env (level+1) b in
     env, Twhile(e,b)
  | Tret (Some expr) ->
     let env = bcheck_free_expr prg env expr in
     let _, l = find_nofree_env env level in
     let b = Tbloc {
             btodo = [];
             bexpr = Some expr;
             bret = false;
             btyp = expr.ttyp;
             btofree = l;
           } in
     let expr = {
         te = b;
         tepos = expr.tepos;
         ttyp = expr.ttyp;
         tlv = expr.tlv
       } in
     env, Tret(Some expr)
  | Tret None -> env, Tret(None)
  | Tif sif ->
     let env, t, sif = bcheck_if prg env level sif in
     env, Tif sif
  | Tfree v -> assert false

and bcheck_if prg env level sif = env, Dunit, sif

and bcheck_bloc prg env level bloc =
  match bloc.btodo with
    | first :: follow ->
       let env, first = bcheck_stmt prg env level first in
       let env, t, follow =
         bcheck_bloc prg env level
           {
             btodo = follow;
             bexpr = bloc.bexpr;
             bret = bloc.bret;
             btyp = bloc.btyp;
             btofree = [];
           }
      in env, t, {
	btodo = first :: follow.btodo;
	bexpr = follow.bexpr;
	bret = follow.bret;
        btyp = follow.btyp;
        btofree = follow.btofree;
      }
    | [] ->
       begin match bloc.bexpr with
       | Some expr ->
          let env, t, expr = bcheck_expr prg env level expr in
          let env = bcheck_free_expr prg env expr in
          let env, l = find_nofree_env env level in
          env, t, {
	      btodo = [];
	      bexpr = Some expr;
	      bret = bloc.bret;
              btyp = bloc.btyp;
              btofree = l;
            }
       | None ->
          let env, l = find_nofree_env env level in
          env, Dunit, {
	      btodo = [];
	      bexpr = None;
	      bret = bloc.bret;
              btyp = bloc.btyp;
              btofree = l;
            }
       end




and bcheck_fun prg _ funct =
  let env = empty_benv in
  let env = Smap.fold
              (fun key (i,a) env ->
                add_var env (create_var 0 a.atyp key)
              ) funct.args env in
  let env, t, bloc = bcheck_bloc prg env 1 funct.todo in
  {
    fname = funct.fname;
    args = funct.args;
    rtyp = funct.rtyp;
    todo = bloc;
    funloc = funct.funloc;
    tofree = [];
  }


let bcheck_prg prg = {
    functs = Smap.mapi (bcheck_fun prg ) prg.functs;
    structs = prg.structs;
  }


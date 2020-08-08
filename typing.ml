open Ast
open Lexing


exception Error of string * Lexing.position * Lexing.position

module Smap = Map.Make(String)


type typ =
  | Tunit
  | Ti32
  | Tybool
  | Tstruct of ident
  | Tyvec of typ
  | Tyfreevec
  | Tborrow of typ
  | Tborrowmut of typ




(* expressions entieres *)

(* On reprend pos, Unop et Binop presents dans le fichier Ast.mli *)


type texpr = {
  te : tlocal_expr;
  tepos : pos;
  ttyp : typ;
  tlv: bool;
}
and tlocal_expr =
  | Tconst of int
  | Tbool of bool
  | Tvar of string
  | Tbinop of binop * texpr * texpr
  | Tunop of unop * texpr
  | Tattribut of texpr * ident
  | Tlength of texpr
  | Tacces of texpr * texpr
  | Tapp of ident * texpr Smap.t
  | Tvec of texpr list
  | Tprint of string
  | Tbloc of tbloc
  | Teif of tsif




(* instructions, blocs et structures conditionnelles *)

and tstmt = {
  ts : tlocal_stmt;
  tspos : pos;
}
and tlocal_stmt =
  | Temptystmt
  | Texpr of texpr
  | Tlet of ident * bool * texpr
  | Tletv of ident * bool * ident * texpr Smap.t
  | Twhile of texpr * tbloc
  | Tret of texpr option
  | Tif of tsif
  | Tfree of texpr

and tsif = {
  ityp : typ;
  icond : texpr;
  itodo : tbloc;
  ielse : tbloc option;
}

and tbloc = {
  btodo : tstmt list;
  bexpr : texpr option;
  bret : bool;
  btyp : typ;
  btofree : texpr list;
}





(* Declarations *)

type targs = {
  amut : bool;
  aname : ident;
  atyp : typ;
}

type tdecl_struct = {
  sname : ident;
  param : typ Smap.t;
  strloc : pos;
}

type tdecl_fun =  {
  fname : ident;
  args : (int * targs) Smap.t;
  rtyp : typ;
  todo : tbloc;
  funloc : pos;
  tofree : string list;
}

(* programme *)

type tprogram = {
  functs : tdecl_fun Smap.t;
  structs : tdecl_struct Smap.t;
}

type envtyp = {
  mut : bool;
  vtyp : typ;
}

type env = {
  vars : (bool * typ) Smap.t;
  (* mutable, type *)
}


let is_some = function
  | None -> false
  | Some _ -> true

let is_none = function 
  | None -> true
  | Some _ -> false

let get_some = function
  | None -> assert false
  | Some x -> x

let empty_pos =
  { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}

let empty_program =
  { functs = Smap.empty; structs = Smap.empty }

let empty_bloc = {
    btodo = [];
    bexpr = None;
    bret = false;
    btyp = Tunit;
    btofree = [];
  }

let empty_env =
  { vars = Smap.empty }

let add_funct fn prg = 
  { functs = Smap.add fn.fname fn (prg.functs); structs = prg.structs }

let add_struct str prg =
  { functs = prg.functs; structs = Smap.add str.sname str (prg.structs) }

let add_empty_struct name prg = 
  add_struct {
    sname = name; 
    param = Smap.empty; 
    strloc = (empty_pos, empty_pos) 
  } prg

let add_empty_funct name args typr prg =
  add_funct {
    fname = name; 
    args = args; 
    rtyp = typr; 
    todo = empty_bloc;
    funloc = (empty_pos, empty_pos);
    tofree = [];
  } prg

let add_variable name typ env =
  { vars = Smap.add name typ env.vars }

let get_arrival_function name prg =
  (Smap.find name prg.functs).rtyp

let is_let = function
  | Tlet (_,_,_) | Tletv (_,_,_,_) -> true
  | Temptystmt | Texpr _ | Twhile (_,_) | Tret _ | Tif _ | Tfree _ -> false

let rec userType_to_typ prg = function
  | Typeident s ->
     begin
       if Smap.mem s prg.structs
       then Tstruct s
       else
         match s with
         | "i32" -> Ti32
         | "bool" -> Tybool
         | _ -> Tstruct s
     end
  | Template (s,u) ->
     if s = "vec" || s = "Vec"
     then Tyvec (userType_to_typ prg u)
     else
       assert false
  | Addrtype u -> Tborrow (userType_to_typ prg u)
  | Addrmuttype u -> Tborrowmut (userType_to_typ prg u)

let userTypeOptions prg = function
  | None -> Tunit
  | Some typ -> userType_to_typ prg typ

let rec type_to_str = function
  | Tunit -> "()"
  | Ti32 -> "i32"
  | Tybool -> "bool"
  | Tstruct s -> "struct " ^ s
  | Tyvec u -> "Vec <" ^ (type_to_str u) ^ ">"
  | Tborrow u -> "& " ^ (type_to_str u)
  | Tborrowmut u -> "&m " ^ (type_to_str u) 
  | Tyfreevec -> "Vec <'a>"

let typing_error t1 t2 p =
  raise (Error ("mismactched types: expected "
                ^ type_to_str t1 ^ ", found "
                ^ type_to_str t2
              , fst p, snd p))

let styping_error s t2 p =
  raise (Error ("mismactched types: expected "
                ^ s ^ ", found "
                ^ type_to_str t2
              , fst p, snd p))

let rec is_free_borrow = function
  | Tunit | Ti32 | Tybool | Tstruct _ | Tyfreevec -> true
  | Tyvec u -> is_free_borrow u
  | Tborrow _ | Tborrowmut _ -> false

let rec is_well_formed prg = function
  | Tunit | Ti32 | Tybool | Tyfreevec -> true
  | Tstruct s -> Smap.mem s (prg.structs)
  | Tyvec u | Tborrow u | Tborrowmut u -> is_well_formed prg u

let rec is_without_recursion name = function
  | Tunit -> true
  | Ti32 -> (String.compare name "i32") != 0
  | Tybool -> (String.compare name "bool") != 0
  | Tyvec _ -> (String.compare name "Vec") != 0
  | Tyfreevec -> (String.compare name "Vec") != 0
  | Tborrow s | Tborrowmut s -> is_without_recursion name s
  | Tstruct s -> not (s = name)

let rec is_same wanted given =
  match given with
    | Tunit -> wanted == Tunit
    | Ti32 -> wanted == Ti32
    | Tybool -> wanted == Tybool
    | Tstruct sg ->
      begin
	match wanted with
	  | Tstruct sw -> sg = sw
	  | _ -> false
      end
    | Tyvec sg ->
      begin
	match wanted with
	  | Tyvec sw -> is_same sw sg
	  | _ -> false
      end
    | Tborrow sg ->
      begin
	match wanted with
	  | Tborrow sw -> is_same sw sg
	  | _ -> false
      end
    | Tborrowmut sg ->
      begin
	match wanted with
	  | Tborrowmut sw -> is_same sw sg
	  | _ -> false
      end
    | Tyfreevec ->
      begin
	match wanted with
	  | Tyvec _ | Tyfreevec -> true
	  | _ -> false
      end



(*

let rec is_usable wanted given =
  if is_same wanted given then 
    true
  else
    match given with
      | Tborrowmut s -> is_same wanted (Tborrow s)
      | Tyfreevec -> begin match wanted with | Tyvec _ -> true | _ -> false end
      | _ -> false

*)

let rec is_usable wanted given =
  match given with
    | Tunit -> wanted == Tunit
    | Ti32 -> wanted == Ti32
    | Tybool -> wanted == Tybool
    | Tstruct sg ->
      begin 
	match wanted with 
	  | Tstruct sw -> sg = sw
	  | _ -> false
      end
    | Tyvec sg ->
      begin 
	match wanted with 
	  | Tyvec sw -> is_usable sw sg
	  | _ -> false
      end
    | Tborrow sg ->
      begin 
	match wanted with
	  | Tborrow sw -> is_usable sw sg
	  | _ -> false
      end
    | Tborrowmut sg ->
      begin
	match wanted with
	  | Tborrowmut sw | Tborrow sw -> is_usable sw sg
	  | _ -> false
      end
    | Tyfreevec ->
      begin
	match wanted with
	  | Tyvec _ | Tyfreevec -> true
	  | _ -> false
      end

let min_usable t1 t2 =
  if is_usable t1 t2
  then t1
  else if is_usable t2 t1
  then t2
  else raise Not_found

let print_env env =
  print_string "Affichage de l'environnement: \n";
  Smap.mapi (fun key (b,t) ->
    let b = if b then " mut " else " " in
    let t = type_to_str t in
    print_string ("\t" ^ key ^ b ^ t ^ "\n")
  ) env.vars

let get_if_stmt stmt =
  match stmt.ts with
    | Tif isif -> Some isif
    | _ -> None

let check_type_struct prg str =
  let name, param, loc = str in
  let () =
    if Smap.mem name prg.structs
    then raise (Error ("a structure named `"
		       ^ name
		       ^ "` has already been defined in this file"
		       , fst loc, snd loc))
    else ()
  in
  let tmpprg = add_empty_struct name prg in
  let param = List.fold_left
    (fun acc (id, ut) ->
      let typ = userType_to_typ prg ut in
      if  not (Smap.mem id acc)
      then if (is_well_formed tmpprg typ)
	      && (is_free_borrow typ)
	      && (is_without_recursion name typ)
           then	Smap.add id typ acc
           else
	     raise (Error ("Unauthorized type of field `"
                           ^ id
                           ^ "`"
                         , fst loc, snd loc))
      else raise(Error ("field `"
                        ^ id
                        ^ "` is already declared", fst loc, snd loc))
    )
    Smap.empty
    param
  in
  add_struct {
    sname = name;
    param = param;
    strloc = loc
  } prg

let rec check_type_expr_vec prg env wanted l p =
  let finish, l = 
    List.fold_right (
        fun expr (finish,acc) ->
        let finishHere, expr = check_type_expr prg env wanted expr in
        (finish || finishHere), (expr :: acc)
      ) l (false,[])
  in
  if (List.length l) == 0 then
    false, { te = Tvec l; tepos = p; ttyp = Tyfreevec; tlv = false}
  else
    let tau = ref (List.hd l).ttyp in
    List.iter
      (fun x ->
	if not (is_usable !tau x.ttyp)
	then
	  if not (is_usable x.ttyp !tau)
	  then typing_error !tau x.ttyp p
	  else
	    tau := x.ttyp;
      )
      l;
    finish, { te = Tvec l; tepos = p; ttyp=Tyvec(!tau); tlv = false }

and check_type_expr_lenght prg env wanted expr p =
  let finish, expr = check_type_expr prg env wanted expr in
  match expr.ttyp with
  | Tyvec _ -> finish, {
	te = Tlength expr;
	tepos = p;
	ttyp=Ti32;
	tlv=false;
      }
  | Tborrowmut (Tyvec t) | Tborrow (Tyvec t) -> finish, {
	te = Tlength ({
	           te = Tunop(Unaddr, expr);
	           tepos = expr.tepos;
	           ttyp = Tyvec t;
	           tlv = true;
	       });
	tepos = p;
	ttyp=Ti32;
	tlv=false;
      }
  | Tyfreevec -> finish, {
	te = Tconst 0;
	tepos = expr.tepos;
	ttyp = Ti32;
	tlv = false
      }
  | _ ->
     styping_error "vec<'a>" expr.ttyp p

and check_type_expr_acces prg env wanted expr1 expr2 p =
  let finish1, expr1 = check_type_expr prg env wanted expr1 in
  let finish2, expr2 = check_type_expr prg env wanted expr2 in
  if expr2.ttyp == Ti32 then
    match expr1.ttyp with
    | Tyvec t -> (finish1 || finish2), {
	  te = Tacces (expr1, expr2);
	  tepos = p;
	  ttyp = t;
	  tlv = true;
	}
    | Tborrowmut (Tyvec t) | Tborrow (Tyvec t) -> (finish1 || finish2), {
	  te = Tacces (
		   {
		     te = Tunop(Unaddr, expr1);
		     tepos = expr1.tepos;
		     ttyp = Tyvec t;
		     tlv = true;
		   }
		 , expr2);
	  tepos = p;
	  ttyp = t;
	  tlv = true;
	}
    | Tyfreevec ->
       raise (Error ("Impossible d'acceder a un element d'un tableau vide"
		   , fst expr1.tepos, snd expr1.tepos))
    | _ -> typing_error Tyfreevec expr1.ttyp expr1.tepos
  else typing_error Ti32 expr2.ttyp expr2.tepos

and check_type_expr_call prg env wanted name lexpr p =
  try
    let _ =
      if Smap.mem name env.vars
      then raise (Error("expected function, found "
                          ^ type_to_str (snd (Smap.find name env.vars))
                      , fst p, snd p))
    in
    let fn = Smap.find name prg.functs in
    if List.length lexpr == Smap.cardinal fn.args then
      let finish, param =
        Smap.fold
	  (fun key (num, arg) (finish, acc) ->
	    let expr = List.nth lexpr (num-1) in
	    let finish2, expr = check_type_expr prg env wanted expr in
	    if is_usable arg.atyp expr.ttyp then
              let expr = {
                  te = expr.te;
                  tepos = expr.tepos;
                  ttyp = arg.atyp;
                  tlv = expr.tlv;
                } in
	      (finish2 || finish),
              (Smap.add key expr acc)
	    else typing_error arg.atyp expr.ttyp p
	  )
	  fn.args
	  (false, Smap.empty)
      in
      finish, {
          te = Tapp (name, param);
          tepos = p;
          ttyp = fn.rtyp;
          tlv = false
        }
    else raise (Error ("this function takes "
                       ^ (string_of_int (Smap.cardinal fn.args))
                       ^" parameters but "
                       ^  (string_of_int (List.length lexpr))
                       ^ " parameter was supplied"
                     , fst p, snd p))
  with Not_found ->
    raise (Error ("cannot find function `"
                  ^ name
                  ^ "` in this scope"
                , fst p, snd p))

and check_type_expr_attribut prg env wanted expr name p =
  let finish, expr = check_type_expr prg env wanted expr in
  match expr.ttyp with
  | Tborrow (Tstruct ident) | Tborrowmut (Tstruct ident) ->
     begin
       try
	 let str = Smap.find ident prg.structs in
	 begin
	   try
	     let t = Smap.find name str.param in
	     finish, {
		 te = Tattribut (
			  {
			    te = Tunop(Unaddr, expr);
			    tepos = expr.tepos;
			    ttyp = Tstruct ident;
			    tlv = true;
			  }
			, name);
		 tepos = p;
		 ttyp = t;
		 tlv = true;
	       }
	   with Not_found ->
	     raise (Error ("no field `"
                           ^ name
                           ^ "` on type `"
                           ^ ident ^"`"
                         , fst p, snd p))
	 end
       with Not_found ->
         raise (Error ("cannot find struct `"
                       ^ ident
                       ^ "` in this file"
                     , fst p, snd p))
     end
  | Tstruct ident ->
     begin
       try
	 let str = Smap.find ident prg.structs in
	 begin
	   try
	     let t = Smap.find name str.param in
	     finish, {
		 te = Tattribut (expr, name);
		 tepos = p;
		 ttyp = t;
		 tlv = true;
	       }
	   with Not_found ->
	     raise (Error ("no field `"
                           ^ name
                           ^ "` on type `"
                           ^ ident ^"`"
                         , fst p, snd p))
	 end
       with Not_found ->
         raise (Error ("cannot find struct `"
                       ^ ident
                       ^ "` in this file"
                     , fst p, snd p))
     end
  | _ -> styping_error "S" expr.ttyp p

and check_type_expr prg env wanted expr =
  let (e,p) = expr.e, expr.epos in
  match e with
    | Econst i -> false, { te = (Tconst i); tepos = p; ttyp = Ti32; tlv = false}
    | Ebool b -> false, { te = (Tbool b); tepos = p; ttyp = Tybool; tlv = false}
    | Eunop (u,e) -> check_type_unop prg env wanted u e p
    | Ebinop (b, e1, e2) -> check_type_binop prg env b wanted e1 e2 p
    | Ebloc bloc ->
      let bloc = check_type_bloc prg env wanted bloc in
      bloc.bret, { te = (Tbloc bloc); tepos = p; ttyp = bloc.btyp; tlv = false}
    | Evec l ->
       check_type_expr_vec prg env wanted l p
    | Eprint s -> false, { te = Tprint s; tepos = p; ttyp = Tunit; tlv = false}
    | Elength expr ->
       check_type_expr_lenght prg env wanted expr p
    | Eacces (expr1, expr2) ->
       check_type_expr_acces prg env wanted expr1 expr2 p
    | Evar x ->
       begin
	 try
	   let _, t = Smap.find x env.vars in
	   false, {
	       te = Tvar x;
	       tepos = p;
	       ttyp = t;
	       tlv = true;
	     }
	 with Not_found ->
	   raise (Error ("cannot find value `"
                         ^ x
                         ^ "` in this scope", fst p, snd p))
       end
    | Eapp (name, lexpr) ->
       check_type_expr_call prg env wanted name lexpr p
    | Eattribut (expr, name) ->
       check_type_expr_attribut prg env wanted expr name p


and check_type_mutability prg env wanted expr =
  match expr.te with
    | Tacces (expr, _) | Tattribut (expr, _) ->
       check_type_mutability prg env wanted expr
    | Tvar x ->
      begin
	try
	  fst (Smap.find x env.vars)
	with Not_found -> false
      end
    | Tunop (Unaddr, expr) ->
      begin
	match expr.ttyp with
	  | Tborrowmut _ -> true
	  | _ -> false;
      end
    | _ -> false

and check_type_unop prg env wanted unop expr pos =
  match unop with
    | Unmin | Not -> check_type_unop_car prg env wanted unop expr pos
    | And ->
      let finish, expr = check_type_expr prg env wanted expr in
      if expr.tlv then finish, {
	te = (Tunop (unop, expr));
	tepos = pos;
	ttyp = Tborrow (expr.ttyp);
	tlv = false
      } else raise (Error ("invalid left-hand side expression"
                         , fst pos, snd pos))
    | Andmut ->
      let finish, expr = check_type_expr prg env wanted expr in
      let b = check_type_mutability prg env wanted expr in
      if expr.tlv
      then
        if b
        then finish, {
	    te = Tunop (unop, expr);
	    tepos = pos;
	    ttyp = Tborrowmut (expr.ttyp);
	    tlv = false;
	  }
        else raise (Error ("left-hand side expression is immutable"
                         , fst pos, snd pos))
      else raise (Error ("invalid left-hand side expression", fst pos, snd pos))
    | Unaddr ->
      begin
	let finish, expr = check_type_expr prg env wanted expr in
        match expr.ttyp with
	  | Tborrow t | Tborrowmut t -> finish, {
	    te = Tunop(unop, expr);
	    tepos = pos;
	    ttyp = t;
	    tlv = true;
	  }
	  | _ -> styping_error "&'a" expr.ttyp pos
      end

and check_type_unop_car prg env wanted unop expr pos =
  let finish, expr = check_type_expr prg env wanted expr in
  let wanted =
    match unop with
      | Unmin -> Ti32
      | Not -> Tybool
      | _ -> assert false
  in
  if is_same wanted expr.ttyp
  then finish, {
      te = (Tunop (unop, expr));
      tepos = pos;
      ttyp = expr.ttyp;
      tlv = false
    }
  else typing_error wanted expr.ttyp pos

and check_type_binop prg env binop wanted expr expr2 pos =
  let finish1, expr1 = check_type_expr prg env wanted expr in
  let finish2, expr2 = check_type_expr prg env wanted expr2 in
  if binop == Equ then
    if (is_usable expr1.ttyp expr2.ttyp)
    then
      if  expr1.tlv && (check_type_mutability prg env wanted expr1)
      then (finish1 || finish2), {
          te = Tbinop (binop, expr1, expr2);
          tepos = pos;
          ttyp = Tunit;
          tlv = false;
        }
      else raise (Error ("expected mutable left value", fst pos, snd pos))
    else typing_error expr1.ttyp expr2.ttyp pos
  else
    let wantedType, obtained, finish =
      match binop with
      | Equality | Dist | Small
        | Smalleq | Great | Greateq -> Ti32, Tybool, finish1 || finish2
      | Add | Sub | Mul | Div | Mod -> Ti32, Ti32, finish1 || finish2
      | Land | Lor -> Tybool, Tybool, finish1 || (finish1 && finish2)
      | Equ -> assert false
    in
    if (is_same wantedType expr1.ttyp)
    then if (is_same wantedType expr2.ttyp)
         then
           finish, {
             te = (Tbinop (binop, expr1, expr2));
             tepos = pos;
             ttyp = obtained;
             tlv = false
           }
         else typing_error wantedType expr2.ttyp expr2.tepos
    else typing_error wantedType expr1.ttyp expr1.tepos


and check_type_stmt_letv prg env wanted stmt name mut str lie p =
  try
    (* Verification du nom de la structure *)
    let str = Smap.find str prg.structs in

    (* Verification de la definition de chaque champ de la structure *)
    let finish, params =
      List.fold_left
	(fun (f, acc) (s, expr) ->
	  if Smap.mem s acc then
	    raise (Error ("field `"
                          ^ s
                          ^ "` specified more than once"
                        , fst p, snd p))
	  else
	    try
	      let t = Smap.find s str.param in
	      let finish, expr = check_type_expr prg env wanted expr in
	      if (is_same t expr.ttyp) then
		(finish || f, Smap.add s expr acc)
	      else
		typing_error t expr.ttyp p
	    with Not_found ->
	      raise (Error ("struct `"
                            ^ str.sname
                            ^ "` has no field named `"
                            ^ s ^ "`"
			  , fst p, snd p))
	) (false, Smap.empty) lie
    in
    let env = add_variable name (mut, Tstruct str.sname) env in
    if (Smap.cardinal params) == (Smap.cardinal str.param) then
      (finish, env, {ts = (Tletv (name, mut, str.sname, params)); tspos = p})
    else
      raise (Error ("missing some fields in initializer of `"
                            ^ str.sname
                            ^ "`"
		  , fst p, snd p))

  with Not_found ->
    raise (Error ("cannot find struct `"
                  ^ str
                  ^ "` in this file"
                , fst p, snd p))


and check_type_stmt prg env wanted stmt =
  let (s,p) = stmt.s, stmt.spos in
  match s with
    | Semptystmt -> (false, env, { ts = Temptystmt; tspos = p})
    | Sret None ->
      if is_same wanted Tunit then (true, env, {ts = Tret None; tspos = p})
      else typing_error wanted Tunit p
    | Sret (Some expr) ->
      let finish, expr = check_type_expr prg env wanted expr in
      if is_same wanted expr.ttyp then
	(true, env, {ts = Tret (Some expr); tspos = p})
      else typing_error wanted expr.ttyp p
    | Sif sif -> check_type_if prg env wanted p sif
    | Swhile (expr, bloc) ->
      let finish, expr = check_type_expr prg env wanted expr in
      let bloc = check_type_bloc prg env wanted bloc in
      if expr.ttyp == Tybool then
	if bloc.btyp == Tunit then
	  (finish, env, { ts = Twhile (expr, bloc); tspos = p })
	else typing_error Tunit bloc.btyp p
      else typing_error Tybool expr.ttyp p
    | Sexpr expr ->
      let finish, expr = check_type_expr prg env wanted expr in
      (finish, env, {ts = (Texpr expr); tspos = p})
    | Slet (name, mut, expr) ->
      let finish, expr = check_type_expr prg env wanted expr in
      let env = add_variable name (mut, expr.ttyp) env in
      (finish, env, {ts = (Tlet (name, mut, expr)); tspos = p})
    | Sletv (name, mut, str, lie) ->
       check_type_stmt_letv prg env wanted stmt name mut str lie p

and check_type_if prg env wanted pos sif =
  let expr, bloc, se = sif in
  let finish, expr = check_type_expr prg env wanted expr in
  if expr.ttyp == Tybool then
    let bloc = check_type_bloc prg env wanted bloc in
    match se with
      | None ->
         if (is_same bloc.btyp Tunit) || bloc.bret
         then
           finish, env,
           { ts = Tif {
                      ityp = bloc.btyp;
	              icond = expr;
	              itodo = bloc;
	              ielse = None;
                    };
             tspos = pos
           }
         else
           typing_error Tunit bloc.btyp pos
      | Some el ->
         begin
	   let el = check_type_else prg env wanted pos el in
           try
	     let t =
               if bloc.bret
               then el.btyp
               else
                 if el.bret
                 then bloc.btyp
                 else
                   min_usable el.btyp bloc.btyp
             in
	     ((finish || (bloc.bret && el.bret)), env,
	      { ts = Tif {
	                 ityp = t;
	                 icond = expr;
	                 itodo = bloc;
	                 ielse = Some el
	               }; tspos = pos })
	   with Not_found ->
             raise (Error ("if and else have incompatible types"
                         , fst pos, snd pos))
         end
  else
    typing_error Tybool expr.ttyp expr.tepos

and check_type_else prg env wanted pos = function
  | Selse bloc ->
     check_type_bloc prg env wanted bloc
  | Selif sif ->
    let finish ,_,sif = check_type_if prg env wanted pos sif in
    match sif.ts with
      | Tif tsif -> {
	btodo = [sif];
	bexpr = None;
	bret = finish;
	btyp = tsif.ityp;
        btofree = [];
      }
      | _ -> assert false

and check_type_bloc_instr prg env wanted se first follow = 
  let (finish,newenv,first) = check_type_stmt prg env wanted first in
  let size = List.length follow in
  let follow = check_type_bloc prg
                 (if is_let first.ts then newenv else env)
                 wanted (follow, se) in
  let isif = get_if_stmt first in
  if size == 0 && is_none se && is_some isif then
    let isif = get_some isif in
    let expr = {
	te = Teif (isif);
	tepos = first.tspos;
	ttyp = isif.ityp;
	tlv = false;
      } in
    {
      btodo = [];
      bexpr = Some expr;
      bret = finish || follow.bret;
      btyp = expr.ttyp;
      btofree = [];
    }
  else
    {
      btodo = first :: follow.btodo;
      bexpr = follow.bexpr;
      bret = finish || follow.bret;
      btyp = follow.btyp;
      btofree = [];
    }

and check_type_bloc_expr prg env wanted = function
  | None ->
     {
       btodo = [];
       bexpr = None;
       bret = false;
       btyp = Tunit;
       btofree = [];
     }
  | Some expr ->
     let (finish, expr) = check_type_expr prg env wanted expr in
     {
       btodo = [];
       bexpr = Some expr;
       bret = finish;
       btyp = expr.ttyp;
       btofree = [];
     }

and check_type_bloc prg env wanted bloc =
  (* let _ = print_env env in *)
  let ls, se = bloc in
  match ls with
  | first :: follow ->
     check_type_bloc_instr prg env wanted se first follow
  | [] ->
     check_type_bloc_expr prg env wanted se


let check_type_funct prg funct =

  (* Verification du nom de la fonction *)
  let name, param, ret, todo, loc = funct in
  let () =
    if Smap.mem name prg.functs
    then raise (Error ("a function named `"
                       ^ name
                       ^ "` has already been defined in this file"
		       , fst loc, snd loc))
    else ()
  in

  (* Verification des parametres *)
  let num = ref 0 in
  let param = List.fold_left
    (fun acc (mut, id, ut, loc) ->
      let () = num := !num + 1 in
      let typ = userType_to_typ prg ut in
      if
	(is_well_formed prg typ)
	&& not (Smap.mem id acc)
      then
	Smap.add id (!num, {amut = mut; aname = id; atyp = typ }) acc
      else
	let loc1, loc2 = loc in
        if Smap.mem id acc
        then raise (Error ("identifier `" ^ name
                           ^ "` is bound more than once in this parameter list"
                         , loc1, loc2))
        else raise (Error ("unauthorized type of identifier `"
                           ^ name ^"`", loc1, loc2))
    )
   Smap.empty
   param
  in

  (* Verification du type de retour de la fonction *)
  let ret = userTypeOptions prg ret in
  let ret =
  if
    (is_well_formed prg ret)
    && (is_free_borrow ret)
  then ret
  else raise (Error ("unauthorized return type of function "
		     ^ name, fst loc, snd loc))
  in

  (* Typage du bloc d'instructions de la fonction *)
  let tmpprg = add_empty_funct name param ret prg in
  let env = Smap.fold
    (fun key (_,v) acc -> add_variable v.aname (v.amut,v.atyp) acc)
    param
    empty_env
  in
  let todo = check_type_bloc tmpprg env ret todo in
  if (is_same Tunit todo.btyp && todo.bret)
     || (is_same ret todo.btyp )
  then
    add_funct {
        fname = name;
        args = param;
        rtyp = ret;
        todo = todo;
        funloc = loc;
        tofree = [];
      } prg
  else typing_error ret todo.btyp loc



let check_type_decl prg = function
  | Ddeclfun fn -> check_type_funct prg fn
  | Ddeclstruct str -> check_type_struct prg str

let check_type program =
  let program = List.fold_left
    check_type_decl
    empty_program
    program.decls
  in if Smap.mem "main" program.functs then program
    else raise (Error ("main function not found", empty_pos, empty_pos))




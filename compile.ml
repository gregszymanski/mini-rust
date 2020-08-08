open Ast
open Precomp
open Typing
open X86_64

exception Error of string

let create_label name i =
  name ^ "_" ^ (string_of_int i)

let make0 r = xorq r r

let make1 r = xorq r r ++ notq r

let rec copyn n =
  let m = n-8 in
  if n > 0 then
    movq (ind ~ofs:m r9) (reg r10) ++
      movq (reg r10) (ind ~ofs:m rax) ++
      copyn m
  else nop

let rec copyn_inv n =
  let m = n-8 in
  if n > 0 then
    copyn_inv m ++
      movq (ind ~ofs:m r9) (reg r10) ++
      movq (reg r10) (ind ~ofs:m rax)
  else nop

let rec pushn_rax n =
  let m = n-8 in
  if n > 0 then
    movq (ind ~ofs:m rax) (reg r9) ++
      pushq (reg r9)  ++
      pushn_rax m
  else nop

let rec pushn_rax_inv n =
  let m = n-8 in
  if n > 0 then
    pushn_rax_inv m ++
      movq (ind ~ofs:m rax) (reg r9) ++
      pushq (reg r9)
  else nop

let check_bound () =
  movq (ind ~ofs:8 rax) (reg r10) ++
    cmpq (reg r10) (reg r9) ++
    jge "error_bound"

let copy_r9_to_rax prg = function
  | Tunit -> movq (imm 0) (ind rax)
  | Ti32 | Tybool | Tborrow _ | Tborrowmut _ -> movq (reg r9) (ind rax)
  | Tstruct str ->
     let str = Hashtbl.find prg.cstructs str in
     copyn str.cssize
  | Tyvec _ | Tyfreevec ->
     copyn 16

let copy_r9_to_rax_inv prg = function
  | Tunit -> movq (imm 0) (ind rax)
  | Ti32 | Tybool | Tborrow _ | Tborrowmut _ -> movq (reg r9) (ind rax)
  | Tstruct str ->
     let str = Hashtbl.find prg.cstructs str in
     copyn str.cssize
  | Tyvec _ | Tyfreevec ->
     copyn_inv 16

let push_rax prg = function
  | Tunit -> pushq (imm 0)
  | Ti32 | Tybool | Tborrow _ | Tborrowmut _ -> pushq (reg rax)
  | Tstruct str ->
     let str = Hashtbl.find prg.cstructs str in
     pushn_rax str.cssize
  | Tyvec _ | Tyfreevec ->
     pushn_rax 16

let push_rax_inv prg = function
  | Tunit -> pushq (imm 0)
  | Ti32 | Tybool | Tborrow _ | Tborrowmut _ -> pushq (reg rax)
  | Tstruct str ->
     let str = Hashtbl.find prg.cstructs str in
     pushn_rax str.cssize
  | Tyvec _ | Tyfreevec ->
     pushn_rax 16


let compile_binop_arith b =
  let op = match b with
    | Add -> addq
    | Sub -> subq
    | Mul -> imulq
    | Land -> andq
    | Lor -> orq
    | _ -> assert false
  in
  op (reg r9) (reg rax)

let compile_binop_div = movq (imm 0) (reg rdx) ++
                          cqto ++
                          idivq (reg r9)

let compile_binop_tests s b =
  let jcd = match b with
    | Equality -> jnz
    | Dist -> jz
    | Small -> jge
    | Smalleq -> jg
    | Great ->  jle
    | Greateq -> jl
    | _ -> assert false
  in
  make0 (reg rcx) ++
    cmpq (reg r9) (reg rax) ++
    jcd s  ++
    notq (reg rcx) ++
    label s ++
    movq (reg rcx) (reg rax)

let rec compile_expr prg out expr =
  compile_local_expr prg out expr.ctyp expr.ce

and compile_binop s b =
  match b with
  | Add | Sub | Mul -> compile_binop_arith b
  | Div -> compile_binop_div
  | Mod -> compile_binop_div ++
             movq (reg rdx) (reg rax)
  | Equality | Dist | Small | Smalleq | Great | Greateq ->
     compile_binop_tests s b
  | Equ | Land | Lor -> assert false

and compile_unop = function
  | Unmin -> negq (reg rax)
  | Not -> notq (reg rax)
  | Unaddr -> assert false
  | And | Andmut -> assert false

and compile_local_expr prg out typ = function
  | Cemptyexpr -> nop
  | Cconst i -> movq (imm i) (reg rax)
  | Cbool b -> (if b then make1 else make0) (reg rax)
  | Cvar i ->
     if is_mul_typ typ
     then leaq (ind ~ofs:i rbp) rax
     else movq (ind ~ofs:i rbp) (reg rax)
  | Cbinop(Land, e1, e2, i) ->
     let l = create_label "follow" i in
     make0 (reg rcx) ++
       compile_expr prg out e1 ++
       testq (reg rax) (reg rax) ++
       jz l ++
       compile_expr prg out e2 ++
       testq (reg rax) (reg rax) ++
       jz l ++
       make1 (reg rcx) ++
       label l
  | Cbinop(Lor, e1, e2, i) ->
     let l = create_label "follow" i in
     make1 (reg rcx) ++
       compile_expr prg out e1 ++
       testq (reg rax) (reg rax) ++
       jnz l ++
       compile_expr prg out e2 ++
       testq (reg rax) (reg rax) ++
       jnz l ++
       make0 (reg rcx) ++
       label l
  | Cbinop (b, e1, e2, i) ->
     compile_expr prg out e2 ++
       pushq (reg rax) ++
       compile_expr prg out e1 ++
       popq r9 ++
       compile_binop (create_label "follow" i) b
  | Cassig (e1, e2) ->
     compile_expr prg out e2 ++
       pushq (reg rax) ++
       compile_left_expr prg out e1 ++
       popq r9 ++
       copy_r9_to_rax prg e2.ctyp
  | Cunop (And, e) | Cunop (Andmut, e) ->
     compile_left_expr prg out e
  | Cunop (Unaddr, e) ->
     compile_expr prg out e ++
       if is_vec typ then nop else movq (ind rax) (reg rax)
  | Cunop (u,e) -> compile_expr prg out e ++
                     compile_unop u
  | Cattribut (e, i) ->
     compile_left_expr prg out e ++
       if is_mul_typ typ
       then leaq (ind ~ofs:i rax) rax
       else movq (ind ~ofs:i rax) (reg rax)

  | Clength e ->
     compile_expr prg out e ++ movq (ind ~ofs:8 rax) (reg rax)
  | Cacces (e1, e2) ->
     compile_expr prg out e2 ++
       pushq (reg rax) ++
       compile_expr prg out e1 ++
       popq r9 ++
       check_bound () ++
       movq (ind rax) (reg rax) ++
       imulq (imm (size_of_typ prg.cstructs typ)) (reg r9) ++
       leaq (ind ~index:r9 rax) rax ++
       if is_mul_typ typ
       then nop
       else movq (ind rax) (reg rax)

  | Capp (s, le) ->
     let f = Smap.find s prg.cfuncts in
     push_list_expr prg out le
     ++ call (create_label "function" f.cnum)
     ++ free_list_expr (Smap.find s prg.cfuncts)
  | Cvec (nbrElem, sizeElem, le) ->
     movl (imm (nbrElem * sizeElem)) (reg edi) ++
       call "malloc" ++
       pushq (imm nbrElem) ++
       pushq (reg rax) ++
       List.fold_left (fun acc (i,e) ->
           acc ++
             compile_expr prg out e ++
             movq (reg rax) (reg r9) ++
             movq (ind rsp) (reg rax) ++
             leaq (ind ~ofs:(i*sizeElem) rax) rax ++
             copy_r9_to_rax_inv prg e.ctyp
         ) nop le ++
       movq (reg rsp) (reg rax) ++
       addq (imm 16) (reg rsp)
  | Cprint s -> compile_print s
  | Cbloc b -> compile_bloc prg out b
  | Ceif cif -> compile_if prg out cif

and push_list_expr prg out le =
  Smap.fold (fun key e acc ->
      compile_expr prg out e ++
        push_rax_inv prg e.ctyp ++ 
        acc
    ) le nop

and free_list_expr f =
  addq (imm (f.csizeargs - 8)) (reg rsp)

and compile_left_expr prg out expr =
  compile_local_left_expr prg expr.ctyp out expr.ce

and compile_local_left_expr prg typ out = function
  | Cemptyexpr -> nop
  | Cconst i -> raise (Error "Not a left value")
  | Cbool b -> raise (Error "Not a left value")
  | Cvar i ->
     leaq (ind ~ofs:i rbp) rax
  | Cbinop (b, e1, e2, i) -> raise (Error "Not a left value")
  | Cassig (e1, e2) -> raise (Error "Not a left value")
  | Cunop (Unaddr,e) -> compile_expr prg out e
  | Cunop (u,e) -> raise (Error "Not a left value")
  | Cattribut (e, i) ->
     compile_left_expr prg out e ++
       addq (imm i) (reg rax)
  | Clength e -> raise (Error "Not a left value")
  | Cacces (e1, e2) ->
     compile_expr prg out e2 ++
       pushq (reg rax) ++
       compile_expr prg out e1 ++
       popq r9 ++
       check_bound () ++
       movq (ind rax) (reg rax) ++
       imulq (imm (size_of_typ prg.cstructs typ)) (reg r9) ++
       leaq (ind ~index:r9 rax) rax

  | Capp (s, le) ->
     compile_local_expr prg out typ (Capp(s,le))
  | Cvec _ -> raise (Error "Not a left value")
  | Cprint s -> raise (Error "Not a left value")
  | Cbloc b -> raise (Error "Not a left value")
  | Ceif cif -> raise (Error "Not a left value")

and compile_stmt prg out = function
  | Cemptystmt -> nop
  | Cexpr e -> compile_expr prg out e
  | Clet (ofs,e) -> compile_expr prg out e ++
                      movq (reg rax) (reg r9) ++
                      leaq (ind ~ofs rbp) rax ++
                      copy_r9_to_rax prg e.ctyp
(*                      movq (reg rax) (ind ~ofs rbp) *)
  | Cletstr (i,s,le) ->
     let str = Hashtbl.find prg.cstructs s in
     Smap.fold (fun key expr acc ->
         let pos = i + (Smap.find key str.csparam) in
         acc ++
           compile_expr prg out expr ++
           movq (reg rax) (reg r9) ++
           leaq (ind ~ofs:pos rbp) rax ++
           copy_r9_to_rax prg expr.ctyp
       ) le nop
  | Cwhile (i,e,b) ->
     let begin_while = create_label "begin_while" i in
     let end_while = create_label "end_while" i in
     label begin_while ++
       compile_expr prg out e ++
       testq (reg rax) (reg rax) ++
       jz end_while ++
       compile_bloc prg out b ++
       jmp begin_while ++
       label end_while
  | Cret (Some e) ->
     compile_expr prg out e ++
       if is_mul_typ e.ctyp
       then (push_rax prg e.ctyp ++ movq (reg rsp) (reg rax))
       else nop ++
       jmp out
  | Cret None ->  jmp out
  | Cif cif -> compile_if prg out cif
  | Tfreevec expr ->
     compile_expr prg out expr ++
       movl (ind rax) (reg edi) ++
       call "free"

and compile_print s =
  movq (lab ("$" ^ s)) (reg rdi) ++
    make0 (reg rax) ++
    call "printf"

and compile_if prg out cif =
  let begin_if = create_label "begin_if" cif.ccount in
  let begin_else = create_label "begin_else" cif.ccount in
  let end_if = create_label "end_if" cif.ccount in
  label begin_if ++
    compile_expr prg out cif.cicond ++
    testq (reg rax) (reg rax) ++
    jz begin_else ++
    compile_bloc prg out cif.citodo ++
    jmp end_if ++
    label begin_else ++
    compile_bloc prg out cif.cielse ++
    label end_if

and compile_bloc prg out bloc =
  List.fold_left (fun acc s -> acc ++ compile_stmt prg out s) nop bloc.cbtodo ++
    compile_expr prg out bloc.cexpr ++
    List.fold_left (fun acc s -> acc ++ compile_stmt prg out s) nop bloc.ctofree

and compile_fun prg f =
  (if f.cfname = "main" then label f.cfname else nop) ++
    label (create_label "function" f.cnum) ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    subq (imm f.cframe) (reg rsp) ++
    compile_bloc prg (create_label "end_function" f.cnum) f.ctodo ++
    label (create_label "end_function" f.cnum) ++
    (if f.cfname = "main" then make0 (reg rax) else nop) ++
    (* addq (imm f.cframe) (reg rsp) *) movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret



let error_bound =
  label "error_bound" ++
    compile_print "string_error_bound" ++
    movl (imm 1) (reg edi) ++
    call "exit"


let compile_program prg =
  { text =
      globl "main" ++
	Smap.fold (fun k f acc ->
            acc ++ compile_fun prg f)
          prg.cfuncts nop
          ++ error_bound
  ;
    data =
      Smap.fold (
          fun key s acc ->
          acc ++ label key ++ string s
        ) prg.cstrings nop
      ++ label "string_error_bound"
      ++ string "Error: vec index out of bounds\n"
  }



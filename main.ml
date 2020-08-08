open Lexing
open Ast
open Bcheck
open Precomp
open X86_64

(*
let print_tab n =
  print_string (String.make n '\t')


let rec print_if n = function
  | Sif (e,b,None) ->
     print_tab n; print_string "if cond\n"
  | Sif (e,b,Some el) ->
     begin
       print_tab n;
       print_string "if cond\n";
       print_tab n;
       print_string "else\n";
       match el with
       | Selse b -> ()
       | Selif i -> print_if (n+1) (Sif i)
     end

let test_if = function
  | Sif i -> print_if 0 (Sif i)
  | _ -> ()
 *)

(*
Steps:
   0: Do nothing
   1: parse only
   2: type only
   3: no asm
   4: usual use
*)

let endAtStep = ref 4
let tmp_nameFile = ref ""
let () = Array.iter
  (fun a -> match a with
  | "--parse-only" -> endAtStep := 1
  | "--type-only" -> endAtStep := 2
  | "--no-asm" -> endAtStep := 3
  | _ -> tmp_nameFile := a
  )
  (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
let nameFile = !tmp_nameFile


let print_pos lb =

  let l = lb.pos_lnum in
  let c = lb.pos_cnum - lb.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" nameFile l (c-1) c

  (*let start = Lexing.lexeme_start_p lb in
  let line = start.pos_lnum in
  let character = start.pos_cnum - start.pos_bol in
  Format.eprintf
    "File \"%s\", line %d, characters %d-%d:\n"
    nameFile
    line
    character
    (character + 1)*)

let print_multipos l1 l2 =
  Format.eprintf
    "File \"%s\", line %d, characters %d-%d:\n"
    nameFile
    l1.pos_lnum
    0
    0


let nameOutput = Filename.chop_suffix nameFile ".rs" ^ ".s"

let c = open_in nameFile
let lb = Lexing.from_channel c
let () =
  try
    if !endAtStep > 0 then
      let v = Parser.prog Lexer.token lb in
      if !endAtStep > 1 then
	let b = Typing.check_type v in
	if !endAtStep > 2 then
          let b = Bcheck.bcheck_prg b in
	  let p = Precomp.precomp_prg b in
	  if !endAtStep > 3 then
            let c = Compile.compile_program p in
	    let f = open_out nameOutput in
	    let fmt = Format.formatter_of_out_channel f in
	    X86_64.print_program fmt c;
	    Format.fprintf fmt "@?";
	    close_out f
  with
  | Lexer.Lexing_error e ->
     print_pos (Lexing.lexeme_start_p lb);
    Format.eprintf
      "Lexical error: %s@."
      e;
    exit 1
  | Parser.Error ->
     print_pos (Lexing.lexeme_start_p lb);
    Format.eprintf
      "Syntax error@."
    ;
    exit 1
  | Bcheck.Error (e,p1,p2) ->
     print_pos p1;
    Format.eprintf
      "Typing2 error : %s@."
      e;
    exit 1
  | Typing.Error (e,p1,p2) ->
     print_pos p1;
    Format.eprintf
      "Typing error : %s@."
      e;
    exit 1
  | _ -> exit 2

let () = close_in c

  (*
let v = v.decls
let v = List.rev v
let v = List.hd v
let () =
  match v with
  | Ddeclfun (_,_,_,b) ->
     let l,b = b in
     let v = List.hd (List.rev l) in
     test_if v
  
  *)
  

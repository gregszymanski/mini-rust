
(** {0 Bibliothèque pour l'écriture de programmes X86-64 }

    Il s'agit là uniquement d'un fragment relativement petit
    de l'assembleur X86-64.

    @author Jean-Christophe Filliâtre (CNRS)
    @author Kim Nguyen (Université Paris Sud)
*)

(** {1 Code } *)

type 'a asm
  (** type abstrait du code assembleur.
      Le paramètre ['a] est utilisé comme type fantôme. *)

type text = [ `text ] asm
  (** du code assembleur se trouvant dans la zone de texte *)

type data = [ `data ] asm
  (** du code assembleur se trouvant dans la zone de données *)

type label = string
  (** les étiquettes d'addresses sont des chaînes de caractères *)

val nop : [> ] asm
  (** l'instruction vide. Peut se trouver dans du text ou du data *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
  (** concatène deux bouts de codes (soit text avec text, soit data avec
      data) *)

val inline: string -> [> ] asm
  (** [inline s] recopie la chaîne [s] telle quelle dans le fichier
      assembleur *)

type program = {
  text : text;
  data : data;
}
  (** un programme est constitué d'une zone de texte et d'une zone de données *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] imprime le code du programme [p] dans le
      formatter [fmt] *)

val print_in_file: file:string -> program -> unit

(** {1 Registres } *)

type size = [`B | `W | `L | `Q]

type 'size register
  (** type abstrait des registres *)

val rax: [`Q] register
val rbx: [`Q] register
val rcx: [`Q] register
val rdx: [`Q] register
val rsi: [`Q] register
val rdi: [`Q] register
val rbp: [`Q] register
val rsp: [`Q] register
val r8 : [`Q] register
val r9 : [`Q] register
val r10: [`Q] register
val r11: [`Q] register
val r12: [`Q] register
val r13: [`Q] register
val r14: [`Q] register
val r15: [`Q] register
  (** registres 64 bits *)

val eax: [`L] register
val ebx: [`L] register
val ecx: [`L] register
val edx: [`L] register
val esi: [`L] register
val edi: [`L] register
val ebp: [`L] register
val esp: [`L] register
val r8d: [`L] register
val r9d: [`L] register
val r10d: [`L] register
val r11d: [`L] register
val r12d: [`L] register
val r13d: [`L] register
val r14d: [`L] register
val r15d: [`L] register
  (** registres 32 bits *)

val ax: [`W] register
val bx: [`W] register
val cx: [`W] register
val dx: [`W] register
val si: [`W] register
val di: [`W] register
val bp: [`W] register
val sp: [`W] register
val r8w: [`W] register
val r9w: [`W] register
val r10w: [`W] register
val r11w: [`W] register
val r12w: [`W] register
val r13w: [`W] register
val r14w: [`W] register
val r15w: [`W] register
  (** registres 16 bits *)

val al: [`B] register
val bl: [`B] register
val cl: [`B] register
val dl: [`B] register
val ah: [`B] register
val bh: [`B] register
val ch: [`B] register
val dh: [`B] register
val sil: [`B] register
val dil: [`B] register
val bpl: [`B] register
val spl: [`B] register
val r8b: [`B] register
val r9b: [`B] register
val r10b: [`B] register
val r11b: [`B] register
val r12b: [`B] register
val r13b: [`B] register
val r14b: [`B] register
val r15b: [`B] register
  (** registres 8 bits *)

(** {1 Opérandes } *)

type 'size operand
  (** Le type abstrait des opérandes *)

val imm: int -> [>] operand
  (** opérande immédiate $i *)
val imm32: int32 -> [>] operand
  (** opérande immédiate $i *)
val reg: 'size register -> 'size operand
  (** registre *)
val ind: ?ofs:int -> ?index:'size1 register -> ?scale:int ->
  'size2 register -> [>] operand
  (** opérande indirecte ofs(register, index, scale) *)
val lab: label -> [>] operand
  (** étiquette L  *)
val ilab: label -> [`Q] operand
  (** étiquette immédiate $L *)

(** {1 Instructions } *)

(** {2 Transfert } *)

val movb: [`B] operand -> [`B] operand -> text
val movw: [`W] operand -> [`W] operand -> text
val movl: [`L] operand -> [`L] operand -> text
val movq: [`Q] operand -> [`Q] operand -> text
  (** attention : toutes les combinaisons d'opérandes ne sont pas permises *)
val movsbw: [`B] operand -> [`W] register -> text
val movsbl: [`B] operand -> [`L] register -> text
val movsbq: [`B] operand -> [`Q] register -> text
val movswl: [`W] operand -> [`L] register -> text
val movswq: [`W] operand -> [`Q] register -> text
val movslq: [`L] operand -> [`Q] register -> text
  (** 8->64 bit, avec extension de signe *)
val movzbw: [`B] operand -> [`W] register -> text
val movzbl: [`B] operand -> [`L] register -> text
val movzbq: [`B] operand -> [`Q] register -> text
val movzwl: [`W] operand -> [`L] register -> text
val movzwq: [`W] operand -> [`Q] register -> text
  (** 8->64 bit, avec extension par zéro *)

val movabsq: int64 -> [`Q] register -> text
  (** copie une valeur immédiate 64 bits dans un registre *)

(** {2 Arithmétique } *)

val leab: [`B] operand -> [`B] register -> text
val leaw: [`W] operand -> [`W] register -> text
val leal: [`L] operand -> [`L] register -> text
val leaq: [`Q] operand -> [`Q] register -> text

val incb: [`B] operand -> text
val incw: [`W] operand -> text
val incl: [`L] operand -> text
val incq: [`Q] operand -> text

val decb: [`B] operand -> text
val decw: [`W] operand -> text
val decl: [`L] operand -> text
val decq: [`Q] operand -> text

val negb: [`B] operand -> text
val negw: [`W] operand -> text
val negl: [`L] operand -> text
val negq: [`Q] operand -> text

val addb: [`B] operand -> [`B] operand -> text
val addw: [`W] operand -> [`W] operand -> text
val addl: [`L] operand -> [`L] operand -> text
val addq: [`Q] operand -> [`Q] operand -> text

val subb: [`B] operand -> [`B] operand -> text
val subw: [`W] operand -> [`W] operand -> text
val subl: [`L] operand -> [`L] operand -> text
val subq: [`Q] operand -> [`Q] operand -> text

val imulw: [`W] operand -> [`W] operand -> text
val imull: [`L] operand -> [`L] operand -> text
val imulq: [`Q] operand -> [`Q] operand -> text

val idivq: [`Q] operand -> text
val cqto: text

(** {2 Opérations logiques } *)

val notb: [`B] operand -> text
val notw: [`W] operand -> text
val notl: [`L] operand -> text
val notq: [`Q] operand -> text

val andb: [`B] operand -> [`B] operand -> text
val andw: [`W] operand -> [`W] operand -> text
val andl: [`L] operand -> [`L] operand -> text
val andq: [`Q] operand -> [`Q] operand -> text

val orb : [`B] operand -> [`B] operand -> text
val orw : [`W] operand -> [`W] operand -> text
val orl : [`L] operand -> [`L] operand -> text
val orq : [`Q] operand -> [`Q] operand -> text

val xorb: [`B] operand -> [`B] operand -> text
val xorw: [`W] operand -> [`W] operand -> text
val xorl: [`L] operand -> [`L] operand -> text
val xorq: [`Q] operand -> [`Q] operand -> text
  (** Opérations de manipulation de bits. "et" bit à bit, "ou" bit à
       bit, "not" bit à bit *)

(** {2 Décalages } *)

val shlb: [`B] operand -> [`B] operand -> text
val shlw: [`W] operand -> [`W] operand -> text
val shll: [`L] operand -> [`L] operand -> text
val shlq: [`Q] operand -> [`Q] operand -> text
  (** note: shl est la même chose que sal *)

val shrb: [`B] operand -> [`B] operand -> text
val shrw: [`W] operand -> [`W] operand -> text
val shrl: [`L] operand -> [`L] operand -> text
val shrq: [`Q] operand -> [`Q] operand -> text

val sarb: [`B] operand -> [`B] operand -> text
val sarw: [`W] operand -> [`W] operand -> text
val sarl: [`L] operand -> [`L] operand -> text
val sarq: [`Q] operand -> [`Q] operand -> text

(** {2 Sauts } *)

val call: label -> text
val call_star: [`Q] operand -> text
val leave: text
val ret: text
  (** appel de fonction et retour *)

val jmp : label -> text
  (** saut inconditionnel *)
val jmp_star: [`Q] operand -> text
  (** saut à une adresse calculée *)

val je : label -> text  (* =  0 *)
val jz : label -> text  (* =  0 *)
val jne: label -> text  (* <> 0 *)
val jnz: label -> text  (* <> 0 *)
val js : label -> text  (* <  0 *)
val jns: label -> text  (* >= 0 *)
val jg : label -> text  (* >  signé *)
val jge: label -> text  (* >= signé *)
val jl : label -> text  (* <  signé *)
val jle: label -> text  (* <= signé *)
val ja : label -> text  (* >  non signé *)
val jae: label -> text  (* >= non signé *)
val jb : label -> text  (* <  non signé *)
val jbe: label -> text  (* <= non signé *)
  (** sauts conditionnels *)

(** {2 Conditions } *)

val cmpb: [`B] operand -> [`B] operand -> text
val cmpw: [`W] operand -> [`W] operand -> text
val cmpl: [`L] operand -> [`L] operand -> text
val cmpq: [`Q] operand -> [`Q] operand -> text

val testb: [`B] operand -> [`B] operand -> text
val testw: [`W] operand -> [`W] operand -> text
val testl: [`L] operand -> [`L] operand -> text
val testq: [`Q] operand -> [`Q] operand -> text

val sete : [`B] operand -> text  (* =  0 *)
val setne: [`B] operand -> text  (* <> 0 *)
val sets : [`B] operand -> text  (* <  0 *)
val setns: [`B] operand -> text  (* >= 0 *)
val setg : [`B] operand -> text  (* >  signé *)
val setge: [`B] operand -> text  (* >= signé *)
val setl : [`B] operand -> text  (* <  signé *)
val setle: [`B] operand -> text  (* <= signé *)
val seta : [`B] operand -> text  (* >  non signé *)
val setae: [`B] operand -> text  (* >= non signé *)
val setb : [`B] operand -> text  (* <  non signé *)
val setbe: [`B] operand -> text  (* <= non signé *)
  (** positionne l'octet opérande à 1 ou 0 selon que le test est vrai ou non *)

(** {2 Manipulation de la pile} *)

val pushq : [`Q] operand -> text
  (** [pushq r] place le contenu de [r] au sommet de la pile.
      Rappel : %rsp pointe sur l'adresse de la dernière case occupée *)

val popq : [`Q] register -> text
  (** [popq r] place le mot en sommet de pile dans [r] et dépile *)

(** {2 Divers } *)

val label : label -> [> ] asm
  (** un label. Peut se retrouver dans du text ou du data *)
val globl : label -> [> ] asm
  (** déclaration .globl (pour main, typiquement) *)

val comment : string -> [> ] asm
  (** place un commentaire dans le code généré. Peut se retrouver dans
      du text ou du data *)

(** {2 Données } *)

val string : string -> data
  (** une constante chaîne de caractères (terminée par 0) *)
val dbyte : int list -> data
val dword : int list -> data
val dint : int list -> data
val dquad : int list -> data
  (** place une liste de valeurs sur 1/2/4/8 octets dans la zone data *)
val address: label list -> data
  (** place une liste d'adresses dans la zone data (avec .quad) *)
val space: int -> data
  (** [space n] alloue [n] octets (valant 0) dans le segment de données *)


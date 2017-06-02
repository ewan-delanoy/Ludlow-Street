(*

#use"Php_analizer/php_char_range.ml";;

*)

type t=CR of (Lexing.position * Lexing.position);;

let make a b=CR(a,b);;

let unveil(CR(a,b))=(a,b);;

let fst (CR(a,b))=a;;
let snd (CR(a,b))=b;;

let chasles (CR(a,_)) (CR(_,b))=CR(a,b);;

let dummy_lexing=
{Lexing.pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0};;

let dummy=
  CR(dummy_lexing,dummy_lexing);;
  
let add_if_nondummy x l=
   if x=dummy_lexing then l else x::l;;

let select_head l=
   if l=[] 
   then dummy_lexing
   else List.hd(l);;

(*
let print (CR(a,b))=
  let s1=string_of_int(a.Lexing.pos_cnum)
  and s2=string_of_int(b.Lexing.pos_cnum) in
  "char_range("^s1^","^s2^")";;

let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
 *)     
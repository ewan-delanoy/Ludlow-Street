(*

#use"Php_analizer/positioned_php_token.ml";;

*)

type t=
    PPL of Php_token.t*(Lexing.position * Lexing.position);; 

let make x y=PPL(x,y);;
let unveil (PPL(x,y))=(x,y);;
let fst (PPL(x,y))=x;;
let snd (PPL(x,y))=y;;

let print (PPL(x,y))=
  let s=Php_token.content x in
  if String.length(s)>50
  then "\xe2\x8c\x98...\xe2\x8c\x98 "
  else "\xe2\x8c\x98 "^s^"\xe2\x8c\x98 ";;

let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;

let print_list=
  Pretty_print_list.pretty_print_list  
    ((fun (PPL(x,y))->Php_token.short_content x),100," ");;    
    
let print_out_list (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print_list x);
   Format.close_box();;
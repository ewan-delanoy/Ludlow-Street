(*

#use"Php_analizer/php_positioned_molecule_list.ml";;

*)

type t= ( Php_molecule.t * (Lexing.position * Lexing.position)  ) list;;

    
let print (x:t)=
  let temp1=Image.image(fun pair->
    Php_molecule.readable(fst pair)
   ) x in
  "\xe3\x80\x90  "^(String.concat " " temp1)^"  \xe3\x80\x91";;

let print_out (fmt:Format.formatter) (x:t)=
   Format.fprintf fmt "@[%s@]" (print x);;

    
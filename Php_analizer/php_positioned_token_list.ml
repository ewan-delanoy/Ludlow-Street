(*

#use"Php_analizer/php_positioned_token_list.ml";;

*)

type t= ( Php_token.t * (Lexing.position * Lexing.position)  ) list;;

    
let print (x:t)=
  let temp1=Image.image(fun ptok->
    let tok=fst ptok in
    Php_projected_token.readable(Php_token.form tok)
   ) x in
  "\xe3\x80\x90  "^(String.concat " " temp1)^"  \xe3\x80\x91";;

let print_out (fmt:Format.formatter) (x:t)=
   Format.fprintf fmt "@[%s@]" (print x);;

    
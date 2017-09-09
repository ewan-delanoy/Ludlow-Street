(*

#use"Php_analizer/php_parser.ml";;

*)

type 'a t= 
((Php_positioned_token_list.t )->
((('a)*Php_char_range.t*(Php_positioned_token_list.t)) option));;

 
let parse (f:'a t) l=f l;;  
 

  

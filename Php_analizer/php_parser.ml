(*

#use"Php_analizer/php_parser.ml";;

*)

type 'a t= 
((Positioned_php_token.t list)->
((('a)*Php_char_range.t*(Positioned_php_token.t list)) option));;

 
let parse (f:'a t) l=f l;;  
 

  

(*

#use"Php_analizer/php_recognizer.ml";;

*)

type t=( Php_positioned_token_list.t -> 
(Php_char_range.t * Php_positioned_token_list.t) option );;

let recognize (f:t) l=f l;;
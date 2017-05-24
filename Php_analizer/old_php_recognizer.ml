(*

#use"Php_analizer/php_recognizer.ml";;

*)

type t=( Positioned_php_token.t list -> (Php_char_range.t * Positioned_php_token.t list) option );;

let recognize (f:t) l=f l;;
(*

#use"Php_analizer/Beavers/beaver_result.ml";;


*)

type t={
    description : string;
    caught_items : Php_positioned_token_list.t;
    deferred_items : (string*Php_positioned_token_list.t) list;
    char_range : Php_char_range.t; 
};;

let make a b c d={
  description =a;
  caught_items =b;
  deferred_items =c;
  char_range =d; 
};;

let description x=x.description;;
let caught_items x=x.caught_items;;
let deferred_items x=x.deferred_items;;
let char_range x=x.char_range;;
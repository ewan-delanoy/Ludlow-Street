(*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=
     Kwd of Php_keyword.t
    |Punct of Php_punctuator.t
    |Op of Php_operator.t;;

let to_string=function
      (Kwd s)->Php_keyword.make_visible s
     |(Punct s)->Php_punctuator.to_string s
     |(Op s)->Php_operator.make_visible s;;
  
let all_pairs=
       let kwds=Image.image (fun kwd->(Php_keyword.make_visible kwd,Kwd kwd)) Php_keyword.all
       and puncts=Image.image (fun pkt->(Php_punctuator.to_string pkt,Punct pkt)) Php_punctuator.all_punctuators
       and ops=Image.image (fun op->(Php_operator.make_visible op,Op op)) Php_operator.all in
  Ordered.forget_order(Ordered.diforchan Keyval_ordering.ko  
     (kwds@puncts@ops) );;

let all_string_constants=Image.image fst all_pairs;;

let all=Image.image snd all_pairs;;

exception Unknown of string;;

let of_string s=
   try List.assoc s all_pairs with
   _->raise(Unknown(s));;

let putative_of_string s=try (Some(of_string s)) with _->None;;


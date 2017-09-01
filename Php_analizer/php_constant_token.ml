(*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=
     Kwd of Php_keyword.t
    |Punct of Php_punctuator.t
    |Op of Php_operator.t;;

let to_string=function
      (Kwd s)->Php_keyword.to_string s
     |(Punct s)->Php_punctuator.to_string s
     |(Op s)->Php_operator.to_string s;;
  
let all_pairs=
       let kwds=Image.image (fun (s,kwd)->(s,Kwd kwd)) Php_keyword.all_pairs 
       and puncts=Image.image (fun (s,punct)->(s,Punct punct)) Php_punctuator.all_pairs
       and ops=Image.image (fun (s,op)->(s,Op op)) Php_operator.all_pairs in
  Ordered.forget_order(Ordered.diforchan Keyval_ordering.ko  
     (kwds@puncts@ops) );;

let all_string_constants=Image.image fst all_pairs;;

let all=Image.image snd all_pairs;;

exception Unknown of string;;

let of_string s=
   try List.assoc s all_pairs with
   _->raise(Unknown(s));;

let putative_of_string s=try (Some(of_string s)) with _->None;;

(*
let token_category=function
      Kwd(_)           ->Token_category.Keyword
     |Punct(_)         ->Token_category.Punctuator
     |Op(_)            ->Token_category.Operator;;
*)

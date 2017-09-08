(*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=[ Php_keyword.t | Php_punctuator.t | Php_operator.t ];;

let c_kwd=((fun (kwd:Php_keyword.t) ->(kwd:>t)) : Php_keyword.t -> t);;

let c_punct (pkt:Php_punctuator.t)=(pkt:>t);;
let c_op (op:Php_operator.t)=(op:>t);;    

let precedence ctok=
    Option.catch_exception Php_operator.precedence ctok;; 

let all_pairs=
       let kwds=Image.image (fun kwd->(Php_keyword.make_visible kwd,c_kwd kwd)) Php_keyword.all
       and puncts=Image.image (fun pkt->(Php_punctuator.make_visible pkt,c_punct pkt)) Php_punctuator.all
       and ops=Image.image (fun op->(Php_operator.make_visible op,c_op op)) Php_operator.all in
  Ordered.diforchan_plaen Keyval_ordering.ko  
     (kwds@puncts@ops) ;;

let all=Image.image snd all_pairs;;

exception Unknown of string;;

let from_visible s=
   try List.assoc s all_pairs with
   _->raise(Unknown(s));;

let make_visible tok=
    let (s1,_)=Option.find_really(fun (s,tok1)->tok1=tok) all_pairs in
    s1;;
(*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=
     Kwd of Php_keyword.t
    |Punct of Php_punctuator.t
    |Op of Php_operator.t;;

let c_kwd kwd=Kwd(kwd);;
let c_punct pkt=Punct(pkt);;
let c_op op=Op(op);;    

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
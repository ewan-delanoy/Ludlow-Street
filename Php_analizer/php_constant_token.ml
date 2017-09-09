(*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=[ Php_keyword.t | Php_punctuator.t | Php_operator.t ];;

let c_kwd=((fun (kwd:Php_keyword.t) ->(kwd:>t)) : Php_keyword.t -> t);;

let c_punct (pkt:Php_punctuator.t)=(pkt:>t);;
let c_op (op:Php_operator.t)=(op:>t);;    

let precedence (ctok:t)=match ctok with
    |#Php_operator.t as op->Some(Php_operator.precedence op)
    |_->None;;

let names_and_tokens=
       let kwds=Image.image (fun kwd->(Php_keyword.short_name kwd,c_kwd kwd)) Php_keyword.all
       and puncts=Image.image (fun pkt->(Php_punctuator.short_name pkt,c_punct pkt)) Php_punctuator.all
       and ops=Image.image (fun op->(Php_operator.short_name op,c_op op)) Php_operator.all in
  Ordered.diforchan_plaen Keyval_ordering.ko  
     (kwds@puncts@ops) ;;

let visibles_and_tokens=
        let kwds=Image.image (fun kwd->(Php_keyword.make_visible kwd,c_kwd kwd)) Php_keyword.all
        and puncts=Image.image (fun pkt->(Php_punctuator.make_visible pkt,c_punct pkt)) Php_punctuator.all
        and ops=Image.image (fun op->(Php_operator.make_visible op,c_op op)) Php_operator.all in
      (kwds@puncts@ops) ;;     

let all=Image.image snd names_and_tokens;;

exception Unknown_short_name of string;;
exception Unknown_visible of string;;



let short_name (ctok:t)=match ctok with 
     #Php_operator.t as op->Php_operator.short_name op
    |#Php_keyword.t as kwd->Php_keyword.short_name kwd
    |#Php_punctuator.t as pkt->Php_punctuator.short_name pkt;;

let from_short_name s=
        try List.assoc s names_and_tokens with
        _->raise(Unknown_short_name(s));;    

let make_visible (ctok:t)=match ctok with 
    #Php_operator.t as op->Php_operator.make_visible op
   |#Php_keyword.t as kwd->Php_keyword.make_visible kwd
   |#Php_punctuator.t as pkt->Php_punctuator.make_visible pkt;;    

let from_visible s=
    try List.assoc s visibles_and_tokens with
    _->raise(Unknown_visible(s));;
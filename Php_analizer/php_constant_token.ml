(*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=[ Php_keyword.t | Php_punctuator.t | Php_operator.t ];;

let of_keyword=((fun (kwd:Php_keyword.t) ->(kwd:>t)) : Php_keyword.t -> t);;
let of_punctuator (pkt:Php_punctuator.t)=(pkt:>t);;
let of_operator (op:Php_operator.t)=(op:>t);;    

let seek_punctuator (ctok:t)=match ctok with 
#Php_operator.t->None
|#Php_keyword.t->None
|#Php_punctuator.t as pkt->Some(pkt);;  


let precedence (ctok:t)=match ctok with
    |#Php_operator.t as op->Some(Php_operator.precedence op)
    |_->None;;

let visibles_and_tokens=
        let kwds=Image.image (fun kwd->(Php_keyword.make_visible kwd,of_keyword kwd)) Php_keyword.all
        and puncts=Image.image (fun pkt->(Php_punctuator.make_visible pkt,of_punctuator pkt)) Php_punctuator.all
        and ops=Image.image (fun op->(Php_operator.make_visible op,of_operator op)) Php_operator.all in
      (kwds@puncts@ops) ;;     

let all=Image.image snd visibles_and_tokens;;


exception Unknown_visible of string;;

  

let make_visible (ctok:t)=match ctok with 
    #Php_operator.t as op->Php_operator.make_visible op
   |#Php_keyword.t as kwd->Php_keyword.make_visible kwd
   |#Php_punctuator.t as pkt->Php_punctuator.make_visible pkt;;    

let from_visible s=
    try List.assoc s visibles_and_tokens with
    _->raise(Unknown_visible(s));;

let readable (ctok:t)=match ctok with 
    #Php_operator.t as op->Php_operator.readable op
   |#Php_keyword.t as kwd->Php_keyword.readable kwd
   |#Php_punctuator.t as pkt->Php_punctuator.readable pkt;;    
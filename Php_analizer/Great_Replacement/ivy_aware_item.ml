(*

#use"Php_analizer/Great_Replacement/ivy_aware_item.ml";;



*)

type t={
    kind : Ivy_aware_kind.t;
    before_content : string;
    content : string;
    after_content : string;
};;

let kind x=x.kind;;
let before_content x=x.before_content;;
let content x=x.content;;
let after_content x=x.after_content;;

let full_content x=
    (x.before_content)^
    (x.content)^
    (x.after_content);;  

let length x=
   String.length(x.before_content)+
   String.length(x.content)+
   String.length(x.after_content);; 

let non_ivy text={
    kind =Ivy_aware_kind.non_ivy;
    before_content="";
    content =text;
    after_content="";
};;

let make a b c d={
    kind =a;
    before_content=b;
    content =c;
    after_content=d;
};;
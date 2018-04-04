(*

#use"Php_analizer/HRecognizer/standard_jdsijunction.ml";;

A standard jdisjunction is a disjunction of chains.



*)

type t=S of ((Nonatomic_jrecognizer.t list) list);;


let unveil (S x)=x;;

let veil x=(S x);;
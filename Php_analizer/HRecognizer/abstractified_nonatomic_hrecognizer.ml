(*

#use"Php_analizer/HRecognizer/abstractified_nonatomic_hrecognizer.ml";;

Unfortunate coupling with the type definition in nonatomic_hrecognizer module,
but I couldn't find anything better so far.

*)

type t=
  Chain of string*(string list)
 |Star of string*string
 |Maybe of string*string
 |Avoider of string*string*Avoider_label.t
 |Motionless of string*string
 |Disjunction_of_chains of string*(string list list);;

 let name=function
  Chain(s,_)->s
 |Star(s,_)->s
 |Maybe(s,_)->s
 |Avoider(s,_,_)->s
 |Motionless(s,_)->s
 |Disjunction_of_chains(s,_)->s;;

 let support=function
  Chain(_,l)->l
 |Star(_,s1)->[s1]
 |Maybe(_,s1)->[s1]
 |Avoider(_,s1,_)->[s1]
 |Motionless(_,s1)->[s1]
 |Disjunction_of_chains(_,ll)->List.flatten ll;;


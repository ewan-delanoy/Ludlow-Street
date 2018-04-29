(*

#use"Php_analizer/HRecognizer/abstractified_nonatomic_hrecognizer.ml";;

Unfortunate coupling with the type definition in nonatomic_hrecognizer module,
but I couldn't find anything better so far.

*)

type t=
  Leaf of string*Atomic_hrecognizer.t
 |Chain of string*(string list)
 |Ordered_disjunction of string*(string list)
 |Star of string*string
 |Maybe of string*string
 |Keyword_avoider of string*string
 |Motionless of string*string;;

 let name=function
  Leaf(s,_)->s
 |Chain(s,_)->s
 |Ordered_disjunction(s,_)->s
 |Star(s,_)->s
 |Maybe(s,_)->s
 |Keyword_avoider(s,_)->s
 |Motionless(s,_)->s;;

 let support=function
  Leaf(_,_)->[]
 |Chain(_,l)->l
 |Ordered_disjunction(_,l)->l
 |Star(_,s1)->[s1]
 |Maybe(_,s1)->[s1]
 |Keyword_avoider(_,s1)->[s1]
 |Motionless(_,s1)->[s1];;
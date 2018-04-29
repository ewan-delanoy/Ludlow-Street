(*

#use"Php_analizer/HRecognizer/abstractified_nonatomic_hrecognizer.ml";;

*)

type t=
  Leaf of string*Atomic_hrecognizer.t
 |Chain of string*(string list)
 |Ordered_disjunction of string*(string list)
 |Star of string*string
 |Maybe of string*string
 |Keyword_avoider of string*(string*(string list))
 |Motionless of string*(string list);;


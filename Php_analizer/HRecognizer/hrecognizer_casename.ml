(*

#use"Php_analizer/HRecognizer/hrecognizer_casename.ml";;

*)

type t=
  Leaf 
 |Chain 
 |Ordered_disjunction 
 |Star 
 |Maybe 
 |Avoider 
 |Motionless
 |Disjunction_of_chains;;


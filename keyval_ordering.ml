(*

#use"keyval_ordering.ml";;

*)


let ko=((fun x y->Total_ordering.product 
  Total_ordering.dictionary_order Total_ordering.standard x y): 
  (string * 'a) Total_ordering.t);;
(*

#use"keyval_ordering.ml";;

*)


let ko=((fun x y->Total_ordering.product 
  Dictionary_order.dictionary_order Total_ordering.standard x y): 
  (string * 'a) Total_ordering.t);;
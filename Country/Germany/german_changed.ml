
(* 


#use"Country/Germany/german_changed.ml";;



*)

let update r_changed changed=
  let r_chan=Ordered_string.safe_set r_changed
  and   chan=Ordered_string.safe_set changed in
  let whole=Ordered_string.teuzin r_chan chan in
  Ordered.forget_order whole;;


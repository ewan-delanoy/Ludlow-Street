(*

#use"industrial_separator.ml";;

Separators used in encoding of data types by strings.
The main key should not appear in any string inside the data types,
change it if necessary.

*)

let key="mpdykruvueaoqhkt";;

let d=3;;
let bound=
  let tens=Ennig.doyle (fun i->10) 1 d in
  (List.fold_left (fun u v->u*v ) 1 tens)-1;;

let counter=ref 0;;

exception Separator_overflow;;

let number_of_separators_used_so_far ()=(!counter);;

let new_separator ()=
  let i=(!counter)+1 in
  if i>bound
  then raise(Separator_overflow)
  else 
  counter:=i;
  key^(Cull_string.resize_from_right (string_of_int i) d '0');;
  

  
  
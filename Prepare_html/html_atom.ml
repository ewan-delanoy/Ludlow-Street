(*

#use"Prepare_html/html_atom.ml";;

*)


type t=
    Fixed of string
   |Changing of Html_variable_atom.t;;
  
let apply x l=match x with
 Fixed(s)->s
 |Changing(vaar)->Html_variable_atom.apply vaar l;;   
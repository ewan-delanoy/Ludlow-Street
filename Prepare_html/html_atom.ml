(*

#use"Prepare_html/html_atom.ml";;

*)


type t=
    Fixed of string
   |Changing of Html_variable_atom.t;;
  
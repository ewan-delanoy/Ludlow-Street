(*

#use"Prepare_html/html_line.ml";;

*)


type t=
    Usual of (Html_atom.t list)*bool
   |Tag_opener of string
   |Tag_closer of string;;
  
let is_pushable =function
   Usual(_,pushable)->pushable
   |Tag_opener(_)->true
   |Tag_closer(_)->true;;
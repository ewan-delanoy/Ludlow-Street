(*

#use"Prepare_html/html_line.ml";;

*)


type t=
    Usual of (Html_atom.t list)*bool
   |Opener of Html_tag.t
   |Closer of Html_tag.t;;
  
let weight =function
Usual(_,pushable)->0
|Opener(tag)->Html_tag.weight tag
|Closer(tag)->Html_tag.weight tag;;

let is_pushable =function
   Usual(_,pushable)->pushable
   |Opener(_)->true
   |Closer(_)->true;;
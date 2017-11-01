(*

#use"Prepare_html/html_tag.ml";;

*)


type t=Tag of string;;
  
let weight (Tag tag)=
    if tag="a"
    then 0
    else (String.length tag)+3;;
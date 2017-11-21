(*

#use"Prepare_html/Html_text_with_tags.ml";;

*)

type t=
    Leaf of string
    |Tagged of string*t
    |Concat of t list;;
    
let leaf text =Leaf text;;
let tagged tag_name text =Tagged(tag_name,text);;
let concat l=Concat l;;






(*

#use"Prepare_html/html_text_with_tags.ml";;

*)

type t=
     Leaf of string
    |Tagged of string*t
    |Concat of t list;;
    
let leaf text =Leaf text;;
let tagged tag_name text =Tagged(tag_name,text);;
   

     


let concat l=
    let temp1=Image.image (function 
        Concat(l2)->l2
        |x->[x]
    ) l in
    let temp2=List.flatten temp1 in
    Concat temp2;;



(*

#use"Prepare_html/html_parse_text_with_tags.ml";;

*)

let special_tags=
    [
      "br"  
    ];;

let zero_width_tags=
    [
        "html"
    ]    

let tag_width s=
    if List.mem s zero_width_tags
    then 0
    else String.length s;;    

(*

let rec main_iterator (graet,s,n,idx)=
    if idx>n
    then 
    
*)    
        




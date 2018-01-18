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

exception Unmatched_tag_opener of int;;

let rec main_iterator (graet,s,n,idx)=
    if idx>n
    then Html_hedgehog_pack.simplify_to_text graet
    else
    let opt1=Option.seek (fun j->
        (Strung.get s j)='<'
    )(Ennig.ennig idx n) in
    if opt1=None
    then let last_one=Html_hedgehog_pack.add_string_constant 
              (idx,n,Cull_string.interval s idx n) graet in
         Html_hedgehog_pack.simplify_to_text last_one     
    else 
    let i1=Option.unpack opt1 in
    let opt2=Option.seek (fun j->
        (Strung.get s j)='>'
    )(Ennig.ennig (i1+1) n) in
    if opt2=None
    then raise(Unmatched_tag_opener(i1))
    else
    let i2=Option.unpack opt2 in
    let temp1=Html_hedgehog_pack.add_string_constant 
    (idx,i1-1,Cull_string.interval s idx (i1-1)) graet in
    let temp2=Html_hedgehog_pack.add_tag 
    (i1,i2,Cull_string.interval s i1 i2) temp1 in
    main_iterator (temp2,s,n,i2+1) ;;   


    







  
        




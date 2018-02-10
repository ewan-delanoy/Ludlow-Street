(*

#use"Prepare_html/Tag_related/html_text_with_tags.ml";;

*)

type t=
     Leaf of string
    |Tagged of string*t
    |Concat of t list;;
    
let leaf text =Leaf text;;
let tagged tag_name text =Tagged(tag_name,text);;
   
let leaf_content=function
    Leaf(x)->Some(x)
    |_->None;;

let rec merge_leaves (graet,da_ober)=
    (* it is assumed that there are no more than two
       successive leaves. This will be the case if
       one concatenates admissible t values.
    *) 
    match da_ober with
    []->List.rev graet
    |a::peurrest->
       let opt_a=leaf_content a in
       if opt_a=None
       then merge_leaves(a::graet,peurrest)
       else 
       let inside_a=Option.unpack opt_a in
       (
         match peurrest with
         []->List.rev(a::graet)
         |b::peurrest2->  
            let opt_b=leaf_content b in
            if opt_b=None
            then merge_leaves(b::a::graet,peurrest2)
            else 
            let inside_b=Option.unpack opt_b in
            merge_leaves((Leaf (inside_a^inside_b))::graet,peurrest2)
       );;


let concat l=
    let temp1=Image.image (function 
        Concat(l2)->l2
        |x->[x]
    ) l in
    let temp2=List.flatten temp1 in
    let temp3=merge_leaves([],temp2) in
    if List.length temp3=1
    then List.hd temp3
    else Concat temp3;;



(*

#use"Prepare_html/Tag_related/html_hedgehog_pack.ml";;

*)

type t=P of Html_hedgehog.t list;;

let empty = P [];;

let add_constant (i,j,text) (P l_hedgehog)=
    match l_hedgehog with
    []->P([Html_hedgehog.from_constant (i,j,text)])
    |hedgehog1::peurrest->
        P((Html_hedgehog.add_constant (i,j,text) hedgehog1 )
           ::peurrest);;
      
let add_string_constant (i,j,s) pack=
    add_constant (i,j,Html_text_with_tags.leaf s) pack;;

let add_opening_tag (i,j,tag_name)  (P l_hedgehog)=
    match l_hedgehog with
    []->P([Html_hedgehog.from_opening_tag (i,j,tag_name)])
    |hedgehog1::peurrest->
        if Html_hedgehog.finished_part hedgehog1=None
        then P((Html_hedgehog.add_opening_tag (i,j,tag_name) hedgehog1 )
                ::peurrest)
        else P((Html_hedgehog.from_opening_tag (i,j,tag_name))::l_hedgehog);;    

exception Add_finished_hedgehog_unfinished_exn of Html_hedgehog.t;;
exception Add_finished_hedgehog_empty_exn;;


let add_finished_hedgehog hhog pack=
    if Html_hedgehog.unfinished_part hhog=[]
    then (
           match Html_hedgehog.finished_part hhog with
           None->raise(Add_finished_hedgehog_empty_exn)
           |Some(i,j,text)->add_constant (i,j,text) (P pack)
         )
    else raise(Add_finished_hedgehog_unfinished_exn(hhog));;

exception Unmatched_closing_tag of string;;
exception Cold_reception of string;;
exception Tag_mismatch of string*string;;

let add_closing_tag ((i:int),j,tag_name) (P l_hedgehog)=
    match l_hedgehog with
     []->raise(Unmatched_closing_tag(tag_name))
    |hedgehog1::peurrest->
    (
      match Html_hedgehog.unfinished_part hedgehog1 with
      []->raise(Cold_reception(tag_name))
      |(i2,j2,tag_name2,full_tag2)::peurrest2->
         if tag_name2<>tag_name 
         then raise(Tag_mismatch(tag_name,tag_name2))
         else 
        let new_hedgehog1=
             Html_hedgehog.close_latest_tag (i,j) hedgehog1 in
        if Html_hedgehog.unfinished_part new_hedgehog1=[]
        then  add_finished_hedgehog new_hedgehog1 peurrest
        else P(new_hedgehog1::peurrest)
    );;

let is_not_a_closable_tag t=
  (
      List.exists(
        Substring.begins_with t
      ) ["!DOCTYPE";"link "]
  )  
  ||
  (List.mem t ["br"]);;


let add_tag (i,j,tag_descr) hpack=
   if not(Substring.is_a_substring_located_at "/" tag_descr 2)
   then let t=Cull_string.interval tag_descr 2 (j-i) in
        if is_not_a_closable_tag t
        then add_string_constant (i,j,tag_descr) hpack
        else add_opening_tag (i,j,t) hpack
   else let t=Cull_string.interval tag_descr 3 (j-i) in
        add_closing_tag (i,j,t) hpack ;;

exception Cannot_simplify_multipack of Html_hedgehog.t list;;

let simplify_to_text (P l)=
        if (List.length l)<>1
        then raise(Cannot_simplify_multipack(l))
        else
        let hedgehog=List.hd l in
        Html_hedgehog.simplify_to_text hedgehog;;
    
        
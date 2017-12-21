(*

#use"Prepare_html/html_hedgehog_pack.ml";;

*)

type t=P of Html_hedgehog.t list;;

let add_constant (i,j,text) (P l_hedgehog)=
    match l_hedgehog with
    []->P([Html_hedgehog.from_constant (i,j,text)])
    |hedgehog1::peurrest->
        P((Html_hedgehog.add_constant (i,j,text) hedgehog1 )
           ::peurrest);;
      
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
      |(i2,j2,tag_name2)::peurrest2->
         if tag_name2<>tag_name 
         then raise(Tag_mismatch(tag_name,tag_name2))
         else 
        let new_hedgehog1=
             Html_hedgehog.close_latest_tag (i,j) hedgehog1 in
        if Html_hedgehog.unfinished_part new_hedgehog1=[]
        then  add_finished_hedgehog new_hedgehog1 peurrest
        else P(new_hedgehog1::peurrest)
    );;


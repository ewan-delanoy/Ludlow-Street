(*

#use"Prepare_html/Tag_related/html_hedgehog.ml";;

The fundamental object used in parsing tags inside a HTML string.
It has two parts, unfinished and finished. The unfinished part
keeps a list of the not yet closed opening tags encountered so far.

*)

type t=
    {
        unfinished : (int*int*string) list;
        finished   : (int*int*Html_text_with_tags.t) option;
    };;

let unfinished_part x=x.unfinished;;
let finished_part x=x.finished;;

let from_constant (i,j,text)=
        {
            unfinished = [];
            finished = Some(i,j,text)
        };;

let from_string_constant (i,j,s_text)=
     from_constant (i,j,Html_text_with_tags.leaf s_text);;

let from_opening_tag (i,j,tag_name)=
        {
            unfinished = [i,j,tag_name];
            finished = None
        };;    

let add_constant (i,j,text) hedgehog=
     let unfinished1=hedgehog.unfinished
     and finished1=hedgehog.finished in
     if finished1=None
     then {
            unfinished = unfinished1;
            finished = Some(i,j,text )
          }
     else let (old_i,_,old_constant)=Option.unpack finished1 in
          {
            unfinished = unfinished1;
            finished = Some(old_i,j,Html_text_with_tags.concat
                                    [old_constant;text])
          };;      

let add_string_constant (i,j,s) hedgehog=
    add_constant (i,j,Html_text_with_tags.leaf s) hedgehog;;

exception Forbidden_opening_tag_addition of string;;

let add_opening_tag (i,j,tag_name) hedgehog=
    if hedgehog.finished<>None
    then raise(Forbidden_opening_tag_addition(tag_name))
    else {
            unfinished = (i,j,tag_name)::hedgehog.unfinished;
            finished = None
          };;   

exception No_open_tag;;

let close_latest_tag (i1,j1) hedgehog= 
    match hedgehog.unfinished with
     []->raise(No_open_tag)
    |(i,j,tag_name)::peurrest->
       let center_part=(
        match  hedgehog.finished with
        None->Html_text_with_tags.leaf ""
        |Some(_,_,txt)->txt
       ) in          
       let new_achievement=
      Html_text_with_tags.tagged tag_name center_part in
      {
          unfinished = peurrest;
          finished = Some(i,j1,new_achievement);
      };;

exception Cannot_simplify_unfinished_hedgehog of (int*int*string) list;;
exception Nothing_to_simplify;;

let simplify_to_text hedgehog=
    let l=hedgehog.unfinished in
    if l<>[]
    then raise(Cannot_simplify_unfinished_hedgehog(l))
    else
    let opt=hedgehog.finished in
    if opt=None
    then raise(Nothing_to_simplify)
    else
    let (_,_,answer)=Option.unpack opt in
    answer;;

    
    
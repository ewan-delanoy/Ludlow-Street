(*

#use"Prepare_html/html_hedgehog.ml";;

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
        finished = Some(i,j,Html_text_with_tags.leaf text )
    };;

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
            finished = Some(i,j,Html_text_with_tags.leaf text )
          }
     else let (old_i,_,old_constant)=Option.unpack finished1 in
          {
            unfinished = unfinished1;
            finished = Some(old_i,j,Html_text_with_tags.concat
                                    [old_constant;
                                     Html_text_with_tags.leaf text])
          };;      



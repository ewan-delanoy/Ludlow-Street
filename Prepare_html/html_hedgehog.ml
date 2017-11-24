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





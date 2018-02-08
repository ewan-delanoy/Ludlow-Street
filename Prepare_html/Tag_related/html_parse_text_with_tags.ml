(*

#use"Prepare_html/Tag_related/html_parse_text_with_tags.ml";;

*)


exception Unmatched_tag_opener of int;;

let main_pusher (s,n)=
    let rec tempf=(function (ndr,walker)->
    let (graet,idx)=walker in
    if idx>n
    then (End_reached_in_recursive_cycle.Reached(1),walker)
         (* Html_hedgehog_pack.simplify_to_text graet *)
    else
    
    let opt1=Option.seek (fun j->
        (Strung.get s j)='<'
    )(Ennig.ennig idx n) in
    if opt1=None
    then (End_reached_in_recursive_cycle.Reached(2),walker)
        (* 
        let last_one=Html_hedgehog_pack.add_string_constant 
              (idx,n,Cull_string.interval s idx n) graet in
         Html_hedgehog_pack.simplify_to_text last_one     
        *) 
    else 
    let i1=Option.unpack opt1 in
    let opt2=Option.seek (fun j->
        (Strung.get s j)='>'
    )(Ennig.ennig (i1+1) n) in
    if opt2=None
    then raise(Unmatched_tag_opener(i1))
    else
    let i2=Option.unpack opt2 in
    let temp1=(
      if i1>idx
      then Html_hedgehog_pack.add_string_constant 
            (idx,i1-1,Cull_string.interval s idx (i1-1)) graet
      else graet      
     ) in
    let temp2=Html_hedgehog_pack.add_tag 
    (i1,i2,Cull_string.interval s i1 i2) temp1 in
    (End_reached_in_recursive_cycle.Not_reached,(temp2,i2+1)) 
    ) in
    tempf;;   



exception Bad_exit_index;;

let rec main_iterator (s,n) wrapped_walker=match fst(wrapped_walker) with
End_reached_in_recursive_cycle.Reached(i)-> 
              let (graet,idx)=snd(wrapped_walker) in
              if i=1
              then Html_hedgehog_pack.simplify_to_text graet
              else 
              if i=2
              then let last_one=Html_hedgehog_pack.add_string_constant 
                   (idx,n,Cull_string.interval s idx n) graet in
                   Html_hedgehog_pack.simplify_to_text last_one 
              else raise(Bad_exit_index)     
| End_reached_in_recursive_cycle.Not_reached->
    main_iterator (s,n) (main_pusher (s,n) wrapped_walker);;

let main_initializer s=
    (End_reached_in_recursive_cycle.Not_reached,
    (Html_hedgehog_pack.empty,1));;

let parse s=main_iterator (s,String.length s) (main_initializer s);;
    
(*

parse "<!DOCTYPE html>\n<html>\n        <head>abc </head></html>";;

*)






  
        




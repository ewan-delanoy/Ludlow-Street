(*

#use"Php_analizer/Great_Replacement/ivy_aware_decomposition.ml";;

*)

let reference_for_dubious_cases=ref [];;

let common_computation kind s i0 i2=
    let opt3=After.after_whites s i2 in
    if opt3=None then None else 
    let i3=Option.unpack opt3 in
    if (Strung.get s i3)<>'{'  
    then  let j4=Substring.leftmost_index_of_in_from ";" s i3 in
          if j4<0 then None else 
          let dubious_content = Cull_string.interval s i3 (j4-1) in
          let _=(
            reference_for_dubious_cases:=
              dubious_content::(!reference_for_dubious_cases)
          ) in
          Some(
            (Ivy_aware_item.make 
              kind
              (Cull_string.interval s i0 (i3-1))
              dubious_content
              (Cull_string.interval s j4 j4)),
             j4+1 
          )
    else
    let i4=After.after_closing_character ('{','}')  s (i3+1,1) in 
    Some(
      (Ivy_aware_item.make 
        kind
        (Cull_string.interval s i0 i3)
        (Cull_string.interval s (i3+1) (i4-2))
        (Cull_string.interval s (i4-1) (i4-1))),
       i4 
    );;

let next_item_in_prepared_short_case kind s i0=    
    common_computation kind s i0 (i0+4);;

let next_item_in_prepared_long_case kind s i0=
    let i1=Substring.leftmost_index_of_in_from "(" s i0 in
    if i1<0 then None else 
    let i2=After.after_closing_character ('(',')')  s (i1+1,1) in
    common_computation kind s i0 i2;;

let naive_next_item_in_prepared_case kind s i0=
   if kind=Ivy_aware_kind.elsie 
   then next_item_in_prepared_short_case kind s i0
   else next_item_in_prepared_long_case kind s i0;;

exception Dubious_case_exn;;

let next_item_in_prepared_case kind s i0=
     let _=(reference_for_dubious_cases:=[]) in
     let answer=naive_next_item_in_prepared_case kind s i0 in
     if (!reference_for_dubious_cases)<>[]
     then raise(Dubious_case_exn)
     else answer;;

let reference_for_subtle_case=ref("",0);;
exception Subtle_case;;

let on_string s=
    let n=String.length s in
    let rec tempf=(
      fun (graet,idx)->
        if idx>n
        then List.rev graet
        else 
        match Unshadowed_appearance.next 
         s ["if";"elseif";"else"] idx with
        None->
            let last_item=
               Ivy_aware_item.non_ivy 
                 (Cull_string.interval s idx n) in
               List.rev (last_item::graet)    
        |Some(jdx)->
            let opt=Option.seek(
                fun kwd->
                 Substring.is_a_substring_located_at kwd s jdx
            ) ["if";"elseif";"else"] in
            if opt=None 
            then (reference_for_subtle_case:=(s,jdx);raise(Subtle_case)) 
            else
            let kwd0=Option.unpack opt in
            let kind=Ivy_aware_kind.of_string kwd0 in
            let opt2=next_item_in_prepared_case kind s jdx in
            if opt2=None 
            then (reference_for_subtle_case:=(s,jdx);raise(Subtle_case)) 
            else
            let (item2,new_idx)=Option.unpack opt2 in
            if jdx=idx
            then tempf(item2::graet, new_idx) 
            else let item1=
                  Ivy_aware_item.non_ivy 
                   (Cull_string.interval s idx (jdx-1)) in
                 tempf(item2::item1::graet, new_idx)
    ) in
    tempf([],1);; 
            
(*

let s="123 if(uvw)  {xyz} else {ikj} 456";;
let l=on_string s;;
let s1=String.concat "" 
(image Ivy_aware_item.full_content l);;
let check=(s1=s);;





*) 





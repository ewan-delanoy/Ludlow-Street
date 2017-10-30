(*

#use"Php_analizer/Great_Replacement/nspc_expand_inclusion.ml";;

*)

let cull_php_enclosers old_s=
  let s=Cull_string.trim_spaces old_s in
  let n=String.length s in
  let i1=(if Substring.begins_with s "<?php" then 6 else 1)
  and j1=(if Substring.ends_with s "?>" then (n-2) else n) in
  Cull_string.interval s i1 j1;;

exception Absent_inclusion_line of string;;
exception Double_inclusion of string;;

let string_in_string
  (comment_before,comment_after)
  l_rep
  inserted_text inclusion_line container_text=
    let cleaned_inserted_text= 
      "\n"^comment_before^"\n"^
      (cull_php_enclosers(Nspc_standardize.string 
      (Replace_inside.replace_several_inside_string l_rep inserted_text)))^
      "\n"^comment_after^"\n" in
    let temp1=Lines_in_string.core container_text in
    let temp2=List.filter (fun (j,line)->line=inclusion_line) temp1 in
    let d=List.length temp2 in
    if d<1 then raise(Absent_inclusion_line(inclusion_line)) else
    if d>1 then raise(Double_inclusion(inclusion_line)) else
    let j1=fst(List.hd temp2) in
    let temp2=Image.image(
      fun (j,line)->
        if  j=j1
        then cleaned_inserted_text
        else line
    ) temp1 in
    String.concat "\n" temp2;;

let file_in_file 
  (comment_before,comment_after)
  l_rep
  inserted_file inclusion_line container_file=
   let inserted_text=Io.read_whole_file inserted_file
   and container_text=Io.read_whole_file container_file in
   let new_text=string_in_string
   (comment_before,comment_after)
   l_rep
   inserted_text inclusion_line container_text in
   Io.overwrite_with container_file new_text;;


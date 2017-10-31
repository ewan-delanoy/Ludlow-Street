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
exception No_namespace_in_container;;

let prepare_inserted_text
    (comment_before,comment_after)
    (name_outside,name_inside)
    inserted_text=
     if name_outside<>name_inside
     then "}\n"^comment_before^"\n"^
          (cull_php_enclosers(inserted_text))^
           "\n"^comment_after^"\nnamespace "^name_outside^" {\n"
     else "\n"^comment_before^"\n"^
          (cull_php_enclosers(Nspc_remove.r(inserted_text)))^ 
          "\n"^comment_after^"\n";;


let string_in_string
  (comment_before,comment_after)
  l_rep
  inserted_text inclusion_line container_text=
    let temp1=Lines_in_string.core container_text in
    let temp2=List.filter (fun (j,line)->line=inclusion_line) temp1 in
    let d=List.length temp2 in
    if d<1 then raise(Absent_inclusion_line(inclusion_line)) else
    if d>1 then raise(Double_inclusion(inclusion_line)) else
    let j1=fst(List.hd temp2) in
    let (temp3,_,_)=Three_parts.select_center_element_and_reverse_left
            (fun (j,_)->j=j1) temp1 in
    let opt1=Option.find_and_stop(fun (j,line)->
        match Nspc_detect.extract_namespace_name line with
        None->None |Some(nahme,_)->Some(nahme)
    ) temp3 in
    if opt1=None then raise(No_namespace_in_container) else
    let name_outside=Option.unpack opt1 in
    let (temp5,name_inside)=Nspc_standardize.string_and_remember_name 
    (Replace_inside.replace_several_inside_string l_rep inserted_text) in
    let final_inserted_text=
      prepare_inserted_text
      (comment_before,comment_after)
      (name_outside,name_inside)
      temp5 in
    let temp4=Image.image(
      fun (j,line)->
        if  j=j1
        then final_inserted_text
        else line
    ) temp1 in
    String.concat "\n" temp4;;



(*

string_in_string ("above","below") []
"<?php \nnamespace A {\n bcd}"
"inc;"
("<?php \nnamespace E {\n fg} \nnamespace H {\npq;\ninc;\njk} "^
"\nnamespace L {\nmn} ");;

string_in_string ("above","below") []
"<?php \nnamespace H {\n bcd}"
"inc;"
("<?php \nnamespace E {\n fg} \nnamespace H {\npq;\ninc;\njk} "^
"\nnamespace L {\nmn} ");;

*)

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


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
    inserted_text paragraph_containing_uses
    nspc_is_unique=
     if name_outside=name_inside && nspc_is_unique
     then "\n"^comment_before^"\n"^
          (cull_php_enclosers(Nspc_remove.r(inserted_text)))^ 
          "\n"^comment_after^"\n"
     else let uses=List.filter (
              fun line->
              (Clean_duplicate_uses.extract_used_item line)<>None
          ) paragraph_containing_uses in
          let all_uses=String.concat "\n" uses in
          "}\n"^comment_before^"\n\n"^
          (cull_php_enclosers(inserted_text))^
          "\n"^comment_after^"\nnamespace "^name_outside^" {\n"^
          all_uses^"\n\n";;


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
    let opt2=Option.seek(fun (j,line)->
        Nspc_detect.test_for_namespace_line line
    ) temp3 in
    if opt2=None then raise(No_namespace_in_container) else
    let (j2,line2)=Option.unpack opt2 in
    let (name_outside,_)=Option.unpack(Nspc_detect.extract_namespace_name line2) in
    let (temp7,_,_)=Three_parts.select_center_element_and_reverse_left
        (fun (j,_)->j=j2) temp3 in
    let paragraph_containing_uses=Image.image snd temp7 in 
    let replaced_text=Replace_inside.replace_several_inside_string l_rep inserted_text in   
    let dec_form_inside=Nspc_split.decompose replaced_text in
    let new_dec_form_inside=Nspc_standardize.decomposed_form dec_form_inside in
    let temp5=Nspc_split.recompose new_dec_form_inside in
    let nspc_parts_inside=Nspc_decomposed_form.namespaced_parts new_dec_form_inside in
    let (first_nspc_line,_,_,_)=List.hd(nspc_parts_inside) in
    let (name_inside,_)=Option.unpack(Nspc_detect.extract_namespace_name first_nspc_line) in
    let nspc_is_unique=((List.length nspc_parts_inside)=1) in
    let preparatory_inserted_text=
      prepare_inserted_text
      (comment_before,comment_after)
      (name_outside,name_inside)
      temp5 paragraph_containing_uses 
      nspc_is_unique in
    let temp4=Image.image(
      fun (j,line)->
        if  j=j1
        then preparatory_inserted_text
        else line
    ) temp1 in
    let temp6=String.concat "\n" temp4 in
    Clean_duplicate_uses.in_string(
    Nspc_reaggregate.string(
      Clean_empty_namespaces.in_string temp6));;



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

string_in_string ("above","below") []
"<?php \nnamespace A {\n bcd}"
"inc;"
("<?php \nnamespace H {\nuse U;\n use V;\nxyz;\nuse W;\n\npq;\ninc;\njk} "^
"\nnamespace L {\nmn} ");;

string_in_string ("above","below") []
"<?php \nnamespace A ;\n bcd \nnamespace B ;\n efg"
"inc;"
("<?php \nnamespace H {\nuse U;\n use V;\nxyz;\nuse W;\n\npq;\ninc;\njk} "^
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


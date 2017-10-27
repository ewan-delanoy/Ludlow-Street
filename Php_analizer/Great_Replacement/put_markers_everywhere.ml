(*

#use"Php_analizer/Great_Replacement/put_markers_everywhere.ml";;

Works only on a previously standardized PHP text.
See the standardize function in the namespacize module.

*)

let rec low_level_helper
  (mark_count,line_count,idx_start,idx,s,n,accu)=
    if idx>n
    then String.concat "" (List.rev accu)
    else 
    let c=Strung.get s idx in
    if c='\n'
    then (
           if Substring.is_a_substring_located_at ";" s (idx-1)
           then let marker_line=
                 "marker_here("^(string_of_int(mark_count+1))^
                 ","^(string_of_int (line_count+2))^");\n" in
                let elt=
                 (Cull_string.interval s idx_start idx)^marker_line in
                 low_level_helper(mark_count+1,line_count+2,idx+1,idx+1,s,n,elt::accu)
           else  low_level_helper(mark_count,line_count+1,idx_start,idx+1,s,n,accu)     
         )
    else
    if c='{'
    then let j=After.after_closing_character ('{','}') s (idx,0) in
         let d=
          List.length(List.filter (fun t->Strung.get s t='\n') 
          (Ennig.ennig idx j)) in
          low_level_helper(mark_count,line_count+d,idx_start,j,s,n,accu)
    else  low_level_helper(mark_count,line_count,idx_start,idx+1,s,n,accu);;

let in_namespace s=low_level_helper(0,0,1,1,s,String.length s,[]);;  

let rec high_level_helper (graet,da_ober)=
  match da_ober with
  []->Cnspc.reconstruct_string (List.rev graet)
  |(dec_content,nspc_name,nspc_content)::peurrest->
      let marked_content=in_namespace nspc_content in
      let temp1=Cnspc.rewrite_item (dec_content,nspc_name,marked_content) in
      high_level_helper (temp1::graet,peurrest)
  ;;    


let in_string s=
   let temp1=Cnspc.decompose s in
   let temp2=high_level_helper ([],temp1) in
   Marker.adjust_all_markers temp2;; 

let in_file ap=
    let old_text=Io.read_whole_file ap in
    let new_text=in_string old_text in
    Io.erase_file_and_fill_it_with_string ap new_text;;

(*

let rewrite_item
  (dec_content,nspc_name,nspc_content)=
    let dec_component=(
      if dec_content=""
      then ""
      else (linebreaks padding_before_declaration)^
           "declare("^dec_content^");"^
           (linebreaks padding_after_declaration)
    ) in
    let trimmed_content=Cull_string.trim_spaces nspc_content in
    if test_for_lonely_marker trimmed_content
    then None
    else 
    Some(
       dec_component^"namespace "^nspc_name^" {"^
       (linebreaks padding_at_start_of_namespace)^
       trimmed_content^
       (linebreaks padding_at_end_of_namespace)
       ^"}"^
       (linebreaks padding_after_namespace)
    );;
    
let rewrite_string s=
    let temp1=decompose s in
    let temp2=Option.filter_and_unpack rewrite_item temp1 in
    "<?php"^
    (linebreaks padding_after_php_open_tag)
    ^(String.concat 
     (linebreaks padding_between_namespaces)
    temp2);;

*)

(*
let s="ab\ncde;\nfg\n\n{hi\njkl;\nmn\n\n}op\nqr;\nst;\n\n";;
print_string s;;
print_string (pme s);;
*)

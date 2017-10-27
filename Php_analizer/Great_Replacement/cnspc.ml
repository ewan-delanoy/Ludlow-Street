(*

#use"Php_analizer/Great_Replacement/cnspc.ml";;

*)

(*
cnspc stands for : commented namespace 
Arguments to cnspc functions are assumed to be already standardized
*)

exception Individual_cut_exn of int*int;;

let individual_cut s j=
   if (j>(String.length s)) then None else
   let opt1=After.after_whites_and_comments s j in
   if opt1=None then None else
   let j1=Option.unpack opt1 in
   let (nspc_name,nspc_idx,left_idx,right_idx,_,dec_content)=
      Namespacize.decompose_from s j1 in
   if nspc_idx=0
   then raise(Individual_cut_exn(j,j1)) 
   else let j2=After.after_closing_character ('{','}') s (right_idx,0) in
        Some((dec_content,nspc_name,Cull_string.interval s (right_idx+1) (j2-2)),j2+1);; 

(*

individual_cut "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 7;;
individual_cut "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 22;;
individual_cut "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 24;;
individual_cut "<?php  declare(abc); namespace def {gh}" 8;;

*)

let rec decomposition_helper s (graet,j)=
   match individual_cut s j with
     None->List.rev(graet)
     |Some(result,new_j)->
       decomposition_helper s (result::graet,new_j);;

exception Dh_debug_exn;;

let rec dh_debug s (graet,j)=
  try
  (match individual_cut s j with
    None->raise(Dh_debug_exn)
    |Some(result,new_j)->
      dh_debug s ((j,result)::graet,new_j)
  ) with
  Individual_cut_exn(x,y)->
     (x,y,graet)
  ;;

let decompose_from s j=
  decomposition_helper s ([],j);; 

exception Absent_php_open_tag;;
exception Empty_text;;  

let decompose s=
    if not(Substring.begins_with s "<?php") 
    then raise(Absent_php_open_tag)
    else
    let opt1=After.after_whites_and_comments s 6 in
    if opt1=None 
    then raise(Empty_text) 
    else let i1=Option.unpack opt1 in
         decompose_from s i1;;

let test_for_lonely_marker s=
    if (not(Substring.begins_with s "marker_here("))
    then false
    else (Substring.leftmost_index_of_in ";" s)=(String.length s);;  

let padding_before_declaration=2;;
let padding_after_declaration=2;;
let padding_at_start_of_namespace=3;;
let padding_at_end_of_namespace=3;;
let padding_after_namespace=2;;
let padding_after_php_open_tag=2;;
let padding_between_namespaces=1;;

let linebreaks j=String.make j '\n';;

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

let rewrite_file ap=
    let old_text=Io.read_whole_file ap in
    let new_text=rewrite_string old_text in
    Io.erase_file_and_fill_it_with_string ap new_text;;

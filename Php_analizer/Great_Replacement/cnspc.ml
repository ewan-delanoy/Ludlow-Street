(*

#use"Php_analizer/Great_Replacement/cnspc.ml";;

*)

(*
cnspc stands for : commented namespace 
Arguments to cnspc functions are assumed to be already standardized
*)

exception Individual_cut_exn of int;;

let individual_cut s j=
   if (j>(String.length s)) then None else
   let opt1=Namespacize.after_whites_and_comments s j in
   if opt1=None then None else
   let j1=Option.unpack opt1 in
   let (nspc_name,nspc_idx,left_idx,right_idx,_,dec_content)=
      Namespacize.decompose_from s j1 in
   if nspc_idx=0
   then raise(Individual_cut_exn(j1)) 
   else let j2=Namespacize.after_closing_character ('{','}') s (right_idx,0) in
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

let decompose_from s j=
  decomposition_helper s ([],j);; 

exception Absent_php_open_tag;;
exception Empty_text;;  

let decompose s=
    if not(Substring.begins_with s "<?php") 
    then raise(Absent_php_open_tag)
    else
    let opt1=Namespacize.after_whites_and_comments s 6 in
    if opt1=None 
    then raise(Empty_text) 
    else let i1=Option.unpack opt1 in
         decompose_from s i1;;

let test_for_lonely_marker s=
    if (not(Substring.begins_with s "marker_here("))
    then false
    else (Substring.leftmost_index_of_in ";" s)=(String.length s);;

let test_for_first_marker s=
      let opt1=Namespacize.after_whites_and_comments s 1 in
      if opt1=None then false else
      let i1=Option.unpack opt1 in
      Substring.is_a_substring_located_at "marker_here(" s i1;;    

let rewrite_item
  (dec_content,nspc_name,nspc_content)=
    let dec_component=(
      if dec_content=""
      then ""
      else "\n\ndeclare("^dec_content^");\n\n"
    ) in
    let trimmed_content=Cull_string.trim_spaces nspc_content in
    if test_for_lonely_marker trimmed_content
    then None
    else 
    let new_nspc_content=(
      if test_for_first_marker trimmed_content
      then trimmed_content
      else "marker_here(0,0);\n"^trimmed_content
    )  in
    Some(
       dec_component^"namespace "^nspc_name^" {\n\n\n"^
       new_nspc_content^"\n\n\n}\n\n"
    );;
    
let rewrite_string s=
    let temp1=decompose s in
    let temp2=Option.filter_and_unpack rewrite_item temp1 in
    "<?php\n\n"^(String.concat "\n" temp2);;

let rewrite_file ap=
    let old_text=Io.read_whole_file ap in
    let new_text=rewrite_string old_text in
    (Io.erase_file_and_fill_it_with_string ap new_text;
     Marker.color_all_markers ap);;

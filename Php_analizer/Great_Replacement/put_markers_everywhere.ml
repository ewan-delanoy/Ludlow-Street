(*

#use"Php_analizer/Great_Replacement/put_markers_everywhere.ml";;

Works only on a previously standardized PHP text.
See the standardize function in the namespacize module.

*)

let rec low_level_helper
  (mark_count,line_count,idx_start,idx,s,n,accu)=
    if idx>n
    then let elt=(Cull_string.interval s idx_start n) in
         String.concat "" (List.rev (elt::accu))
    else 
    if Substring.is_a_substring_located_at "/*" s idx
    then let j=Substring.leftmost_index_of_in_from "*/" s (idx+2) in
         let d=Lines_in_string.number_of_lines_in_char_interval s idx j in
         low_level_helper(mark_count,line_count+d,idx_start,j+2,s,n,accu)
    else 
    if Substring.is_a_substring_located_at "//" s idx
    then let j=Substring.leftmost_index_of_in_from "\n" s (idx+2) in
         low_level_helper(mark_count,line_count+1,idx_start,j+1,s,n,accu)
    else 
    if (Substring.is_a_substring_located_at "<<<EOF\n" s idx)
       ||
       (Substring.is_a_substring_located_at "<<<'EOF'\n" s idx) 
    then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (idx+7) in
         let d=Lines_in_string.number_of_lines_in_char_interval s idx (j+5) in
         low_level_helper(mark_count,line_count+d,idx_start,j+6,s,n,accu)
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
         let d=Lines_in_string.number_of_lines_in_char_interval s idx j in
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
    Io.overwrite_with ap new_text;;


(*
let s="ab\ncde;\nfg\n\n{hi\njkl;\nmn\n\n}op\nqr;\nst;\n\n";;
print_string s;;
print_string (pme s);;
*)

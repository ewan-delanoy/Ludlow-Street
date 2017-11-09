(*

#use"Php_analizer/Great_Replacement/put_markers_everywhere.ml";;

Works only on a previously standardized PHP text.
Creates a new text identical to the initial one except for
some markers inserted on single lines.

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
    let opt=After.after_classlike_block_with_linebreak s idx in
    if opt<>None
    then let jdx=Option.unpack opt in
         let d=Lines_in_string.number_of_lines_in_char_interval s idx (jdx-1) in
         let marker_line=Marker.from_numbers(mark_count+1)(line_count+d) in
          let elt=
            (Cull_string.interval s idx_start (jdx-1))^marker_line^"\n" in
            low_level_helper(mark_count+1,line_count+d+1,jdx,jdx,s,n,elt::accu)
    else
    let c=Strung.get s idx in
    if c='\n'
    then (
           if Substring.is_a_substring_located_at ";" s (idx-1)
           then let marker_line=Marker.from_numbers(mark_count+1)(line_count+1) in
                let elt=
                 (Cull_string.interval s idx_start idx)^marker_line^"\n" in
                 low_level_helper(mark_count+1,line_count+2,idx+1,idx+1,s,n,elt::accu)
           else  low_level_helper(mark_count,line_count+1,idx_start,idx+1,s,n,accu)     
         )
    else
    if c='{'
    then let j=After.after_closing_character ('{','}') s (idx,0) in
         let d=Lines_in_string.number_of_lines_in_char_interval s idx (j-1) in
          low_level_helper(mark_count,line_count+d,idx_start,j,s,n,accu)
    else  low_level_helper(mark_count,line_count,idx_start,idx+1,s,n,accu);;

let in_namespace s=low_level_helper(0,1,1,1,s,String.length s,[]);;  

(*

in_namespace "\nhaag;\nclass {u\nv\nw}diamond;\nxy";;

*)

exception Marking_on_nonstandard_text;;

let in_string s=
  let dec_form=Nspc_split.decompose s in 
  if Nspc_decomposed_form.namespacable dec_form<>None
  then raise(Marking_on_nonstandard_text)
  else 
  let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form
  and items=Nspc_decomposed_form.namespaced_parts dec_form in
  let new_items=Image.image(
      fun (a,b,c,d)->(a,in_namespace b,c,d)
   ) items in
   let new_dec_form=Nspc_decomposed_form.make 
      before_namespaces None new_items in
   let temp2=Nspc_split.recompose new_dec_form in
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

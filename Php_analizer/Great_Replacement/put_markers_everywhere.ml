(*

#use"Php_analizer/Great_Replacement/put_markers_everywhere.ml";;

Works only on a previously standardize PHP text.
See the standardize function in the namespacize module.

*)

let rec main_helper
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
                 main_helper(mark_count+1,line_count+2,idx+1,idx+1,s,n,elt::accu)
           else  main_helper(mark_count,line_count+1,idx_start,idx+1,s,n,accu)     
         )
    else
    if c='{'
    then let j=After.after_closing_character ('{','}') s (idx,0) in
         let d=
          List.length(List.filter (fun t->Strung.get s t='\n') 
          (Ennig.ennig idx j)) in
         main_helper(mark_count,line_count+d,idx_start,j,s,n,accu)
    else main_helper(mark_count,line_count,idx_start,idx+1,s,n,accu);;
    
let in_string s=main_helper(0,0,1,1,s,String.length s,[]);;   
let in_file ap=
    let old_text=Io.read_whole_file ap in
    let new_text=in_string old_text in
    Io.erase_file_and_fill_it_with_string ap new_text;;

(*
let s="ab\ncde;\nfg\n\n{hi\njkl;\nmn\n\n}op\nqr;\nst;\n\n";;
print_string s;;
print_string (pme s);;
*)

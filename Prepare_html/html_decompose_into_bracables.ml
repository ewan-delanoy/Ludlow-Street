(*

#use"Prepare_html/html_decompose_into_bracables.ml";;

*)

exception Unknown_key of string;;

let assoc_in_brcb_table x l=
     try List.assoc x l with
     _->raise(Unknown_key(x));;

let rec main_helper (brcb_table,s,n,graet,idx_start,idx)=
    if idx>n
    then List.rev graet
    else 
    if Substring.is_a_substring_located_at "{{" s idx
    then let jdx=Substring.leftmost_index_of_in_from "}}" s (idx+2) in
         let s_elt2=Cull_string.interval s (idx+2) (jdx-1) in
         let elt2=assoc_in_brcb_table s_elt2 brcb_table in
         if idx=idx_start
         then main_helper (brcb_table,s,n,elt2::graet,jdx,jdx)
         else 
         let s_elt1=Cull_string.interval s idx_start (idx-1) in
         let elt1=Html_bracable.constant s_elt1 in
         main_helper (brcb_table,s,n,elt2::elt1::graet,jdx+2,jdx+2)
    else main_helper (brcb_table,s,n,graet,idx_start,idx+1);;

let dec brcb_table s= 
    main_helper (brcb_table,s,String.length s,[],1,1);;     

(*    
let table1= [
   "1",Html_bracable.constant "one";
   "2",Html_bracable.constant "two"
] ;;   

dec table1 "abc{{1}}de{{2}}fg hi{{1}}";;
*)
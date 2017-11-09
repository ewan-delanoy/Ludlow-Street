(*

#use"Php_analizer/Great_Replacement/clean_empty_namespaces.ml";;

*)

let test_for_line s=
    (Substring.begins_with s "//")
    ||
    (Clean_duplicate_uses.extract_used_item s)<>None
    ||
    (Cull_string.trim_spaces s ="");;
  
(*

test_for_line "   \r\t   ";;

*)

let test_for_text text=
    let temp1=Str.split (Str.regexp_string "\n") text in
    List.for_all test_for_line temp1;;

let rec main_helper 
  (graet,nspc_line,nspc_content,offset,after_nspc,da_ober)=
  match da_ober with
  []->List.rev((nspc_line,nspc_content,offset,after_nspc)::graet)
  |(nspc_line2,nspc_content2,offset2,after_nspc2)::peurrest->
    if test_for_text nspc_content2
    then main_helper 
         (graet,nspc_line,nspc_content,offset,after_nspc^"\n"^after_nspc2,peurrest) 
    else let graet2=(nspc_line,nspc_content,offset,after_nspc)::graet in
         main_helper
         (graet2,nspc_line2,nspc_content2,offset2,after_nspc2,peurrest)
    ;;    

let in_list=function
   []->[]
   |(nspc_line,nspc_content,offset,after_nspc)::peurrest->
   main_helper 
   ([],nspc_line,nspc_content,offset,after_nspc,peurrest);;

exception On_nonstandard_text;;

let in_decomposed_form dec_form=
    let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form in
    match  Nspc_decomposed_form.namespacable dec_form with
    Some(text)->raise(On_nonstandard_text)
    |None->
    let items=Nspc_decomposed_form.namespaced_parts dec_form in
    let good_items=in_list items in
    Nspc_decomposed_form.make before_namespaces None good_items;; 

 
let in_string s=
        let dec_form=Nspc_split.decompose s in
        let new_dec_form=in_decomposed_form dec_form in
        Nspc_split.recompose new_dec_form;;

(*

in_string "<?php \nnamespace A{\nB}C\nnamespace D{\n}E\nnamespace F{\nG}\nH";;

*)

let in_file ap=
    let old_text=Io.read_whole_file ap in
    let new_text=in_string old_text in
    Io.overwrite_with ap new_text;;



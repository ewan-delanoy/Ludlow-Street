(*

#use"Php_analizer/Great_Replacement/nspc_standardize.ml";;

*)

let insert_end_padding text=
    if Substring.ends_with text "\n\n"
    then text
    else text^"\n\n";;

let namespaced_item item=
    let (nspc_line,nspc_content,offset,after_nspc)=item in
     if offset=""
     then let new_nspc_line=Replace_inside.replace_inside_string
               (";","{") nspc_line in
          (new_nspc_line,insert_end_padding nspc_content,"}",after_nspc) 
     else item;;    

let decomposed_form dec_form=
    let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form in
    match  Nspc_decomposed_form.namespacable dec_form with
    Some(text)->
                 Nspc_decomposed_form.make before_namespaces
                   None ["namespace {",
                         insert_end_padding text,
                         "}",""]
    |None->
    let items=Nspc_decomposed_form.namespaced_parts dec_form in
    let new_items=Image.image namespaced_item items in
     Nspc_decomposed_form.make before_namespaces None new_items;; 

let string s=
    let dec_form=Nspc_split.decompose s in
    let new_dec_form=decomposed_form dec_form in
    Nspc_split.recompose new_dec_form;;

let file fn=
     let old_text=Io.read_whole_file fn in
     let new_text=string old_text in
     Io.overwrite_with fn new_text;;


(*

string "<?php \na\nb\nc\n namespace uvw ;\ndef\n hi ;\n jk";;
string "<?php \na\nb\nc\n numespace uvw ;\ndef\n hi ;\n jk";;
string "<?php \na\nb\nc\n declare(def);\n jk";;

*)





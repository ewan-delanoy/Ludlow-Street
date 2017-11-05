(*

#use"Php_analizer/Great_Replacement/nspc_reaggregate.ml";;

To be used only on standardized texts.

*)



exception On_nonstandard_text;;

let decomposed_form dec_form=
    let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form in
    if (Nspc_decomposed_form.namespacable dec_form)<>None 
    then raise(On_nonstandard_text)
    else 
    let items=Nspc_decomposed_form.namespaced_parts dec_form in
    let temp1=Image.image (fun item->
       let (nspc_line,_,_,_)=item in
       let opt=Nspc_detect.extract_namespace_name nspc_line in
       let (nspc_name,_)=Option.unpack opt in
       (nspc_name,item)
    ) items in
    let temp2=Prepared.partition fst temp1 in
    let new_items=Image.image(
        fun l->
          if List.length l=1 then snd(List.hd l) else
          let (common_name,_)=List.hd l in
          let ttemp4=Image.image (
            fun (_,(nspc_line,nspc_content,nspc_offset,after_nspc))->
                nspc_content^"\n"^after_nspc
          ) l in
          let ttemp5=String.concat "\n" ttemp4 in
          ("namespace "^common_name^" {",ttemp5,"}","")
    ) temp2 in
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

let z1=
     "<?php \na\nb\n"^
     "namespace A{\n11;\nwz;}\nkij;\n"^
     "namespace B{\n22;\nwz;}\nkij;\n"^
     "namespace B{\n33;\nwz;}\nkij;\n"^
     "namespace C{\n44;\nwz;}\nkij;\n"^
     "namespace C{\n55;\nwz;}\nkij;\n"^
     "namespace C{\n44;\nwz;}\nkij;\n"^
     "namespace D{\n55;\nwz;}\nkij;\n";;

string z1;;

*)





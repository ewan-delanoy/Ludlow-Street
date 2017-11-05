(*

#use"Php_analizer/Great_Replacement/nspc_remove.ml";;

Removes namespace declaration if it exists and is unique.
Used to avoid closing and reopening the same namespace when
expanding an inclusion.

*)

exception Nonunique_namespace;;
exception Marking_on_nonstandard_text;;


let r s=

    let dec_form=Nspc_split.decompose s in 
    if Nspc_decomposed_form.namespacable dec_form<>None
    then raise(Marking_on_nonstandard_text)
    else 
    let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form
    and items=Nspc_decomposed_form.namespaced_parts dec_form in
    if List.length(items)<>1
    then raise(Nonunique_namespace)   
    else 
    let (nspc_line,nspc_content,_,after_nspc)=List.hd items in
    before_namespaces^" \n"^nspc_content^after_nspc;;

(*  

r "<?php12\nnamespace A{\n34\n56}78\n";;    
r "<?php12\nnamespace A;\n34\n56978\n";;   
*)
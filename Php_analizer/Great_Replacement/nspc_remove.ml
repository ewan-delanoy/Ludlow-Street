(*

#use"Php_analizer/Great_Replacement/nspc_remove.ml";;

Removes namespace declaration if it exists and is unique.
Used to avoid closing and reopening the same namespace when
expanding an inclusion.

*)

exception Nonunique_namespace;;

let r s=
    let (before_namespaces,items)=Nspc_split.decompose s in
    if List.length(items)<>1
    then raise(Nonunique_namespace)   
    else 
    let (nspc_line,nspc_content,after_nspc)=List.hd items in
    before_namespaces^" \n"^nspc_content^(Cull_string.cobeginning 1 after_nspc);;

(*  

r "<?php12\nnamespace A{\n34\n56}78\n";;    
*)
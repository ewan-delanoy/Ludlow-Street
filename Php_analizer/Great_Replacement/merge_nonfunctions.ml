(*

#use"Php_analizer/Great_Replacement/merge_nonfunctions.ml";;


*)

module Private=struct

let rec local_helper (graet,nspc_name,class_name,da_ober)=
    match da_ober with
    []->(String.concat "" (List.rev graet),[])
    |x::peurrest->
      if (Functionlike_item.namespace x=nspc_name)
         &&
         (Functionlike_item.containing_class x=class_name)
         &&
         (Functionlike_item.kind x=Functionlike_kind.non_function)
      then  local_helper ((Functionlike_item.content x)::graet,nspc_name,class_name,peurrest)  
      else  (String.concat "" (List.rev graet),da_ober);;

let rec main_helper (graet,da_ober)=
  match da_ober with
  []->List.rev graet
  |x::peurrest->
    if Functionlike_item.kind x=Functionlike_kind.non_function
    then let nspc_name=Functionlike_item.namespace x 
         and class_name=Functionlike_item.containing_class x in
         let (partial_content,peurrest2)=local_helper ([Functionlike_item.content x],nspc_name,class_name,peurrest) in
         let partial=Functionlike_item.non_function nspc_name class_name partial_content in
         main_helper(partial::graet,peurrest2)
    else main_helper(x::graet,peurrest);;

end;;

let mn l=Private.main_helper ([],l);;

(*
let example=
[];;

mn example;;


*)
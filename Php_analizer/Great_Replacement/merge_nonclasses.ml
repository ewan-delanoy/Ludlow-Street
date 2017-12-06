(*

#use"Php_analizer/Great_Replacement/merge_nonclasses.ml";;


*)

module Private=struct

let rec local_helper (graet,nspc_name,da_ober)=
    match da_ober with
    []->(String.concat "" (List.rev graet),[])
    |x::peurrest->
      if (Classlike_item.namespace x=nspc_name)
         &&
         (Classlike_item.kind x=Classlike_kind.plain_text)
      then  local_helper ((Classlike_item.content x)::graet,nspc_name,peurrest)  
      else  (String.concat "" (List.rev graet),da_ober);;

let rec main_helper (graet,da_ober)=
  match da_ober with
  []->List.rev graet
  |x::peurrest->
    if Classlike_item.kind x=Classlike_kind.plain_text
    then let nspc_name=Classlike_item.namespace x in
         let (partial_content,peurrest2)=local_helper ([Classlike_item.content x],nspc_name,peurrest) in
         let partial=Classlike_item.non_class nspc_name partial_content in
         main_helper(partial::graet,peurrest2)
    else main_helper(x::graet,peurrest);;

end;;

let mn l=Private.main_helper ([],l);;

(*
let example=
[{Classlike_item.kind = Classlike_kind.Namespace_line; namespace = "Peggy"; 
  class_name = "";          class_qualifiers = ""; 
  before_content = ""; content = "namespace Peggy{"; 
    after_content = ""};
   {Classlike_item.kind = Classlike_kind.Plain_text; namespace = "Peggy"; class_name = "";
    class_qualifiers = ""; before_content = ""; content = " ";
    after_content = ""};
   {Classlike_item.kind = Classlike_kind.Usual_class; namespace = "Peggy";
    class_name = "Arnold"; class_qualifiers = "";
    before_content = "class Arnold {"; content = " blacken(); ";
    after_content = "}"};
   {Classlike_item.kind = Classlike_kind.Plain_text; namespace = "Peggy"; class_name = "";
    class_qualifiers = ""; before_content = ""; content = " whiten(); \n\n";
    after_content = ""};
   {Classlike_item.kind = Classlike_kind.After_namespace_comments; namespace = ""; class_name = "";
    class_qualifiers = ""; before_content = "";
    content = "}\n /* Always there */ "; after_content = ""}];;

mn example;;


*)
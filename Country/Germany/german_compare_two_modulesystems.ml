
(* 

#use"Country/Germany/german_compare_two_modulesystems.ml";;

*)

 type t = {
      personal_modules : (Half_dressed_module.t list)*(Half_dressed_module.t list);
      intramodular_differences : Compare_two_modulesystem_data.t list;
};;

let consider_modules adjective l=
 if l=[] then "" else 
 adjective^" modules : "^(String.concat "," (Image.image Half_dressed_module.to_string l));;

let compare mdata1 mdata2=
  let (common_core,old_modules,new_modules)=
  	 (Simplify_without_orderings.generic_symmetric_decomposition 
  	   Modulesystem_data.name mdata1 mdata2) in
  let temp1=Image.image Compare_two_modulesystem_data.uncurried_compare common_core in
  let temp2=List.filter (fun dt->(Compare_two_modulesystem_data.display dt)<>"") temp1 in	   
  {
      personal_modules=(old_modules,new_modules);
      intramodular_differences=temp2;
  };;
 
let display x=
  let temp1=[
      
      consider_modules "Old" (fst(x.personal_modules));
      consider_modules "New" (snd(x.personal_modules));
      Compare_two_modulesystem_data.display_list x.intramodular_differences
  
  ] in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  String.concat "\n\n" (temp2@["\n"]);;
  
   
 
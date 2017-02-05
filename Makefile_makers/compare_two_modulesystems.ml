
(* 

#use"Makefile_makers/compare_two_modulesystems.ml";;

*)

 type t = {
      two_roots : Directory_name.t*Directory_name.t;
      two_toplevel_names : string*string;
      personal_modules : (Half_dressed_module.t list)*(Half_dressed_module.t list);
      intramodular_differences : Compare_two_modulesystem_data.t list;
};;

let consider_roots (dir1,dir2)=
  if dir1<>dir2
  then "Root changed from "^(Directory_name.to_string dir1)^" to "^(Directory_name.to_string dir2)
  else "";;

let consider_toplevel_names (s1,s2)=
  if s1<>s2
  then "Toplevel name changed from "^s1^" to "^s2
  else "";;

let consider_modules adjective l=
 if l=[] then "" else 
 adjective^" modules : "^(String.concat "," (Image.image Half_dressed_module.to_string l));;

let compare fs1 fs2=
  let data1=Modulesystem.all_filedata fs1
  and data2=Modulesystem.all_filedata fs2 in
  let (common_core,old_modules,new_modules)=
  	 (Simplify_without_orderings.generic_symmetric_decomposition 
  	   Modulesystem_data.name data1 data2) in
  let temp1=Image.image Compare_two_modulesystem_data.uncurried_compare common_core in
  let temp2=List.filter (fun dt->(Compare_two_modulesystem_data.display dt)<>"") temp1 in	   
  {
      two_roots =(Modulesystem.root fs1,Modulesystem.root fs2);
      two_toplevel_names =(Modulesystem.main_toplevel_name fs1,Modulesystem.main_toplevel_name fs2);
      personal_modules=(old_modules,new_modules);
      intramodular_differences=temp2;
  };;
 
let display x=
  let temp1=[
   
      consider_roots x.two_roots;
      consider_toplevel_names x.two_toplevel_names;
      consider_modules "Old" (fst(x.personal_modules));
      consider_modules "New" (snd(x.personal_modules));
      Compare_two_modulesystem_data.display_list x.intramodular_differences
  
  ] in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  String.concat "\n\n" (temp2@["\n"]);;
  
   
 
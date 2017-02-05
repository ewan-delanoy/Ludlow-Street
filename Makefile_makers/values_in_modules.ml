
(* 

#use"Makefile_makers/values_in_modules.ml";;

*)


let rename_value fs old_name new_name=
  let temp1=Modulesystem.files_containing_string fs old_name in
  let m=String.length(Directory_name.to_string(Modulesystem.root fs)) in
  let temp2=Image.image (fun ap->
    Cull_string.cobeginning m (Absolute_path.to_string ap)) temp1 in
  let message="\n\n The following files will be rewritten : \n\n"^
  (String.concat "\n" temp2) in
  let _=(print_string message;flush stdout) in
  List.iter (Replace_inside.replace_inside_file (old_name,new_name)) temp1;;

let list_values_from_module_in_file module_name file=
   let s=Io.read_whole_file file in
   let temp1=Look_for_module_names.indices_in_file file in
   let temp2=List.filter (fun (t,(i,j))->
     (t=My_str_example.index_for_pointed_case)&&
     (Cull_string.interval s i j=(String.capitalize module_name))
   ) temp1 in
   let temp3=Image.image(fun (t,(i,j))->
    Charset.starry_from
     Charset.strictly_alphanumeric_characters
     s (j+2)
   ) temp2 in
   Ordered_string.diforchan temp3;;

let list_values_from_module_in_modulesystem module_name fs=
   let temp1=Modulesystem.all_mlx_paths fs in
   let temp2=Image.image (fun ap->
    let ttemp1=list_values_from_module_in_file module_name ap in
    Ordered_string.image (fun x->(x,ap) ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=Image.image fst temp3 in 
   let temp5=Ordered_string.diforchan temp4 in
   let temp6=Ordered.forget_order temp5 in
   let temp7=Image.image (
      fun x->(x,Option.filter_and_unpack(
        fun (y,ap)->if y=x then Some(ap) else None
      ) temp3)
   ) temp6 in
   temp7;;
 
 let list_value_occurrences_in_file t file=
   let s=Io.read_whole_file file in
   let temp1=Substring.occurrences_of_in t s in
   Image.image (fun j->Cull_string.closeup_around_index 
      s j
   ) temp1;; 
 
 let modules_using_value ms value_name =
   let temp1=Modulesystem.all_filedata ms in
   Option.filter_and_unpack (fun md->
    let ap=Modulesystem_data.principal_path md in
    if (list_value_occurrences_in_file value_name ap)<>[] 
    then Some(Modulesystem_data.name md)
    else None ) temp1;;
 
 


let show_value_occurrences_in_modulesystem t fs=
   let m=String.length(Directory_name.to_string(Modulesystem.root(fs))) in
   let temp1=Modulesystem.all_mlx_paths fs in
   let temp2=Image.image (fun ap->
    let ttemp1=list_value_occurrences_in_file t ap in
    let mname=Cull_string.cobeginning(m)(Absolute_path.to_string ap) in
    Image.image (fun x->mname^":\n"^x ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=String.concat "\n\n\n" (""::temp3@[""]) in 
   print_string temp4;;




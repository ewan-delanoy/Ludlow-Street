(*

#use"Ocaml_analysis/rename_value_inside_module.ml";;

Changes a value's name inside a module. 
This function should never be used by itself except for debugging.

*)

exception No_module_given of string;;

let get_module_inside_name s=
   let f=Father_and_son.father s '/' in
   if f=""
   then raise(No_module_given(s))
   else f;;
   

let rename_value_inside_module s new_name=
   let j=Substring.leftmost_index_of_in "." s in
   if j<0 
   then raise(No_module_given(s))
   else 
   let module_name=Cull_string.beginning (j-1) s in
   let hm=German_vague_string.to_module module_name 
   and path=German_vague_string.to_path module_name in 
   let temp1=German_wrapper.data() in
   let md1=Option.find_really (fun md->Modulesystem_data.name md=hm) temp1 in
   let temp2=(Modulesystem_data.all_ancestors md1)@[hm] in
   let all_files=Image.image  (fun hm->
   	 Mlx_filename.to_path(Mlx_filename.join hm Ocaml_ending.Ml)
   ) temp2 in
   let temp3=Read_ocaml_files.read_ocaml_files all_files in
   let temp4=Option.find_really (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s
   ) temp3 in
   let (i1,j1)=temp4.Ocaml_gsyntax_item.interval_for_name in
   Replace_inside.overwrite_at_interval_inside_file
   new_name (i1,j1) path;;
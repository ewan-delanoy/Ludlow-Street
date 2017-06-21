(*
#use"self_contained_module_copy.ml";;
*)

let self_contained_module_copy prefix hm=
   let those_above=German_data.above (German_wrapper.data()) hm in
   let temp1=Image.image (
       fun hm2->
         let mlx=Mlx_ended_absolute_path.join hm2 Ocaml_ending.ml in
         let ap=Mlx_ended_absolute_path.to_absolute_path mlx in
         let naked_name=Modularize.module_name_from_path ap in
         let s_name=Naked_module.to_string naked_name in
         let mname=String.capitalize_ascii s_name in
         let new_name=String.capitalize_ascii (prefix^s_name) in
         let content=Io.read_whole_file ap
         ((mname,new_mname),content)
   ) those_above in
   let replacements=Image.image fst temp1 in
   let contents=Image.image snd temp1 in
   let new_contents=Image.image 
        (Look_for_module_names.change_several_module_names_in_string replacements  ) 
         contents in
   let temp2=List.combine replacements new_contents in
   let temp3=Image.image (
     fun ((_n,new_mname),txt)->
      "module "^new_mname^"=struct "^txt^" end;;"
   ) in    
   String.concat "\n\n\n" temp3;;



(*

*)


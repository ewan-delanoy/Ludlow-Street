(*
#use"self_contained_module_copy.ml";;
*)


let unsharped_content hm=
    let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
    let ap_ml=Mlx_ended_absolute_path.to_absolute_path mlx in
    let naive_content=Io.read_whole_file ap_ml in
    if Sys.file_exists((Absolute_path.to_string ap_ml)^"l")
    then (
           let temp1=Str.split (Str.regexp_string "\n") naive_content in
           let temp2=Option.filter_and_unpack (
                fun t->
                 if t="" then Some("") else
                 if String.get t 0='#' then None else Some(t) 
           ) temp1 in
           String.concat "\n" temp2
         )
    else naive_content;;

let self_contained_module_copy prefix hm=
   let those_above=German_data.above (German_wrapper.data()) hm in
   let temp1=Image.image (
       fun hm2->
         let mlx=Mlx_ended_absolute_path.join hm2 Ocaml_ending.ml in
         let ap=Mlx_ended_absolute_path.to_absolute_path mlx in
         let naked_name=Modularize.module_name_from_path ap in
         let s_name=Naked_module.to_string naked_name in
         let mname=String.capitalize_ascii s_name in
         let new_mname=String.capitalize_ascii (prefix^s_name) in
         let content=unsharped_content hm2 in
         ((mname,new_mname),content)
   ) those_above in
   let replacements=Image.image (
       fun ((a,b),_)->(Naked_module.of_string a,Naked_module.of_string b)
   ) temp1 in
   let contents=Image.image snd temp1 in
   let new_contents=Image.image 
        (Look_for_module_names.change_several_module_names_in_string replacements  ) 
         contents in
   let temp2=List.combine replacements new_contents in
   let temp3=Image.image (
     fun ((_,new_mname),txt)->
      let name=String.capitalize_ascii(Naked_module.to_string new_mname) in
      "module "^name^"=struct "^txt^" end;;"
   ) temp2 in    
   String.concat "\n\n\n" temp3;;



(*

*)


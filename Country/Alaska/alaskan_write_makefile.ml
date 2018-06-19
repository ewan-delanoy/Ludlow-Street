
(* 

#use"Country/Alaska/alaskan_write_makefile.ml";;


*)

let slice_targets tgts=
  let temp1=Sliced_string.make_aggregates_if_possible 
                   (Separator.of_string " ") 
                   (Image.image Ocaml_target.to_string tgts) in
  Sliced_string.to_string_list temp1;;

let slice_shortened_targets tgts=
  let temp1=Sliced_string.make_aggregates_if_possible 
                   (Separator.of_string " ") 
                   (Image.image Ocaml_target.to_shortened_string tgts) in
  Sliced_string.to_string_list temp1;;

let write_usual_makefile_element main_root mdata tgt=
 let ingrs=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target mdata tgt in
 let sliced_ingrs=slice_shortened_targets ingrs in
 let cmds=Alaskan_command_for_ocaml_target.command_for_ocaml_target 
                       main_root mdata tgt in
 let s1=(Ocaml_target.to_shortened_string tgt)^" : " 
 and s2=String.concat " \\\n" sliced_ingrs
 and s3="\n\t"
 and s4=String.concat "\n\t" cmds in
 String.concat "" [s1;s2;s3;s4];;
 
let write_full_compilation_makefile_element main_root mdata=
  let l=Md_list.all_modules mdata in
  let temp1=Image.image (Alaskan_ingredients_for_ocaml_target.ingredients_for_usual_element mdata) l in
  let ingrs=Preserve_initial_ordering.preserve_initial_ordering temp1 in
  let sliced_ingrs=slice_shortened_targets ingrs in
  let l_dt=Image.image (fun hm->Option.unpack(Md_list.find_module_registration mdata hm)) l  in
  let s_root=Directory_name.connectable_to_subpath(main_root) in
  let long_temp4=Image.image (fun fd->
             let hm=Modulesystem_data.name fd in
             let s_hm=(Half_dressed_module.uprooted_version hm) in
             let short_s_hm=Father_and_son.son s_hm '/' in
             if Modulesystem_data.ml_present fd 
             then s_root^"_build/"^short_s_hm^".cmo"
             else " "
  ) l_dt in   
  let long_s_lhm=String.concat " " long_temp4 in
  let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs_for_several false l_dt in
  let cmds=[ "ocamlmktop "^dirs_and_libs^" -o "^s_root^"ecaml "^long_s_lhm^" ";
          "mv "^s_root^"ecaml "^s_root^"_build/"] in
  let s1="ecaml : " 
  and s2=String.concat " \\\n" sliced_ingrs
  and s3="\n\t"
  and s4=String.concat "\n\t" cmds in
  String.concat "" [s1;s2;s3;s4];; 
 
let write_makefile main_root mdata=
 let temp1=Md_list.usual_targets mdata in
 let temp2=Image.image (write_usual_makefile_element main_root mdata) temp1 in
 let temp3=temp2@[write_full_compilation_makefile_element main_root mdata] in
 let temp5=slice_targets  temp1 in
 let temp6=String.concat " \\\n" temp5 in
 let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
 String.concat "\n\n" (temp3@[temp7]);;

 
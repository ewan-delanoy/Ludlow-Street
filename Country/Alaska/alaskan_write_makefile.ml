
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
 let ingrs=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target 
    mdata tgt in
 let sliced_ingrs=slice_shortened_targets ingrs in
 let cmds=Alaskan_command_for_ocaml_target.command_for_ocaml_target 
                       main_root mdata tgt in
 let s1=(Ocaml_target.to_shortened_string tgt)^" : " 
 and s2=String.concat " \\\n" sliced_ingrs
 and s3="\n\t"
 and s4=String.concat "\n\t" cmds in
 String.concat "" [s1;s2;s3;s4];;
 
let write_full_compilation_makefile_element  wmdata=
  let main_root=Modify_md_list.root wmdata in
  let l=Modify_md_list.all_modules wmdata in
  let temp1=Image.image 
  (Alaskan_ingredients_for_ocaml_target.ingredients_for_usual_element wmdata) l in
  let ingrs=Preserve_initial_ordering.preserve_initial_ordering temp1 in
  let sliced_ingrs=slice_shortened_targets ingrs in
  let l_idx=Image.image (fun hm->
    let nm=Half_dressed_module.naked_module hm in
    Modify_md_list.find_module_index wmdata nm) l  in
  let s_root=Root_directory.connectable_to_subpath(main_root) in
  let long_temp4=Image.image (fun idx->
             let hm=Modify_md_list.hm_at_idx wmdata idx in
             let s_hm=(Half_dressed_module.uprooted_version hm) in
             let short_s_hm=Father_and_son.son s_hm '/' in
             let ml_reg=Modify_md_list.check_ending_in_at_idx Ocaml_ending.ml wmdata idx in
             if ml_reg
             then s_root^"_build/"^short_s_hm^".cmo"
             else " "
  ) l_idx in   
  let long_s_lhm=String.concat " " long_temp4 in
  let dirs_and_libs=Modify_md_list.needed_dirs_and_libs_for_several false wmdata l_idx in
  let cmds=[ "ocamlmktop "^dirs_and_libs^" -o "^s_root^"ecaml "^long_s_lhm^" ";
          "mv "^s_root^"ecaml "^s_root^"_build/"] in
  let s1="ecaml : " 
  and s2=String.concat " \\\n" sliced_ingrs
  and s3="\n\t"
  and s4=String.concat "\n\t" cmds in
  String.concat "" [s1;s2;s3;s4];; 
 
let write_makefile wmdata=
 let main_root=Modify_md_list.root wmdata in 
 let temp1=Modify_md_list.usual_targets wmdata in
 let temp2=Image.image (write_usual_makefile_element main_root wmdata) temp1 in
 let temp3=temp2@[write_full_compilation_makefile_element wmdata] in
 let temp5=slice_targets  temp1 in
 let temp6=String.concat " \\\n" temp5 in
 let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
 String.concat "\n\n" (temp3@[temp7]);;

 
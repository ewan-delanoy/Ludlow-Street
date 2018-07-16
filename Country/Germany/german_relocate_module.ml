
(* 


#use"Country/Germany/german_relocate_module.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 

Speech follows action : file displacement takes place first, 
then the Ocaml data values are updated.

*)





let on_targets root_dir (old_mdata,old_tgts) old_name new_subdir= 
  let untouched_tgts=List.filter
   (fun tgt->not(Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=
    Modify_md_list.relocate_module_on_monitored_modules root_dir old_mdata old_name new_subdir in
  let (new_mdata2,new_tgts2,_)=Alaskan_make_ocaml_target.feydeau
    root_dir new_mdata untouched_tgts  in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 

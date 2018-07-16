
(* 


#use"Country/Germany/german_rename_module.ml";;

The functions of this module are supposed to be used
on an already up-to-date modulesystem.

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)



let on_targets root_dir (old_mdata,old_tgts) old_name new_name= 
  let untouched_tgts=List.filter
   (fun tgt->not(Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=
     Modify_md_list.rename_module_on_monitored_modules root_dir old_mdata old_name new_name in
  let (new_mdata2,new_tgts2,_)=Alaskan_make_ocaml_target.feydeau
 root_dir new_mdata untouched_tgts  in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 
 
 


 
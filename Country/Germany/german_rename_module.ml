
(* 


#use"Country/Germany/german_rename_module.ml";;

The functions of this module are supposed to be used
on an already up-to-date modulesystem.

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)



let on_targets (old_mdata,old_tgts) old_name new_name= 
  let untouched_tgts=List.filter
   (fun tgt->not(Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=
     Md_list.rename_module_on_monitored_modules old_mdata old_name new_name in
  let default_top=(Md_list.default_toplevel German_constant.main_toplevel_name new_mdata) in
  let (new_mdata2,new_tgts2,_)=
    snd(Alaskan_make_ocaml_target.make 
 	   German_constant.root
 	    (new_mdata,untouched_tgts,[]) default_top) in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 
 
 


 

(* 


#use"Country/Germany/german_rename_module.ml";;

The functions of this module are supposed to be used
on an already up-to-date modulesystem.

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)



exception Nonregistered_module of Half_dressed_module.t;;  


let on_monitored_modules mdata old_name new_name=
   let interm_list=Image.image
   (Abstract_renamer.abstractify old_name) mdata in
   let opt=German_data.find_module_registration mdata old_name in
   if opt=None
   then raise(Nonregistered_module(old_name))
   else 
   let old_dt=Option.unpack opt in
   let old_acolytes=Modulesystem_data.acolytes old_dt in
   let temp7=Image.image (fun mlx->Mlx_filename.do_file_renaming mlx new_name) 
   old_acolytes in
   let new_hm=Mlx_filename.half_dressed_core(List.hd temp7) in
   let old_mname=Half_dressed_module.module_name old_name
   and new_mname=Half_dressed_module.module_name new_hm
   in
   let changer=Look_for_module_names.change_module_name_in_file
   old_mname new_mname in
   let desc=German_data.descendants mdata [old_name] in
   let temp1=Image.image Modulesystem_data.acolytes desc in
   let temp2=List.flatten temp1 in
   let temp3=Image.image Mlx_filename.to_path temp2 in
   let _=Image.image changer temp3 in
   let cmd2=Shell_command.usual ("rm -f "^(Half_dressed_module.to_string old_name)^".cm* ") in
   let cmd=Shell_command.take_care_of_root_directory German_constant.root [cmd2] in
   let _=Image.image Shell_command.announce_and_do cmd in
   let new_list=Image.image
   (Abstract_renamer.unabstractify new_hm) interm_list in
   new_list;;
 
let on_targets (old_mdata,old_tgts) old_name new_name= 
  let untouched_tgts=List.filter
   (fun tgt->not(German_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let new_mdata=German_rename_file_in_system.rename old_mdata old_name new_name in
  let default_top=(German_data.default_toplevel new_mdata) in
  let (new_mdata2,new_tgts2)=snd(German_make_ocaml_target.make (new_mdata,untouched_tgts) default_top) in
  (new_mdata2,new_tgts2);;   
 
 
 


 
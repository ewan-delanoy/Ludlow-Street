
(* 


#use"Country/Germany/german_recompile.ml";;


*)

module Private=struct

let message_about_circular_dependencies printer cycles= 
  if cycles=[]
  then ""
  else
  let temp1=Image.image(fun cycle->
    let ttemp1=Image.image printer cycle in
     String.concat " -> " ttemp1 
  ) cycles in
  let temp2=String.concat "\n\n" temp1 in
  temp2;;

exception Circular_dependencies of string;;

let treat_circular_dependencies tolerate_cycles printer cycles=
  if cycles=[]
  then ()
  else let msg=message_about_circular_dependencies printer cycles in  
       if tolerate_cycles
       then (print_string msg;flush stdout)     
       else raise(Circular_dependencies(msg));; 
       
let message_about_changed_modules changed_modules=
  let temp1=Image.image Half_dressed_module.to_string changed_modules in
  "\n\n\n"^
  "The following modules have been directly changed :\n"^
  (String.concat "\n" temp1)^
  "\n\n\n"
;;       
       
let announce_changed_modules changed_modules=
  if changed_modules=[]
  then ()
  else (print_string(message_about_changed_modules changed_modules);flush stdout);;
         

let put_md_list_back_in_order tolerate_cycles md_list initially_active_hms=
  let coat=Memoized.make (fun md->
    let anc_md=Modulesystem_data.direct_fathers(md) in
    List.filter (
      fun md2->List.mem(Modulesystem_data.name md2) anc_md
    ) md_list
  ) in
  let (cycles,old_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
     coat md_list in
  let _=treat_circular_dependencies tolerate_cycles
       (fun md->Half_dressed_module.to_string(Modulesystem_data.name md)) 
       cycles in     
  let intermediary_list=Update_ancs_libs_and_dirs_in_modulesystem.update old_list in
  let initially_active_mds=List.filter      
       (fun md->List.mem (Modulesystem_data.name md) initially_active_hms)
       intermediary_list in
  let active_ancestors=List.flatten (
      Image.image Modulesystem_data.all_ancestors initially_active_mds
  ) in    
  let active_descendants=Option.filter_and_unpack (
      fun md->
        let hm=Modulesystem_data.name md in
        if List.mem hm initially_active_hms
        then Some(hm)
        else
        if List.exists (fun hm2->List.mem hm2 initially_active_hms) 
             (Modulesystem_data.all_ancestors md)
        then Some(hm)
        else None
  ) intermediary_list in    
  let active_hms=active_ancestors@active_descendants in
  let final_list=Image.image (
      fun md->
       let hm=Modulesystem_data.name md in
       if List.mem hm active_hms
       then md
       else Modulesystem_data.increment_inactivity_count md
  ) intermediary_list in
  (final_list,active_descendants);;
 
end;; 
 
let on_monitored_modules tolerate_cycles mdata =
  let ref_for_changed_modules=ref[] in
  let declare_changed=(fun hm->
    ref_for_changed_modules:=hm::(!ref_for_changed_modules)
  ) in
  let new_md_list=Image.image(
     fun md->
       match Read_info_on_file_in_system.quick_update mdata md with
       None->md
       |Some(new_md)->
         let _=declare_changed( Modulesystem_data.name new_md) in
         new_md
  ) mdata in
  let changed_modules=List.rev(!ref_for_changed_modules) in
  if changed_modules=[] then (mdata,[]) else
  let _=Private.announce_changed_modules changed_modules in
  let (new_list,to_be_updated)=
    Private.put_md_list_back_in_order tolerate_cycles new_md_list changed_modules in
  (new_list,to_be_updated);;  
  
let on_targets tolerate_cycles (old_mdata,old_tgts)=
    let (new_mdata,hms_to_be_updated)=
      on_monitored_modules tolerate_cycles old_mdata in
	if hms_to_be_updated=[] then None else
	let new_dirs=German_directories.from_data new_mdata 
 	and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
 	let dir=German_constant.root in
 	let checker=(fun tgt->
 	  let s=(Directory_name.to_string dir)^(Ocaml_target.to_string tgt) in 
 	  Sys.file_exists s ) in
 	let new_tgts=List.filter checker new_tgts1 in
 	let default_top=(German_data.default_toplevel new_mdata) in
 	let (new_mdata2,new_tgts2)=snd(German_make_ocaml_target.make (new_mdata,new_tgts) default_top) in
    Some(new_mdata2,new_dirs,new_tgts2);;   


   




(* 


#use"Makefile_makers/self_update_modulesystem.ml";;


*)

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
 
let  self_update_modulesystem tolerate_cycles fs =
  let md_list=Modulesystem.all_filedata fs 
  and ref_for_changed_modules=ref[] in
  let declare_changed=(fun hm->
    ref_for_changed_modules:=hm::(!ref_for_changed_modules)
  ) in
  let new_md_list=Image.image(
     fun md->
       match Read_info_on_file_in_system.quick_update md_list md with
       None->md
       |Some(new_md)->
         let _=declare_changed( Modulesystem_data.name new_md) in
         new_md
  ) md_list in
  let changed_modules=List.rev(!ref_for_changed_modules) in
  if changed_modules=[] then (fs,[]) else
  let _=announce_changed_modules changed_modules in
  let (new_list,to_be_updated)=
    put_md_list_back_in_order tolerate_cycles new_md_list changed_modules in
  (Modulesystem.make(Modulesystem.root fs,new_list,
  Modulesystem.main_toplevel_name fs),to_be_updated);;  
  

(*
let  self_update_modulesystem tolerate_cycles fs =(fs,[]);;
*)


(*
let individual_update (md,atoms_for_md)=
   let hm=Modulesystem_data.name md in
   let edg=List.hd(Modulesystem_data.registered_endings md) in
   let mlx=Mlx_filename.join hm edg in
   let names=Read_info_on_file_in_system.find_needed_names atoms_for_md mlx in
   let uncapitalized_ones=Image.image 
     (fun hm->Naked_module.to_string(Half_dressed_module.undress hm)) names in
   let last_libs=Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
       uncapitalized_ones in
   let all_libs=List.filter
     (
       fun lib->
         (List.mem lib last_libs)
         ||
         (List.exists (fun md2->
            List.mem lib (Modulesystem_data.needed_libraries md2)
         ) atoms_for_md) 
     )
     Ocaml_library.all_libraries    in
     let s_mlx=Mlx_filename.to_string mlx in
    let last_subdir=Subdirectory.of_string(Father_and_son.father s_mlx '/') in
    let subdirs=(Tidel.singleton last_subdir)::(Image.image 
      (fun md2->Tidel.safe_set(Modulesystem_data.needed_directories md2)) 
      atoms_for_md
    ) in
    let all_subdirs=Ordered.forget_order(Tidel.big_teuzin(subdirs)) in
    {
    Modulesystem_data.name=md.Modulesystem_data.name;
    ml_present=md.Modulesystem_data.ml_present;
    mli_present=md.Modulesystem_data.mli_present;
    mll_present=md.Modulesystem_data.mll_present;
    mly_present=md.Modulesystem_data.mly_present;
    ml_modification_time=md.Modulesystem_data.ml_modification_time;
    mli_modification_time=md.Modulesystem_data.mli_modification_time;
    mll_modification_time=md.Modulesystem_data.mll_modification_time;
    mly_modification_time=md.Modulesystem_data.mly_modification_time;
    needed_libraries=all_libs;
    direct_fathers=md.Modulesystem_data.direct_fathers;
    all_ancestors=Image.image Modulesystem_data.name atoms_for_md;
    needed_directories=all_subdirs;
    inactivity_count=md.Modulesystem_data.inactivity_count
    }   ;;
*)     
     
     
     
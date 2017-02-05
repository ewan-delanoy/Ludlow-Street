
(* 


#use"Makefile_makers/modify_target_system.ml";;

*)


let unregister_mlx_file ts mlx=
  let old_fs=Target_system.modulesystem ts
  and old_tgts=Target_system.up_to_date_targets ts 
  and hm=Mlx_filename.half_dressed_core mlx 
  and o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in
 let new_filesys=Modify_modulesystem.unregister_mlx_file old_fs mlx in
 let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_filesys 
 and new_tgts=List.filter (fun tgt->
   match Ocaml_target.main_module tgt with
   None->false |Some(hm2)->hm2<>hm
 ) old_tgts in
  Target_system.make new_filesys new_dirs new_tgts o_dirs o_files r_deleted pe_types;;
  
let unregister_module ts hm=
  let old_fs=Target_system.modulesystem ts
  and old_tgts=Target_system.up_to_date_targets ts 
  and o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in
 let new_filesys=Modify_modulesystem.unregister_module old_fs hm in
 let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_filesys 
 and new_tgts=List.filter (fun tgt->
   match Ocaml_target.main_module tgt with
   None->false |Some(hm2)->hm2<>hm
 ) old_tgts in
  Target_system.make new_filesys new_dirs new_tgts o_dirs o_files r_deleted pe_types;;  
  
  
 let register_mlx_file ts mlx=
  let old_fs=Target_system.modulesystem ts
  and old_dirs=Target_system.directories ts 
  and old_tgts=Target_system.up_to_date_targets ts 
  and hm=Mlx_filename.half_dressed_core mlx 
  and o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in
  let new_dir=Compute_modulesystem_directories.individual_directory hm in
 let new_filesys=Modify_modulesystem.register_mlx_file old_fs mlx in
 let new_dirs=
 (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
 and new_tgts=
 (if Half_dressed_module.is_optional hm
  then old_tgts
  else (*
       The only outdated targets are the main toplevel, 
       and targets corresponding to an identical module
       (for example when a mll or mly is added to
       an already registered ml) 
        *)
       let mn=Target_system.main_toplevel_name ts in
       List.filter (
        fun tgt->match Ocaml_target.toplevel_name tgt with
          None->(match Ocaml_target.main_module tgt with
                 None->true
                 |Some(hm2)->hm2<>hm
                )
          |Some(name)->name<>mn
       ) old_tgts
  ) in
  Target_system.make new_filesys new_dirs new_tgts o_dirs o_files r_deleted pe_types;; 
 
 let reposition ts mlx (l_before,l_after)=
    let old_fs=Target_system.modulesystem ts
    and old_dirs=Target_system.directories ts 
    and old_tgts=Target_system.up_to_date_targets ts 
    and o_dirs=Target_system.outside_directories ts 
    and o_files=Target_system.outside_files ts 
    and r_deleted=Target_system.recently_deleted ts 
    and pe_types=Target_system.printer_equipped_types ts in
    let new_fs=Modify_modulesystem.reposition old_fs mlx (l_before,l_after) in
    Target_system.make new_fs old_dirs old_tgts o_dirs o_files r_deleted pe_types;; 
    
 
let self_update tolerate_cycles ts=
    let old_fs=Target_system.modulesystem ts
    and old_tgts=Target_system.up_to_date_targets ts 
    and o_dirs=Target_system.outside_directories ts 
    and o_files=Target_system.outside_files ts 
    and r_deleted=Target_system.recently_deleted ts 
    and pe_types=Target_system.printer_equipped_types ts in
    let (new_fs,hms_to_be_updated)=Self_update_modulesystem.self_update_modulesystem tolerate_cycles old_fs in
	if hms_to_be_updated=[] then (false,ts) else
	let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_fs 
 	and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
 	let dir=Target_system.root ts in
 	let checker=(fun tgt->
 	  let s=(Directory_name.to_string dir)^(Ocaml_target.to_string tgt) in 
 	  Sys.file_exists s ) in
 	let new_tgts=List.filter checker new_tgts1 in
    (true,Target_system.make new_fs new_dirs new_tgts o_dirs o_files r_deleted pe_types);;   
     
 
let rename_module ts old_hm new_hm=
  let old_fs=Target_system.modulesystem ts
  and old_dirs=Target_system.directories ts 
  and old_tgts=Target_system.up_to_date_targets ts 
  and o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in
  let untouched_targets=List.filter
   (fun tgt->not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_fs [old_hm] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_hm)) ) old_tgts in
  let new_fs=Rename_file_in_system.rename old_fs old_hm new_hm in
  let ts1=Target_system.make new_fs old_dirs untouched_targets o_dirs o_files r_deleted pe_types
  in
  snd(self_update false ts1);;   
  
let make_module_optional ts old_hm =
  let old_fs=Target_system.modulesystem ts
  and old_tgts=Target_system.up_to_date_targets ts 
  and o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in
 let new_fs=Relocate_file_in_system.make_module_optional old_fs old_hm in
 let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_fs in
 let ts1=Target_system.make new_fs new_dirs old_tgts o_dirs o_files r_deleted pe_types in
 snd(self_update false ts1);;   
  
let relocate_module ts old_hm new_dir=
    let old_fs=Target_system.modulesystem ts
    and old_tgts=Target_system.up_to_date_targets ts 
    and o_dirs=Target_system.outside_directories ts 
    and o_files=Target_system.outside_files ts 
    and r_deleted=Target_system.recently_deleted ts 
    and pe_types=Target_system.printer_equipped_types ts in
    let new_fs=Relocate_file_in_system.relocate_module old_fs old_hm new_dir in
    let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_fs in
    let ts1=Target_system.make new_fs new_dirs old_tgts o_dirs o_files r_deleted pe_types in
    snd(self_update false ts1);;   
  
 let force_modification_time_update ts mlx= 
    let fs=Target_system.modulesystem ts 
    and o_dirs=Target_system.outside_directories ts 
    and o_files=Target_system.outside_files ts 
    and r_deleted=Target_system.recently_deleted ts 
    and pe_types=Target_system.printer_equipped_types ts in
    Target_system.make 
    (Modify_modulesystem.force_modification_time_update fs mlx) 
    (Target_system.directories ts) 
    (Target_system.up_to_date_targets ts)
    o_dirs
    o_files
    r_deleted
    pe_types;;
  
let reset_inactivity_counts ts=
  let old_fs=Target_system.modulesystem ts
  and old_dirs=Target_system.directories ts 
  and old_tgts=Target_system.up_to_date_targets ts 
  and o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in
  let new_fs=Modify_modulesystem.reset_inactivity_counts(old_fs) in
  Target_system.make new_fs old_dirs old_tgts o_dirs o_files r_deleted pe_types;;   

let register_outside_file ts ap=
  let main_dir=Target_system.root ts in
  let temp1=Directory_name.cut_beginning main_dir (Absolute_path.to_string ap) in
  let s_sdir=Father_and_son.father temp1 '/' in
  let sdir=Subdirectory.of_string s_sdir in
  let o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in 
  let new_odirs=(if List.mem sdir o_dirs then o_dirs else o_dirs@[sdir]) 
  and new_ofiles=(if List.mem ap o_files then o_files else ap::o_files) in
  Target_system.make
    (Target_system.modulesystem ts)
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    new_odirs
    new_ofiles
    r_deleted
    pe_types;;
    
let unregister_outside_file ts ap=
  let main_dir=Target_system.root ts in
  let tempf=(fun ap2->
  	let temp1=Directory_name.cut_beginning main_dir (Absolute_path.to_string ap2) in
  	let s_sdir=Father_and_son.father temp1 '/' in
  	Subdirectory.of_string s_sdir 
  ) in
  let o_dirs=Target_system.outside_directories ts 
  and o_files=Target_system.outside_files ts 
  and r_deleted=Target_system.recently_deleted ts 
  and pe_types=Target_system.printer_equipped_types ts in
  let sdir=tempf ap in 
  let new_ofiles=List.filter (fun ap2->ap2<>ap) o_files in
  let new_odirs=(
       if List.exists(fun z->tempf(z)=sdir) new_ofiles
       then o_dirs
       else List.filter (fun sdir2->sdir2<>sdir) o_dirs
  )  in
  Target_system.make
    (Target_system.modulesystem ts)
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    new_odirs
    new_ofiles
    r_deleted
    pe_types;;    
    
let recompute_module_info ts hm=
  let old_ms=Target_system.modulesystem ts in
  let new_ms=Modify_modulesystem.recompute_module_info old_ms hm in
  Target_system.make
    new_ms
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    (Target_system.outside_directories ts )
    (Target_system.outside_files ts )
    (Target_system.recently_deleted ts)
    (Target_system.printer_equipped_types ts);;    
      
let declare_printer ts hm0=
  Target_system.make
    (Target_system.modulesystem ts)
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    (Target_system.outside_directories ts )
    (Target_system.outside_files ts )
    (Target_system.recently_deleted ts)
    (hm0::(Target_system.printer_equipped_types ts));;         


let undeclare_printer ts hm0=
  Target_system.make
    (Target_system.modulesystem ts)
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    (Target_system.outside_directories ts )
    (Target_system.outside_files ts )
    (Target_system.recently_deleted ts)
    (List.filter (fun hm->hm<>hm0) (Target_system.printer_equipped_types ts));;    

let add_target_perhaps opt_tgt ts=
  Target_system.make
              (Target_system.modulesystem ts)
              (Target_system.directories ts)
              (Option.add_perhaps opt_tgt (Target_system.up_to_date_targets ts))
              (Target_system.outside_directories ts)
              (Target_system.outside_files ts)
              (Target_system.recently_deleted ts)
              (Target_system.printer_equipped_types ts);;

 let officialize_recent_deletion subpath ts=
  Target_system.make
    (Target_system.modulesystem ts)
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    (Target_system.outside_directories ts)
    (Target_system.outside_files ts)
    (subpath::(Target_system.recently_deleted ts))
    (Target_system.printer_equipped_types ts);; 
  
 let forget_recent_deletions ts=
  Target_system.make
    (Target_system.modulesystem ts)
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    (Target_system.outside_directories ts)
    (Target_system.outside_files ts)
    []
    (Target_system.printer_equipped_types ts);;  
  
 let look_for_printers ts=
   let ms=Target_system.modulesystem ts in
   let temp1=Modulesystem.all_filedata ms in
   let temp2=Option.filter_and_unpack (
  	fun md->
   	let hm=Modulesystem_data.name md
   	and ap=Modulesystem_data.principal_path md in
   	let text=Io.read_whole_file ap in
   	if (Substring.is_a_substring_of
     ("let "^"print_out ") text)&&
    	(not(Half_dressed_module.is_optional hm))
   	then Some(hm)
  	else None
   ) temp1 in
   List.fold_left declare_printer ts temp2;;

  
  
  
  
     
  
  
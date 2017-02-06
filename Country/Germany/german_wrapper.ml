(*

#use"Country/Germany/german_wrapper.ml";;


*)

module Private=struct

let data_ref=ref([]:Modulesystem_data.t list);;
let directories_ref=ref([]:Subdirectory.t list);;
let up_to_date_targets_ref=ref([]:Ocaml_target.t list);;
let outside_files_ref=ref([]:Absolute_path.t list);;
let outside_directories_ref=ref([]:Subdirectory.t list);;
let recently_deleted_ref=ref([]:string list);;
let recently_changed_ref=ref([]:string list);;
let recently_created_ref=ref([]:string list);;
let printer_equipped_types_ref=ref([]:Half_dressed_module.t list);;

let whole ()=(
	(!data_ref),
	(!directories_ref),
	(!up_to_date_targets_ref),
	(!outside_files_ref),
	(!outside_directories_ref),
	(!recently_deleted_ref),
	(!recently_changed_ref),
	(!recently_created_ref),
	(!printer_equipped_types_ref)
);;

let save_all ()=German_save_all.write_all (whole());;

let recompile ()=
   match German_recompile.on_targets false (!data_ref,!up_to_date_targets_ref) with
    None->()
   |Some(new_mdata,new_dirs,new_tgts)->
       (
         data_ref:=new_mdata;
         directories_ref:=new_dirs;
         up_to_date_targets_ref:=new_tgts;
         save_all();
       );;

end;;

let data ()=(!Private.data_ref);;
let directories ()=(!Private.directories_ref);;


 
 
let end_debugging ()=
    let _= Alaskan_remove_debuggables.rd 
                German_constant.root (!Private.data_ref) in
                
    let new_tgts=List.filter Ocaml_target.is_not_a_debuggable
         (!Private.up_to_date_targets_ref)  in 
       (
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;   
   
let forget_unregistered_file ap=
    let _=Private.recompile() in
    let new_rdel=
      German_forget_unregistered_file.on_recently_deleted_files ap in
       (
         Private.recently_deleted_ref:=new_rdel::(!(Private.recently_deleted_ref));
         Private.save_all();
       );;       
       
let forget_file ap=
   let _=Private.recompile() in
   let ((new_mdata,new_dirs,new_tgts),new_rdel)= 
    German_forget_file.on_targets 
      (data(),directories(),(!Private.up_to_date_targets_ref)) ap in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=
            German_forget_file.on_recently_deleted ap (!(Private.recently_deleted_ref));
         Private.save_all();
       );;         
    
let forget_module ap=
   let _=Private.recompile() in
   let ((new_mdata,new_dirs,new_tgts),new_rdel)= 
    German_forget_module.on_targets
      (data(),directories(),(!Private.up_to_date_targets_ref)) ap in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=
          German_forget_module.on_recently_deleted 
            new_rdel
            (!(Private.recently_deleted_ref));
         Private.save_all();
       );;         
    
 let initialize ()=
   let s_ap=Directory_name.join German_constant.root  German_constant.location_for_targetfile in
   let ap=Absolute_path.of_string s_ap in
   let the_archive=Io.read_whole_file ap in
   let 
   (
    mdata,
    directories,
    targets,
    ofiles,
    odirectories,
    dfiles,
    chfiles,
    crfiles,
    pe_types
   )=German_save_all.read_all the_archive in
   (
	Private.data_ref:= mdata;
	Private.directories_ref:= directories;
	Private.up_to_date_targets_ref:= targets;
	Private.outside_files_ref:= ofiles;
	Private.outside_directories_ref:= odirectories;
	Private.recently_deleted_ref:= dfiles;
	Private.recently_changed_ref:= chfiles;
	Private.recently_created_ref:= crfiles;
	Private.printer_equipped_types_ref:= pe_types;
  );;

   
    
 let make_module_optional old_name=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_make_module_optional.on_targets (data(),(!Private.up_to_date_targets_ref)) 
     old_name in
     let (ndel,ncre)=German_created_or_deleted.update 
        (old_files,new_files)
        ((!Private.recently_deleted_ref,!Private.recently_created_ref)) in   
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
       );;          

let outside_files ()=(!Private.outside_files_ref);;
let outside_directories ()=(!Private.outside_directories_ref);;

let printer_equipped_types ()=(!Private.printer_equipped_types_ref);;

let recently_deleted ()=(!Private.recently_deleted_ref);;

let recompile=Private.recompile;;  

let refresh ()=
  let (new_mdata,new_tgts,new_ptypes)=
  Alaskan_create_target_system.from_main_directory 
       German_constant.root
       (Some(German_constant.main_toplevel_name))
   in 
  let new_dirs=German_directories.from_data new_mdata in
  (
        Private.data_ref:=new_mdata;
		Private.directories_ref:=new_dirs;
		Private.up_to_date_targets_ref:=new_tgts;
		Private.outside_files_ref:=[];
		Private.outside_directories_ref:=[];
		Private.recently_deleted_ref:=[];
		Private.printer_equipped_types_ref:=new_ptypes;
        Private.save_all();
   );;  

    
let register_mlx_file mlx=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
   Alaskan_register_mlx_file.on_targets 
	(German_constant.main_toplevel_name)
    (data(),directories(),(!Private.up_to_date_targets_ref)) 
        mlx in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;     
 
 let register_outside_file ap=
   let _=Private.recompile() in
   let (new_ofiles,new_odirs)= 
    German_register_outside_file.on_outside_directories 
     (!Private.outside_files_ref,!Private.outside_directories_ref) ap in
       (
         Private.outside_files_ref:=new_ofiles;
         Private.outside_directories_ref:=new_odirs;
         Private.save_all();
       );;        
    
 let relocate_module old_name new_subdir=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_relocate_module.on_targets (data(),(!Private.up_to_date_targets_ref)) 
         old_name new_subdir in
    let (ndel,ncre)=German_created_or_deleted.update 
        (old_files,new_files)
        ((!Private.recently_deleted_ref,!Private.recently_created_ref)) in
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
         
       );;       
    
 let rename_module old_name new_name=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_rename_module.on_targets (data(),(!Private.up_to_date_targets_ref)) old_name new_name in
    let (ndel,ncre)=German_created_or_deleted.update 
        (old_files,new_files)
        ((!Private.recently_deleted_ref,!Private.recently_created_ref)) in  
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
       );;   
   
let reposition_module hm (l_before,l_after)=
    let _=Private.recompile() in
    let new_mdata=
      Alaskan_reposition_module.rpm (data()) hm (l_before,l_after) in
       (
         Private.data_ref:=new_mdata;
         Private.save_all();
       );;      
  
 let start_debugging ()=
    let _=Private.recompile() in
    let (bowl,(new_mdata,new_tgts))=
      German_start_debugging.sd (data(),(!Private.up_to_date_targets_ref))  in
    if (not(bowl))
    then ()
    else  
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;   
    

    
 let save_all=Private.save_all;;   
    
 let unregister_mlx_file mlx=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
    German_unregister_mlx_file.on_targets (data(),(!Private.up_to_date_targets_ref)) mlx in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;  

let unregister_module mlx=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
    German_unregister_module.on_targets (data(),(!Private.up_to_date_targets_ref)) mlx in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;        
   
 let unregister_outside_file ap=
   let _=Private.recompile() in
   let (new_ofiles,new_odirs)= 
    German_unregister_outside_file.on_outside_directories 
     (!Private.outside_files_ref,!Private.outside_directories_ref) ap in
       (
         Private.outside_files_ref:=new_ofiles;
         Private.outside_directories_ref:=new_odirs;
         Private.save_all();
       );;          
   
let up_to_date_targets ()=(!Private.up_to_date_targets_ref);;   
   
let whole=Private.whole;;
 
 

(* 

#use"Makefile_makers/coma_state.ml";;

*)



let recompile x=
     let old_tgts=x.Coma_state_t.targets in
     let ((_,nms_to_be_updated),short_paths)=
        Modify_md_list.recompile_on_monitored_modules false x in
     if nms_to_be_updated=[] then (false,[]) else
     let new_dirs=Modify_md_list.compute_subdirectories_list x 
     and new_tgts1=Ocaml_target.still_up_to_date_targets nms_to_be_updated old_tgts in
     let checker=Ocaml_target.test_target_existence (Modify_md_list.root x) in
     let new_tgts=List.filter checker new_tgts1 in
     let (_,new_tgts2,rejected_ones2)=
       Alaskan_make_ocaml_target.feydeau
       (Modify_md_list.root x) x new_tgts  in
      let new_preqt=Image.image(
        fun (hm,_)->(hm,not(List.mem hm rejected_ones2))
      )  (x.Coma_state_t.printer_equipped_types) in   
     let _=(
        x.Coma_state_t.directories <- new_dirs;
        x.Coma_state_t.targets <- new_tgts2;
        x.Coma_state_t.printer_equipped_types <- new_preqt;
     )  in
    (true,short_paths);;       

let add_printer_equipped_type x hm=
  Modify_md_list.set_preq_types x ((Modify_md_list.preq_types x)@[hm]);;

let remove_printer_equipped_type x hm=
  Modify_md_list.set_preq_types x (List.filter (fun hm2->hm2<>hm) (Modify_md_list.preq_types x));;

let uple_form x=
  (x,
   x.Coma_state_t.directories,
   x.Coma_state_t.targets,
   x.Coma_state_t.printer_equipped_types
   );;

let remove_debuggables x=
  let new_tgts=List.filter Ocaml_target.is_not_a_debuggable
         (Modify_md_list.targets x)  in 
         Modify_md_list.set_targets x new_tgts;;
    
let backup x diff opt=
  Alaskan_backup_target_system.backup 
  (Modify_md_list.root x,Modify_md_list.backup_dir x) diff opt;;

let forget_file x ap=
    let (_,new_dirs,new_tgts)= 
     German_forget_file.on_targets (Modify_md_list.root x)
       (x,Modify_md_list.directories x,Modify_md_list.targets x) ap in  
        (
          Modify_md_list.set_directories x new_dirs;
          Modify_md_list.set_targets x new_tgts;
        );;         
   
let forget_module x hm=
    let ((_,new_dirs,new_tgts),short_paths)= 
      German_forget_module.on_targets (Modify_md_list.root x)
      (x,Modify_md_list.directories x,Modify_md_list.targets x) hm in
      let _=(
          Modify_md_list.set_directories x new_dirs;
          Modify_md_list.set_targets x new_tgts;
      ) in
      short_paths;;          

let initialize x=
        let s_ap=Root_directory.join (Modify_md_list.root x)  Coma_constant.name_for_targetfile in
        let ap=Absolute_path.of_string s_ap in
        let the_archive=Io.read_whole_file ap in
        let the_bulky_one=Modify_md_list.unarchive the_archive in
        Modify_md_list.copy_mutables_from x the_bulky_one;;      

let refresh x=
      let (new_mdata,new_tgts,new_ptypes)=
        Alaskan_create_target_system.from_main_directory 
             (Modify_md_list.root x)
             (Modify_md_list.backup_dir x)
         in 
        let new_dirs=Modify_md_list.compute_subdirectories_list new_mdata in
        let new_diff=German_delchacre_from_scratch.dfs (Modify_md_list.root x,Modify_md_list.backup_dir x) new_mdata in
        let _=
        (
          Modify_md_list.copy_mutables_from x new_mdata;
          Modify_md_list.set_directories x new_dirs;
          Modify_md_list.set_targets x new_tgts;
          Modify_md_list.set_preq_types x new_ptypes;
         ) in
         new_diff;; 


let register_mlx_file x mlx=
          let (new_mdata,new_dirs,new_tgts)= 
          Alaskan_register_mlx_file.on_targets 
           (x,Modify_md_list.directories x,Modify_md_list.targets x) mlx in
         let (_,new_tgts2,_)=Alaskan_make_ocaml_target.feydeau
               (Modify_md_list.root x) new_mdata new_tgts  in     
                
             (
              Modify_md_list.set_directories x new_dirs;
              Modify_md_list.set_targets x new_tgts2;   
              ) ;;             



let relocate_module x old_name new_subdir=
  let ((_,new_tgts),(old_files,new_files))=
    German_relocate_module.on_targets (Modify_md_list.root x)
       (x,Modify_md_list.targets x) 
       old_name new_subdir in
     (
      Modify_md_list.set_targets x new_tgts; 
     );;    

let rename_directory x (old_subdir,new_subdirname)=
      let _=Rename_endsubdirectory.in_unix_world 
       (Modify_md_list.root x) (old_subdir,new_subdirname) in
      let pair=(old_subdir,new_subdirname) in
      let _=Modify_md_list.rename_directory_on_data pair x
         
      and new_dirs=German_rename_directory.on_subdirectories pair 
        (Modify_md_list.directories x)
      and new_tgts=German_rename_directory.on_up_to_date_targets pair 
        (Modify_md_list.targets x)
      and new_peqt=German_rename_directory.on_printer_equipped_types pair 
        (Modify_md_list.preq_types x)
      in
         (
          
          Modify_md_list.set_directories x new_dirs;
          Modify_md_list.set_targets x new_tgts;
          Modify_md_list.set_preq_types x new_peqt;  
         );;   
      
let rename_module x old_name new_name=
      let ((_,new_tgts),(old_files,new_files))=
        German_rename_module.on_targets (Modify_md_list.root x)
          (x,Modify_md_list.targets x) old_name new_name in  
         (
          Modify_md_list.set_targets x new_tgts;
         );;    

let start_debugging x=
          let (bowl,(new_mdata,new_tgts,_))=
            German_start_debugging.sd (Modify_md_list.root x)
            (x,Modify_md_list.targets x)  in
          if bowl
          then (
            Modify_md_list.set_targets x new_tgts 
               )
          else ();;

          


let unregister_mlx_file x mlx=
        let (_,new_dirs,new_tgts)= 
          German_unregister_mlx_file.on_targets (Modify_md_list.root x)
          (x,Modify_md_list.targets x) mlx in
          (
              Modify_md_list.set_directories x new_dirs;
              Modify_md_list.set_targets x new_tgts;
          ) ;;  


let unregister_module x hm=
        let ((_,new_dirs,new_tgts),short_paths)= 
         German_unregister_module.on_targets (Modify_md_list.root x)
           (x,Modify_md_list.targets x) hm in
            (
              Modify_md_list.set_directories x new_dirs;
              Modify_md_list.set_targets x new_tgts;
            );;        


               